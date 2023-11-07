use std::collections::HashMap;
use cranelift::codegen::ir::{FuncRef, Inst};
use cbpv_runtime::runtime_utils::runtime_alloc;
use cranelift::prelude::*;
use cranelift::prelude::types::I64;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use crate::signature::FunctionDefinition;
use crate::term::{CTerm, VTerm};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use enum_map::{Enum, EnumMap};

static INT: Type = I64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumIter, Enum)]
enum BuiltinFunction {
    Alloc,
    ForceThunk,
}

impl BuiltinFunction {
    fn func_name(&self) -> &'static str {
        match self {
            BuiltinFunction::Alloc => "__runtime_alloc__",
            BuiltinFunction::ForceThunk => "__runtime_force_thunk__"
        }
    }

    fn declare_symbol(&self, builder: &mut JITBuilder) {
        builder.symbol(self.func_name(), runtime_alloc as *const u8);
    }

    fn signature<M: Module>(&self, m: &mut M) -> Signature {
        let mut sig = m.make_signature();
        match self {
            BuiltinFunction::Alloc => {
                sig.params.push(AbiParam::new(INT));
                sig.returns.push(AbiParam::new(INT));
            }
            BuiltinFunction::ForceThunk => {
                sig.params.push(AbiParam::new(INT));
                sig.params.push(AbiParam::new(INT));
            }
        }
        sig
    }

    fn declare<M: Module>(&self, m: &mut M) -> FuncId {
        m.declare_function(self.func_name(), Linkage::Import, &self.signature(m)).unwrap()
    }
}

/// The basic JIT class.
pub struct Compiler<M: Module> {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: M,

    builtin_functions: EnumMap<BuiltinFunction, FuncId>,
    static_strings: HashMap<String, DataId>,
    local_functions: HashMap<String, FuncId>,
}

impl Default for Compiler<JITModule> {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        for f in BuiltinFunction::iter() {
            f.declare_symbol(&mut builder);
        }

        let mut module = JITModule::new(builder);

        let builtin_functions = EnumMap::from_fn(|e: BuiltinFunction| e.declare(&mut module));

        Self::new(module, builtin_functions)
    }
}

impl<M: Module> Compiler<M> {
    fn new(module: M, builtin_functions: EnumMap<BuiltinFunction, FuncId>) -> Self {
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
            builtin_functions,
            static_strings: HashMap::new(),
            local_functions: HashMap::new(),
        }
    }

    pub fn process(&mut self, defs: &HashMap<String, FunctionDefinition>) {
        for (name, _) in defs.iter() {
            let mut sig = self.module.make_signature();
            sig.params.push(AbiParam::new(INT));
            sig.returns.push(AbiParam::new(INT));
            self.local_functions.insert(name.clone(), self.module.declare_function(name, Linkage::Local, &sig).unwrap());
        }

        for (name, function_definition) in defs.iter() {
            self.process_function(name, function_definition);
        }
    }

    fn process_function(&mut self, name: &str, function_definition: &FunctionDefinition) {
        // All functions have the same signature `i64 -> i64`, where the single argument is the
        // base address of the parameter stack and the single return value is the address of the
        // return address. Actual parameters can be obtained by offsetting this base address.
        //
        // Callee should compute the return address by adding the total size of the parameters to
        // the callee base address minus one word. See diagram below for the following call
        //
        // fn caller(x, y) {
        //   ...
        //   let i = callee(a, b, c);
        //   ...
        // }
        //
        //  |---------|
        //  |    y    |
        //  |---------| <- caller return address
        //  |    x    |
        //  |---------| <- caller base address
        //  |  c / i  |
        //  |---------| <- callee return address: the address where callee put the return value
        //  |    b    |
        //  |---------|
        //  |    a    |
        //  |---------| <- callee base address: the address from which callee finds arguments
        //
        // Caller tracks a current tip address, which initially points to the caller base address.
        // When evaluating a redex, this pointer is bumped and a new parameter is stored at this
        // address. After all parameters are pushed, this pointer becomes the callee base address.
        // Whenever a function call completes, this tip is set to the callee return address plus
        // size of one word, so that the next call can happen normally.

        self.ctx.clear();
        self.ctx.func.signature.params.push(AbiParam::new(INT));
        self.ctx.func.signature.returns.push(AbiParam::new(INT));

        let mut function_builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = function_builder.create_block();

        function_builder.append_block_params_for_function_params(entry_block);
        function_builder.switch_to_block(entry_block);
        function_builder.seal_block(entry_block);

        let mut vars = vec![None; function_definition.var_bound];
        for (i, v) in function_definition.args.iter().enumerate() {
            // v is the variable index and i is the offset in the parameter list. The parameter
            // stack grows from higher address to lower address, so parameter list grows in the
            // reverse order and hence the offset is the index of the parameter in the parameter
            // list.
            vars[*v] = Some(ValueOrParam::Param(i));
        }
        let base_address = function_builder.block_params(entry_block)[0];
        let mut translator = FunctionTranslator {
            module: &mut self.module,
            function_builder: &mut function_builder,
            data_description: &mut DataDescription::new(),
            builtin_functions: &self.builtin_functions,
            static_strings: &mut self.static_strings,
            local_functions: &self.local_functions,
            local_vars: &mut vars,
            base_address,
            tip_address: base_address,
            num_args: function_definition.args.len(),
        };
        // The return value will be returned so its type does not matter. Treating it as an integer
        // is sufficient.
        let return_value_or_param = translator.translate_c_term(&function_definition.body, true, INT);
        match return_value_or_param {
            ValueOrParam::Value(..) | ValueOrParam::Param(..) => {
                let return_value = translator.translate_value_or_param(return_value_or_param, INT);
                translator.function_builder.ins().return_(&[return_value]);
            }
            ValueOrParam::TailCall => {
                // Nothing to do since tail call is already a terminating instruction.
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum ValueOrParam {
    Value(Value),
    Param(usize),
    TailCall,
}

struct FunctionTranslator<'a, M: Module> {
    module: &'a mut M,
    function_builder: &'a mut FunctionBuilder<'a>,
    data_description: &'a mut DataDescription,
    builtin_functions: &'a EnumMap<BuiltinFunction, FuncId>,
    static_strings: &'a mut HashMap<String, DataId>,
    local_functions: &'a HashMap<String, FuncId>,
    local_vars: &'a mut [Option<ValueOrParam>],
    base_address: Value,
    tip_address: Value,
    num_args: usize,
}

impl<'a, M: Module> FunctionTranslator<'a, M> {
    fn translate_c_term(&mut self, c_term: &CTerm, is_tail: bool, expected_type: Type) -> ValueOrParam {
        match c_term {
            CTerm::Redex { box function, args } => {
                let arg_values = args.iter().map(|arg| self.translate_v_term_to_value(arg, INT)).collect::<Vec<_>>();
                self.push_args(arg_values);
                self.translate_c_term(function, is_tail, expected_type)
            }
            CTerm::Return { value } => self.translate_v_term(value),
            CTerm::Force { thunk } => {
                let thunk_value = self.translate_v_term_to_value(thunk, INT);
                let inst = self.call_builtin_func(BuiltinFunction::ForceThunk, &[thunk_value, self.tip_address]);
                let func_pointer = self.function_builder.inst_results(inst)[0];
                let signature = &self.function_builder.func.signature;
                let sig_ref = self.function_builder.import_signature(signature.clone());
                if is_tail {
                    let dest_value = self.copy_tail_call_args();
                    self.function_builder.ins().return_call_indirect(sig_ref, func_pointer, &[dest_value]);
                    ValueOrParam::TailCall
                } else {
                    let inst = self.function_builder.ins().call_indirect(sig_ref, func_pointer, &[self.tip_address]);
                    ValueOrParam::Value(self.extract_return_value(expected_type, inst))
                }
            }
            CTerm::Let { .. } => todo!(),
            CTerm::Def { name } => {
                let func_ref = self.get_local_function(name);
                if is_tail {
                    let dest_value = self.copy_tail_call_args();
                    self.function_builder.ins().return_call(func_ref, &[dest_value]);
                    ValueOrParam::TailCall
                } else {
                    let inst = self.function_builder.ins().call(func_ref, &[self.tip_address]);
                    ValueOrParam::Value(self.extract_return_value(expected_type, inst))
                }
            }
            CTerm::CaseInt { .. } => todo!(),
            CTerm::MemGet { .. } => todo!(),
            CTerm::MemSet { .. } => todo!(),
            CTerm::CaseStr { .. } => todo!(),
            CTerm::Primitive { .. } => todo!(),
        }
    }

    fn extract_return_value(&mut self, expected_type: Type, inst: Inst) -> Value {
        let return_address = self.function_builder.inst_results(inst)[0];
        let return_value = self.function_builder.ins().load(expected_type, MemFlags::new(), return_address, 0);
        self.tip_address = self.function_builder.ins().iadd_imm(self.tip_address, 8);
        return_value
    }

    fn copy_tail_call_args(&mut self) -> Value {
        let dest_value = self.function_builder.ins().iadd_imm(self.tip_address, (self.num_args * 8) as i64);
        let num_words_to_copy = self.function_builder.ins().isub(self.base_address, self.tip_address);
        let num_bytes_to_copy = self.function_builder.ins().imul_imm(num_words_to_copy, 8);
        self.function_builder.call_memmove(self.module.target_config(), dest_value, self.tip_address, num_bytes_to_copy);
        dest_value
    }

    fn translate_v_term(&mut self, v_term: &VTerm) -> ValueOrParam {
        match v_term {
            VTerm::Var { index } => self.local_vars[*index].unwrap(),
            VTerm::Thunk { box t } => {
                let CTerm::Redex { function: box CTerm::Def { name }, args } = t else {
                    unreachable!("thunk lifting should have guaranteed this")
                };
                let func_ref = self.get_local_function(name);
                let func_pointer = self.function_builder.ins().func_addr(INT, func_ref);
                // Plus 1 to indicate this pointer points to a bare function (rather than a closure).
                let func_pointer_plus_one = self.function_builder.ins().iadd_imm(func_pointer, 1);
                let arg_size = self.function_builder.ins().iconst(INT, args.len() as i64);
                let mut thunk_components = vec![func_pointer_plus_one, arg_size];
                for arg in args {
                    thunk_components.push(self.translate_v_term_to_value(arg, INT));
                }
                ValueOrParam::Value(self.create_array(thunk_components))
            }
            VTerm::Int { value } => ValueOrParam::Value(self.function_builder.ins().iconst(INT, *value)),
            VTerm::Str { value } => {
                // Insert into the global data section if not already there.
                let data_id = self.static_strings.entry(value.clone()).or_insert_with(|| {
                    self.data_description.define(value.clone().into_bytes().into_boxed_slice());
                    let data_id = self.module.declare_data(value, Linkage::Local, false, false).unwrap();
                    self.module.define_data(data_id, self.data_description).unwrap();
                    self.data_description.clear();
                    data_id
                });
                let global_value = self.module.declare_data_in_func(*data_id, self.function_builder.func);
                ValueOrParam::Value(self.function_builder.ins().symbol_value(INT, global_value))
            }
            VTerm::Array { values } => {
                let translated = values.iter().map(|v| self.translate_v_term_to_value(v, INT)).collect::<Vec<_>>();
                ValueOrParam::Value(self.create_array(translated))
            }
        }
    }

    fn push_args(&mut self, args: Vec<Value>) {
        for arg in args.into_iter().rev() {
            self.tip_address = self.function_builder.ins().iadd_imm(self.tip_address, -8);
            self.function_builder.ins().store(MemFlags::new().with_aligned(), arg, self.tip_address, 0);
        }
    }

    fn get_local_function(&mut self, name: &String) -> FuncRef {
        let func_id = self.local_functions.get(name).unwrap();
        self.module.declare_func_in_func(*func_id, self.function_builder.func)
    }

    fn create_array(&mut self, values: Vec<Value>) -> Value {
        let array_size = values.len() * 8;
        let array_size_arg = self.function_builder.ins().iconst(INT, array_size as i64);
        let runtime_alloc_call = self.call_builtin_func(BuiltinFunction::Alloc, &[array_size_arg]);
        let array_address = self.function_builder.inst_results(runtime_alloc_call)[0];
        for (offset, value) in values.into_iter().enumerate() {
            self.function_builder.ins().store(
                MemFlags::new().with_aligned(),
                value,
                array_address,
                (offset * 8) as i32);
        }
        array_address
    }

    fn call_builtin_func(&mut self, builtin_function: BuiltinFunction, args: &[Value]) -> Inst {
        let func_ref = self.module.declare_func_in_func(self.builtin_functions[builtin_function], self.function_builder.func);
        self.function_builder.ins().call(func_ref, args)
    }

    fn translate_v_term_to_value(&mut self, v_term: &VTerm, expected_type: Type) -> Value {
        let value_or_param = self.translate_v_term(&v_term);
        self.translate_value_or_param(value_or_param, expected_type)
    }

    fn translate_value_or_param(&mut self, value_or_param: ValueOrParam, expected_type: Type) -> Value {
        match value_or_param {
            ValueOrParam::Value(v) => v,
            ValueOrParam::Param(param_index) => {
                let base_address = self.base_address;
                let offset = (param_index + 1) * 8;
                let address = self.function_builder.ins().iadd_imm(base_address, offset as i64);
                let value = self.function_builder.ins().load(expected_type, MemFlags::new(), address, 0);
                value
            }
            ValueOrParam::TailCall => unreachable!(),
        }
    }
}
