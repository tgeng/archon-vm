use std::collections::HashMap;
use cranelift::codegen::ir::{FuncRef, Inst};
use cranelift::frontend::Switch;
use cbpv_runtime::runtime_utils::runtime_alloc;
use cranelift::prelude::*;
use cranelift::prelude::types::{F32, F64, I32, I64};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module, ModuleResult};
use crate::signature::FunctionDefinition;
use crate::term::{CTerm, VTerm, VType, SpecializedType, PType, CType};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use enum_map::{Enum, EnumMap};
use VType::{Specialized, Uniform};
use SpecializedType::{Integer, PrimitivePtr, StructPtr};
use crate::main;
use crate::primitive_functions::PRIMITIVE_FUNCTIONS;

/// None means the function call is a tail call or returned so no value is returned.
type TypedValue = (Value, VType);
type TypedReturnValue = Option<TypedValue>;

trait HasType {
    fn get_type(&self) -> Type;
}

impl HasType for VType {
    fn get_type(&self) -> Type {
        match self {
            Uniform => I64,
            Specialized(t) => match t {
                Integer => I64,
                StructPtr => I64,
                PrimitivePtr => I64,
                SpecializedType::Primitive(t) => match t {
                    PType::I64 => I64,
                    PType::I32 => I32,
                    PType::F64 => F64,
                    PType::F32 => F32,
                }
            }
        }
    }
}

impl HasType for PType {
    fn get_type(&self) -> Type {
        match self {
            PType::I64 => I64,
            PType::I32 => I32,
            PType::F64 => F64,
            PType::F32 => F32,
        }
    }
}

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
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            BuiltinFunction::ForceThunk => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
            }
        }
        sig
    }

    fn declare<M: Module>(&self, m: &mut M) -> FuncId {
        let signature = &self.signature(m);
        m.declare_function(self.func_name(), Linkage::Import, signature).unwrap()
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

impl Compiler<JITModule> {
    pub fn finalize_and_get_main(&mut self) -> fn() -> usize {
        self.module.finalize_definitions().unwrap();
        let main_func_id = self.local_functions.get("__main__").unwrap();
        unsafe {
            let func_ptr = self.module.get_finalized_function(*main_func_id);
            std::mem::transmute::<_, fn() -> usize>(func_ptr)
        }
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

    pub fn compile(&mut self, defs: &[(String, FunctionDefinition)]) {
        for (name, _) in defs.iter() {
            let mut sig = self.module.make_signature();
            sig.params.push(AbiParam::new(I64));
            sig.returns.push(AbiParam::new(I64));
            self.local_functions.insert(name.clone(), self.module.declare_function(name, Linkage::Local, &sig).unwrap());
        }

        for (name, function_definition) in defs.iter() {
            self.compile_function(function_definition);
            let func_id = self.local_functions.get(name).unwrap();
            self.module.define_function(*func_id, &mut self.ctx).unwrap();
            self.module.clear_context(&mut self.ctx);
        }

        // TODO: define wrapper __main__ function that sets up the parameter stack and calls the
        //  user main function.
    }

    fn compile_function(&mut self, function_definition: &FunctionDefinition) {
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
        self.ctx.func.signature.params.push(AbiParam::new(I64));
        self.ctx.func.signature.returns.push(AbiParam::new(I64));

        let mut function_builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = function_builder.create_block();

        function_builder.append_block_params_for_function_params(entry_block);
        function_builder.switch_to_block(entry_block);
        function_builder.seal_block(entry_block);

        let base_address = function_builder.block_params(entry_block)[0];
        let mut translator = FunctionTranslator {
            module: &mut self.module,
            function_builder,
            data_description: &mut DataDescription::new(),
            builtin_functions: &self.builtin_functions,
            static_strings: &mut self.static_strings,
            local_functions: &self.local_functions,
            local_vars: &mut vec![None; function_definition.var_bound],
            base_address,
            tip_address: base_address,
            num_args: function_definition.args.len(),
        };
        // Here we transform the function body to non-specialized version, hence the argument types
        // are ignored.
        for (i, (v, _)) in function_definition.args.iter().enumerate() {
            // v is the variable index and i is the offset in the parameter list. The parameter
            // stack grows from higher address to lower address, so parameter list grows in the
            // reverse order and hence the offset is the index of the parameter in the parameter
            // list.
            let value = translator.function_builder.ins().load(I64, MemFlags::new(), translator.base_address, (i * 8) as i32);
            // TODO: add logic that compiles to a specialized version of this function.
            translator.local_vars[*v] = Some((value, Uniform));
        }
        // The return value will be returned so its type does not matter. Treating it as an integer
        // is sufficient.
        let return_value_or_param = translator.translate_c_term(&function_definition.body, true);
        match return_value_or_param {
            Some((value, _)) => {
                translator.function_builder.ins().return_(&[value]);
            }
            None => {
                // Nothing to do since tail call is already a terminating instruction.
            }
        }
        translator.function_builder.seal_all_blocks();
        translator.function_builder.finalize();
    }
}

struct FunctionTranslator<'a, M: Module> {
    module: &'a mut M,
    function_builder: FunctionBuilder<'a>,
    data_description: &'a mut DataDescription,
    builtin_functions: &'a EnumMap<BuiltinFunction, FuncId>,
    static_strings: &'a mut HashMap<String, DataId>,
    local_functions: &'a HashMap<String, FuncId>,
    local_vars: &'a mut [TypedReturnValue],
    base_address: Value,
    tip_address: Value,
    num_args: usize,
}

impl<'a, M: Module> FunctionTranslator<'a, M> {
    fn translate_c_term(&mut self, c_term: &CTerm, is_tail: bool) -> TypedReturnValue {
        match c_term {
            CTerm::Redex { box function, args } => {
                let arg_values = args.iter().map(|arg| {
                    let v = self.translate_v_term(arg);
                    self.to_uniform(v)
                }).collect::<Vec<_>>();
                self.push_args(arg_values);
                self.translate_c_term(function, is_tail)
            }
            CTerm::Return { value } => self.translate_v_term(value),
            CTerm::Force { thunk } => {
                let thunk_value = self.translate_v_term(thunk);
                // We must change the thunk value to uniform representation because the built-in
                // function expects a uniform representation in order to tell a thunk from a raw
                // function pointer.
                let thunk_value = self.to_uniform(thunk_value);
                let inst = self.call_builtin_func(BuiltinFunction::ForceThunk, &[thunk_value, self.tip_address]);
                let func_pointer = self.function_builder.inst_results(inst)[0];
                let signature = &self.function_builder.func.signature;
                let sig_ref = self.function_builder.import_signature(signature.clone());
                if is_tail {
                    let dest_value = self.copy_tail_call_args();
                    self.function_builder.ins().return_call_indirect(sig_ref, func_pointer, &[dest_value]);
                    None
                } else {
                    let inst = self.function_builder.ins().call_indirect(sig_ref, func_pointer, &[self.tip_address]);
                    self.extract_return_value(inst)
                }
            }
            CTerm::Let { box t, bound_index, box body } => {
                let t_value = self.translate_c_term(t, false);
                self.local_vars[*bound_index] = t_value;
                self.translate_c_term(body, is_tail)
            }
            CTerm::Def { name } => {
                let func_ref = self.get_local_function(name);
                if is_tail {
                    let dest_value = self.copy_tail_call_args();
                    self.function_builder.ins().return_call(func_ref, &[dest_value]);
                    None
                } else {
                    let inst = self.function_builder.ins().call(func_ref, &[self.tip_address]);
                    self.extract_return_value(inst)
                }
            }
            CTerm::CaseInt { t, result_type, branches, default_branch } => {
                let t_value = self.translate_v_term(t);
                let t_value = self.to_special(t_value, Integer);
                let current_block = self.function_builder.current_block().unwrap();

                // Create next block
                let next_block = self.function_builder.create_block();
                let result_v_type = match result_type {
                    CType::Default => &Uniform,
                    CType::SpecializedF(vty) => vty,
                };
                let result_value_type = result_v_type.get_type();
                self.function_builder.append_block_param(next_block, result_value_type);

                // Create branch blocks
                let mut branch_blocks = HashMap::new();
                for (value, branch) in branches.iter() {
                    let branch_block = self.create_branch_block(is_tail, next_block, result_v_type, Some(branch));
                    branch_blocks.insert(*value, branch_block);
                }

                let default_block = self.create_branch_block(is_tail, next_block, result_v_type, match default_branch {
                    None => None,
                    Some(box branch) => Some(branch),
                });

                // Create table jump
                self.function_builder.switch_to_block(current_block);
                let mut switch = Switch::new();
                for (value, branch_block) in branch_blocks.iter() {
                    switch.set_entry(*value as u128, *branch_block);
                }
                switch.emit(&mut self.function_builder, t_value, default_block);

                // Switch to next block for future code generation
                self.function_builder.seal_all_blocks();
                self.function_builder.switch_to_block(next_block);
                Some((self.function_builder.block_params(next_block)[0], *result_v_type))
            }
            CTerm::MemGet { base, offset } => {
                let base_value = self.translate_v_term(base);
                let offset_value = self.translate_v_term(offset);
                let base_value = self.to_special(base_value, StructPtr);
                let offset_value = self.to_special(offset_value, Integer);
                let base_address = self.function_builder.ins().load(I64, MemFlags::new(), base_value, 0);
                let load_address = self.function_builder.ins().iadd(base_address, offset_value);
                let value = self.function_builder.ins().load(I64, MemFlags::new(), load_address, 0);
                Some((value, Uniform))
            }
            CTerm::MemSet { base, offset, value } => {
                let base_value = self.translate_v_term(base);
                let offset_value = self.translate_v_term(offset);
                let value_value = self.translate_v_term(value);
                let base_value = self.to_special(base_value, StructPtr);
                let offset_value = self.to_special(offset_value, Integer);
                let value_value = self.to_uniform(value_value);
                let base_address = self.function_builder.ins().load(I64, MemFlags::new(), base_value, 0);
                let store_address = self.function_builder.ins().iadd(base_address, offset_value);
                self.function_builder.ins().store(MemFlags::new(), value_value, store_address, 0);
                // Return the base address so that the caller can continue to use it.
                Some((base_value, Specialized(StructPtr)))
            }
            CTerm::PrimitiveCall { name, args } => {
                let args = args.iter().map(|arg| { self.translate_v_term(arg) }).collect::<Vec<_>>();
                let primitive_function = *PRIMITIVE_FUNCTIONS.get(name).unwrap();
                let arg_values: Vec<Value> = primitive_function.arg_types.iter().zip(args).map(|(ty, arg)| self.adapt_type(arg, ty)).collect();
                let return_value = (primitive_function.code_gen)(&mut self.function_builder, &arg_values);
                Some((return_value, primitive_function.return_type))
            }
            CTerm::SpecializedFunctionCall { name, args } => todo!(),
        }
    }

    fn create_branch_block(&mut self, is_tail: bool, next_block: Block, result_v_type: &VType, branch: Option<&CTerm>) -> Block {
        let branch_block = self.function_builder.create_block();
        self.function_builder.switch_to_block(branch_block);
        let typed_return_value = match branch {
            None => {
                self.function_builder.ins().trap(TrapCode::UnreachableCodeReached);
                None
            }
            Some(branch) => self.translate_c_term(branch, is_tail),
        };
        match typed_return_value {
            None => {
                // Nothing to do since tail call is already a terminating instruction.
            }
            Some(..) => {
                let value = self.adapt_type(typed_return_value, result_v_type);
                self.function_builder.ins().jump(next_block, &[value]);
            }
        }
        branch_block
    }

    fn extract_return_value(&mut self, inst: Inst) -> TypedReturnValue {
        let return_address = self.function_builder.inst_results(inst)[0];
        let return_value = self.function_builder.ins().load(I64, MemFlags::new(), return_address, 0);
        self.tip_address = self.function_builder.ins().iadd_imm(self.tip_address, 8);
        Some((return_value, Uniform))
    }

    fn copy_tail_call_args(&mut self) -> Value {
        let dest_value = self.function_builder.ins().iadd_imm(self.tip_address, (self.num_args * 8) as i64);
        let num_words_to_copy = self.function_builder.ins().isub(self.base_address, self.tip_address);
        let num_bytes_to_copy = self.function_builder.ins().imul_imm(num_words_to_copy, 8);
        self.function_builder.call_memmove(self.module.target_config(), dest_value, self.tip_address, num_bytes_to_copy);
        dest_value
    }

    fn translate_v_term(&mut self, v_term: &VTerm) -> TypedReturnValue {
        match v_term {
            VTerm::Var { index } => self.local_vars[*index],
            VTerm::Thunk { box t } => {
                let CTerm::Redex { function: box CTerm::Def { name }, args } = t else {
                    unreachable!("thunk lifting should have guaranteed this")
                };
                let func_ref = self.get_local_function(name);
                let func_pointer = self.function_builder.ins().func_addr(I64, func_ref);
                // Plus 1 to indicate this pointer points to a bare function (rather than a closure).
                let func_pointer_plus_one = self.function_builder.ins().iadd_imm(func_pointer, 1);
                let arg_size = self.function_builder.ins().iconst(I64, args.len() as i64);
                let mut thunk_components = vec![func_pointer_plus_one, arg_size];
                for arg in args {
                    let value_and_type = self.translate_v_term(arg);
                    let uniform_value = self.to_uniform(value_and_type);
                    thunk_components.push(uniform_value);
                }
                Some((self.create_struct(thunk_components), Specialized(StructPtr)))
            }
            VTerm::Int { value } => Some((self.function_builder.ins().iconst(I64, *value), Specialized(Integer))),
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
                Some((self.function_builder.ins().symbol_value(I64, global_value), Specialized(PrimitivePtr)))
            }
            VTerm::Struct { values } => {
                let translated = values.iter().map(|v| {
                    let v = self.translate_v_term(v);
                    self.to_uniform(v)
                }).collect::<Vec<_>>();
                Some((self.create_struct(translated), Specialized(StructPtr)))
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

    fn create_struct(&mut self, values: Vec<Value>) -> Value {
        let array_size = values.len() * 8;
        let array_size_arg = self.function_builder.ins().iconst(I64, array_size as i64);
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

    fn to_uniform(&mut self, value_and_type: TypedReturnValue) -> Value {
        let (value, value_type) = Self::extract_value_and_type(value_and_type);
        match value_type {
            Uniform => value,
            Specialized(s) => match s {
                Integer => self.function_builder.ins().ishl_imm(value, 1),
                StructPtr => self.function_builder.ins().iadd_imm(value, 1),
                PrimitivePtr => self.function_builder.ins().iadd_imm(value, 0b011),
                SpecializedType::Primitive(p) => match p {
                    PType::I64 | PType::F64 => {
                        let alloc_size = self.function_builder.ins().iconst(I64, 8);
                        let inst = self.call_builtin_func(BuiltinFunction::Alloc, &[alloc_size]);
                        let ptr = self.function_builder.inst_results(inst)[0];
                        self.function_builder.ins().store(MemFlags::new(), value, ptr, 0);
                        // Add 0b011 to the end to signify this is a primitive pointer
                        self.function_builder.ins().iadd_imm(ptr, 0b011)
                    }
                    PType::I32 => {
                        let extended = self.function_builder.ins().sextend(I64, value);
                        let shifted = self.function_builder.ins().ishl_imm(extended, 32);
                        self.function_builder.ins().iadd_imm(shifted, 0b100)
                    }
                    PType::F32 => {
                        let casted = self.function_builder.ins().bitcast(I32, MemFlags::new(), value);
                        let extended = self.function_builder.ins().sextend(I64, casted);
                        let shifted = self.function_builder.ins().ishl_imm(extended, 32);
                        self.function_builder.ins().iadd_imm(shifted, 0b100)
                    }
                }
            }
        }
    }

    fn to_special(&mut self, value_and_type: TypedReturnValue, specialized_type: SpecializedType) -> Value {
        let (value, value_type) = Self::extract_value_and_type(value_and_type);
        match value_type {
            Uniform => match specialized_type {
                Integer => self.function_builder.ins().sshr_imm(value, 1),
                StructPtr => self.function_builder.ins().iadd_imm(value, -1),
                PrimitivePtr => self.function_builder.ins().iadd_imm(value, -0b011),
                SpecializedType::Primitive(p) => match p {
                    PType::I64 | PType::F64 => {
                        let ptr = self.function_builder.ins().iadd_imm(value, -0b011);
                        self.function_builder.ins().load(p.get_type(), MemFlags::new(), ptr, 0)
                    }
                    PType::I32 => {
                        let shifted = self.function_builder.ins().sshr_imm(value, 32);
                        self.function_builder.ins().ireduce(I32, shifted)
                    }
                    PType::F32 => {
                        let shifted = self.function_builder.ins().sshr_imm(value, 32);
                        let truncated = self.function_builder.ins().ireduce(I32, shifted);
                        self.function_builder.ins().bitcast(F32, MemFlags::new(), truncated)
                    }
                }
            }
            Specialized(s) => if s == specialized_type {
                value
            } else {
                unreachable!("type conversion between two specialized types is not supported and this must be a type error in the input program")
            }
        }
    }

    fn adapt_type(&mut self, value_and_type: TypedReturnValue, target_type: &VType) -> Value {
        let (value, value_type) = Self::extract_value_and_type(value_and_type);
        if value_type == *target_type {
            return value;
        }
        match (value_type, target_type) {
            (Uniform, Specialized(s)) => self.to_special(value_and_type, s.clone()),
            (Specialized(_), Uniform) => self.to_uniform(value_and_type),
            _ => unreachable!("type conversion between two specialized types is not supported and this must be a type error in the input program"),
        }
    }

    fn extract_value_and_type(value_and_type: TypedReturnValue) -> (Value, VType) {
        value_and_type.expect("non-local return value cannot be converted and this must be a bug in the compilation logic or input is not well-typed")
    }
}
