use std::collections::HashMap;
use cranelift::codegen::isa::CallConv;
use cranelift::prelude::*;
use cranelift::prelude::types::{I64};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use cranelift_object::ObjectModule;
use crate::ast::signature::FunctionDefinition;
use crate::ast::term::{VType, CType};
use strum::IntoEnumIterator;
use enum_map::{EnumMap};
use VType::{Uniform};
use crate::cranelift::common::{BuiltinFunction, HasType};
use crate::cranelift::function_translator::FunctionTranslator;

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
    uniform_func_signature: Signature,
}

const MAIN_WRAPPER_NAME: &str = "__main__";

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
        let main_func_id = self.local_functions.get(MAIN_WRAPPER_NAME).unwrap();
        unsafe {
            let func_ptr = self.module.get_finalized_function(*main_func_id);
            std::mem::transmute::<_, fn() -> usize>(func_ptr)
        }
    }
}

impl Default for Compiler<ObjectModule> {
    fn default() -> Self {
        todo!()
    }
}

impl Compiler<ObjectModule> {
    // TODO: implement this for object file emission.
}

impl<M: Module> Compiler<M> {
    fn new(module: M, builtin_functions: EnumMap<BuiltinFunction, FuncId>) -> Self {
        let mut uniform_func_signature = module.make_signature();
        uniform_func_signature.params.push(AbiParam::new(I64));
        uniform_func_signature.returns.push(AbiParam::new(I64));
        uniform_func_signature.call_conv = CallConv::Tail;
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
            builtin_functions,
            static_strings: HashMap::new(),
            local_functions: HashMap::new(),
            uniform_func_signature,
        }
    }

    pub fn compile(&mut self, defs: &[(String, FunctionDefinition)], clir: &mut Option<&mut Vec<(String, String)>>) {
        let mut specialzied_function_signatures = HashMap::new();
        let mut local_function_arg_types = HashMap::new();
        for (name, function_definition) in defs.iter() {
            local_function_arg_types.insert(
                name.clone(),
                (function_definition.args.iter().map(|(_, v_type)| *v_type).collect::<Vec<_>>(), function_definition.c_type),
            );
            self.local_functions.insert(name.clone(), self.module.declare_function(name, Linkage::Local, &self.uniform_func_signature).unwrap());
            if let CType::SpecializedF(v_type) = function_definition.c_type {
                let mut sig = self.module.make_signature();
                sig.call_conv = CallConv::Tail;
                // The first argument is the base address of the parameter stack, which is useful
                // for calling non-specialized functions.
                sig.params.push(AbiParam::new(I64));
                for (_, v_type) in function_definition.args.iter() {
                    sig.params.push(AbiParam::new(v_type.get_type()));
                }
                sig.returns.push(AbiParam::new(v_type.get_type()));
                let specialized_name = format!("{}__specialized", name);
                self.local_functions.insert(specialized_name.clone(), self.module.declare_function(&specialized_name, Linkage::Local, &sig).unwrap());
                specialzied_function_signatures.insert(name, sig);
            }
        }

        for (name, function_definition) in defs.iter() {
            self.compile_function(function_definition, &local_function_arg_types);
            let func_id = self.local_functions.get(name).unwrap();
            self.define_function(name, *func_id, clir);
            self.module.clear_context(&mut self.ctx);
            if function_definition.is_specializable() {
                let sig = specialzied_function_signatures.get(name).unwrap();
                let specialized_name = format!("{}__specialized", name);
                self.compile_specialized_function(sig, function_definition, &local_function_arg_types);
                let func_id = self.local_functions.get(&specialized_name).unwrap();
                self.define_function(&specialized_name, *func_id, clir);
                self.module.clear_context(&mut self.ctx);
            }
        }
    }

    /// Creates a main wrapper function (named `__main__`) that calls the `__runtime_alloc_stack__`,
    /// which sets up the parameter stack and invokes the user-defined `main` function.
    pub fn generate_main_wrapper(&mut self, clir: &mut Option<&mut Vec<(String, String)>>) {
        let main_wrapper_id = self.module.declare_function(MAIN_WRAPPER_NAME, Linkage::Local, &self.uniform_func_signature).unwrap();
        self.local_functions.insert(MAIN_WRAPPER_NAME.to_string(), main_wrapper_id);
        self.ctx.clear();
        self.ctx.func.signature.returns.push(AbiParam::new(I64));

        let mut function_builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = function_builder.create_block();

        function_builder.append_block_params_for_function_params(entry_block);
        function_builder.switch_to_block(entry_block);
        function_builder.seal_block(entry_block);

        let alloc_stack_id = self.builtin_functions[BuiltinFunction::AllocStack];
        let alloc_stack_func_ref = self.module.declare_func_in_func(alloc_stack_id, function_builder.func);
        let inst = function_builder.ins().call(alloc_stack_func_ref, &[]);
        let stack_base = function_builder.inst_results(inst)[0];

        let main_id = self.local_functions.get("main__specialized").unwrap();
        let main_func_ref = self.module.declare_func_in_func(*main_id, function_builder.func);
        let inst = function_builder.ins().call(main_func_ref, &[stack_base]);
        let return_value = function_builder.inst_results(inst)[0];
        function_builder.ins().return_(&[return_value]);

        function_builder.finalize();

        self.define_function("__main__", main_wrapper_id, clir);
        self.module.clear_context(&mut self.ctx);
    }

    pub fn define_function(&mut self, name: &str, func_id: FuncId, clir: &mut Option<&mut Vec<(String, String)>>) {
        if let Some(clir) = clir {
            clir.push((name.to_owned(), format!("{}", self.ctx.func.display())));
        }
        self.module.define_function(func_id, &mut self.ctx).unwrap();
    }

    fn compile_function(&mut self, function_definition: &FunctionDefinition, local_function_arg_types: &HashMap<String, (Vec<VType>, CType)>) {
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
        self.ctx.func.signature = self.uniform_func_signature.clone();

        let mut function_builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = function_builder.create_block();

        // Allocate slot for storing the tip address so that a pointer to the tip address can be
        // passed to built-in force call helper function in order to have the tip address updated.
        let tip_address_slot = function_builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8));
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
            uniform_func_signature: &self.uniform_func_signature,
            tip_address_slot,
            local_function_arg_types,
            is_specialized: false,
        };
        // Here we transform the function body to non-specialized version, hence the argument types
        // are ignored.
        for (i, (v, _)) in function_definition.args.iter().enumerate() {
            // v is the variable index and i is the offset in the parameter list. The parameter
            // stack grows from higher address to lower address, so parameter list grows in the
            // reverse order and hence the offset is the index of the parameter in the parameter
            // list.
            let value = translator.function_builder.ins().load(I64, MemFlags::new(), translator.base_address, (i * 8) as i32);
            translator.local_vars[*v] = Some((value, Uniform));
        }
        // The return value will be returned so its type does not matter. Treating it as an integer
        // is sufficient.
        let return_value_or_param = translator.translate_c_term(&function_definition.body, true);
        match return_value_or_param {
            Some(_) => {
                let value = translator.convert_to_uniform(return_value_or_param);
                let return_address_offset = ((function_definition.args.len() as i64 - 1) * 8);
                let return_address = translator.function_builder.ins().iadd_imm(translator.base_address, return_address_offset);
                translator.function_builder.ins().store(MemFlags::new(), value, return_address, 0);
                translator.function_builder.ins().return_(&[return_address]);
            }
            None => {
                // Nothing to do since tail call is already a terminating instruction.
            }
        }
        translator.function_builder.seal_all_blocks();
        translator.function_builder.finalize();
    }

    fn compile_specialized_function(&mut self, sig: &Signature, function_definition: &FunctionDefinition, local_function_arg_types: &HashMap<String, (Vec<VType>, CType)>) {
        // Parameters of specialized functions are just passed normally.

        self.ctx.clear();
        self.ctx.func.signature = sig.clone();

        let mut function_builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = function_builder.create_block();

        // Allocate slot for storing the tip address so that a pointer to the tip address can be
        // passed to built-in force call helper function in order to have the tip address updated.
        let tip_address_slot = function_builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8));
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
            uniform_func_signature: &self.uniform_func_signature,
            tip_address_slot,
            local_function_arg_types,
            is_specialized: true,
        };
        // Here we transform the function body to non-specialized version, hence the argument types
        // are ignored.
        for (i, (v, v_type)) in function_definition.args.iter().enumerate() {
            // v is the variable index and i is the offset in the parameter list. The parameter
            // stack grows from higher address to lower address, so parameter list grows in the
            // reverse order and hence the offset is the index of the parameter in the parameter
            // list.
            // In addition, i + 1 is the parameter index in the entry block
            translator.local_vars[*v] = Some((translator.function_builder.block_params(entry_block)[i + 1], *v_type));
        }
        // The return value will be returned so its type does not matter. Treating it as an integer
        // is sufficient.
        let return_value_or_param = translator.translate_c_term(&function_definition.body, true);
        match return_value_or_param {
            Some(_) => {
                let CType::SpecializedF(v_type) = function_definition.c_type else { unreachable!() };
                let value = translator.adapt_type(return_value_or_param, &v_type);
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
