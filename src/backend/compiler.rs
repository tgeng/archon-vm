use std::collections::HashMap;
use cranelift::codegen::isa::CallConv;
use cranelift::prelude::*;
use cranelift::prelude::types::{I64};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataId, FuncId, Linkage, Module};
use cranelift_object::ObjectModule;
use crate::ast::signature::FunctionDefinition;
use crate::ast::term::{CType};
use strum::IntoEnumIterator;
use enum_map::{EnumMap};
use crate::backend::common::{BuiltinFunction, FunctionFlavor, HasType};
use crate::backend::simple_function_translator::SimpleFunctionTranslator;

/// The basic JIT class.
pub struct Compiler<M: Module> {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    pub builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    pub ctx: codegen::Context,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    pub module: M,

    pub builtin_functions: EnumMap<BuiltinFunction, FuncId>,
    pub static_strings: HashMap<String, DataId>,
    pub local_functions: HashMap<String, FuncId>,
    pub uniform_func_signature: Signature,
    pub uniform_cps_func_signature: Signature,
    pub uniform_cps_impl_func_signature: Signature,
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

        let mut uniform_cps_func_signature = module.make_signature();
        uniform_cps_func_signature.params.push(AbiParam::new(I64)); // base address
        uniform_cps_func_signature.params.push(AbiParam::new(I64)); // the next continuation object
        uniform_cps_func_signature.returns.push(AbiParam::new(I64));
        uniform_cps_func_signature.call_conv = CallConv::Tail;

        let mut uniform_cps_impl_func_signature = module.make_signature();
        uniform_cps_impl_func_signature.params.push(AbiParam::new(I64)); // base address
        uniform_cps_impl_func_signature.params.push(AbiParam::new(I64)); // the continuation object
        uniform_cps_impl_func_signature.params.push(AbiParam::new(I64)); // the last result
        uniform_cps_impl_func_signature.returns.push(AbiParam::new(I64));
        uniform_cps_impl_func_signature.call_conv = CallConv::Tail;
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
            builtin_functions,
            static_strings: HashMap::new(),
            local_functions: HashMap::new(),
            uniform_func_signature,
            uniform_cps_func_signature,
            uniform_cps_impl_func_signature,
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
            if function_definition.may_be_simple {
                // Simple
                let simple_name = FunctionFlavor::Simple.decorate_name(name);
                self.local_functions.insert(simple_name, self.module.declare_function(name, Linkage::Local, &self.uniform_func_signature).unwrap());

                // Specializable
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
                    let specialized_name = FunctionFlavor::Specialized.decorate_name(name);
                    self.local_functions.insert(specialized_name.clone(), self.module.declare_function(&specialized_name, Linkage::Local, &sig).unwrap());
                    specialzied_function_signatures.insert(name, sig);
                }
            }
        }

        for (name, function_definition) in defs.iter() {
            if function_definition.may_be_simple {
                // simple
                SimpleFunctionTranslator::compile_simple_function(name, self, function_definition, &local_function_arg_types, clir);

                // specialized
                if function_definition.is_specializable() {
                    let sig = specialzied_function_signatures.get(name).unwrap();
                    SimpleFunctionTranslator::compile_specialized_function(name, self, sig.clone(), function_definition, &local_function_arg_types, clir);
                }
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

        let main_id = self.local_functions.get(&FunctionFlavor::Specialized.decorate_name("main")).unwrap();
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
}
