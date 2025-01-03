use crate::ast::signature::FunctionDefinition;
use crate::ast::term::CType;
use crate::backend::common::{
    create_cps_impl_signature, create_cps_signature, BuiltinData, BuiltinFunction, FunctionFlavor,
    HasType,
};
use crate::backend::cps_function_translator::CpsFunctionTranslator;
use crate::backend::simple_function_translator::SimpleFunctionTranslator;
use cranelift::codegen::isa::CallConv;
use cranelift::prelude::types::I64;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataId, FuncId, Linkage, Module};
use cranelift_object::ObjectModule;
use enum_map::EnumMap;
use std::arch::global_asm;
use std::collections::HashMap;
use strum::IntoEnumIterator;

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
    pub builtin_data: EnumMap<BuiltinData, DataId>,
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
        flag_builder.set("preserve_frame_pointers", "true").unwrap();
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

        Self::new(JITModule::new(builder))
    }
}

#[cfg(target_arch = "aarch64")]
global_asm!(
    r#"
    .global _invoke_compiled_function

    _invoke_compiled_function:
        // Store all callee-saved registers on the stack
        stp x19, x20, [sp, #-16]!
        stp x21, x22, [sp, #-16]!
        stp x23, x24, [sp, #-16]!
        stp x25, x26, [sp, #-16]!
        stp x27, x28, [sp, #-16]!
        stp x29, x30, [sp, #-16]!

        // x0 is the first argument, which is the function pointer
        blr x0 // Call the function

        // Restore all callee-saved registers from the stack
        ldp x29, x30, [sp], #16
        ldp x27, x28, [sp], #16
        ldp x25, x26, [sp], #16
        ldp x23, x24, [sp], #16
        ldp x21, x22, [sp], #16
        ldp x19, x20, [sp], #16

        ret
"#
);

extern "C" {
    /// compiled function are often invoked with cranelift's tail call convention , which doesn't
    /// respect callee-saved registers (callee-saved registers are simply not used). But rust
    /// functions expects callee-saved registers to be preserved. So we need to wrap the compiled
    /// function and save & restore the callee-saved registers when invoking the compiled function.
    fn invoke_compiled_function(f: fn() -> usize) -> usize;
}

impl Compiler<JITModule> {
    pub fn finalize_and_get_main(&mut self) -> impl Fn() -> usize {
        self.module.finalize_definitions().unwrap();
        let main_func_id = self.local_functions.get(MAIN_WRAPPER_NAME).unwrap();
        let func_ptr = self.module.get_finalized_function(*main_func_id);
        move || unsafe {
            invoke_compiled_function(std::mem::transmute::<_, fn() -> usize>(func_ptr))
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
    fn new(mut module: M) -> Self {
        let builtin_functions =
            EnumMap::from_fn(|e: BuiltinFunction| e.declare_or_define(&mut module));
        let builtin_data = EnumMap::from_fn(|e: BuiltinData| e.define(&mut module));

        let mut uniform_func_signature = module.make_signature();
        uniform_func_signature.params.push(AbiParam::new(I64));
        uniform_func_signature.returns.push(AbiParam::new(I64));
        uniform_func_signature.call_conv = CallConv::Tail;

        let uniform_cps_func_signature = create_cps_signature(&module);

        let uniform_cps_impl_func_signature = create_cps_impl_signature(&module);

        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
            builtin_functions,
            builtin_data,
            static_strings: HashMap::new(),
            local_functions: HashMap::new(),
            uniform_func_signature,
            uniform_cps_func_signature,
            uniform_cps_impl_func_signature,
        }
    }

    pub fn compile(
        &mut self,
        defs: &[(String, FunctionDefinition)],
        clir: &mut Option<&mut Vec<(String, String)>>,
    ) {
        let mut specialized_function_signatures = HashMap::new();
        let mut local_function_arg_types = HashMap::new();
        for (name, function_definition) in defs.iter() {
            local_function_arg_types.insert(
                name.clone(),
                (
                    function_definition
                        .args
                        .iter()
                        .map(|(_, v_type)| *v_type)
                        .collect::<Vec<_>>(),
                    function_definition.c_type,
                ),
            );

            if function_definition.need_cps {
                let cps_name = FunctionFlavor::Cps.decorate_name(name);
                let function = self
                    .module
                    .declare_function(&cps_name, Linkage::Local, &self.uniform_cps_func_signature)
                    .unwrap();
                self.local_functions.insert(cps_name, function);

                let cps_impl_name = FunctionFlavor::CpsImpl.decorate_name(name);
                let function = self
                    .module
                    .declare_function(
                        &cps_impl_name,
                        Linkage::Local,
                        &self.uniform_cps_impl_func_signature,
                    )
                    .unwrap();
                self.local_functions.insert(cps_impl_name, function);
            }

            if function_definition.need_simple {
                // Simple
                let simple_name = FunctionFlavor::Simple.decorate_name(name);
                let function = self
                    .module
                    .declare_function(&simple_name, Linkage::Local, &self.uniform_func_signature)
                    .unwrap();
                self.local_functions.insert(simple_name, function);
            }

            if function_definition.need_specialized {
                let CType::SpecializedF(v_type) = function_definition.c_type else {
                    unreachable!()
                };
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
                self.local_functions.insert(
                    specialized_name.clone(),
                    self.module
                        .declare_function(&specialized_name, Linkage::Local, &sig)
                        .unwrap(),
                );
                specialized_function_signatures.insert(name, sig);
            }
        }

        for (name, function_definition) in defs.iter() {
            if function_definition.need_cps {
                // CPS
                CpsFunctionTranslator::compile_cps_function(
                    name,
                    self,
                    function_definition,
                    &local_function_arg_types,
                    clir,
                    true,
                );
            }

            if function_definition.need_simple {
                // simple
                SimpleFunctionTranslator::compile_simple_function(
                    name,
                    self,
                    function_definition,
                    &local_function_arg_types,
                    clir,
                );
            }
            // specialized
            if function_definition.need_specialized {
                let sig = specialized_function_signatures.get(name).unwrap();
                SimpleFunctionTranslator::compile_specialized_function(
                    name,
                    self,
                    sig.clone(),
                    function_definition,
                    &local_function_arg_types,
                    clir,
                );
            }
        }

        self.generate_main_wrapper(clir);
    }

    /// Creates a main wrapper function (named `__main__`) that calls the `__runtime_alloc_stack__`,
    /// which sets up the parameter stack and invokes the user-defined `main` function.
    fn generate_main_wrapper(&mut self, clir: &mut Option<&mut Vec<(String, String)>>) {
        let main_wrapper_id = self
            .module
            .declare_function(
                MAIN_WRAPPER_NAME,
                Linkage::Local,
                &self.uniform_func_signature,
            )
            .unwrap();
        self.local_functions
            .insert(MAIN_WRAPPER_NAME.to_string(), main_wrapper_id);
        self.ctx.clear();
        self.ctx.func.signature.returns.push(AbiParam::new(I64));

        let mut function_builder =
            FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = function_builder.create_block();

        function_builder.append_block_params_for_function_params(entry_block);
        function_builder.switch_to_block(entry_block);
        function_builder.seal_block(entry_block);

        let alloc_stack_id = self.builtin_functions[BuiltinFunction::AllocStack];
        let alloc_stack_func_ref = self
            .module
            .declare_func_in_func(alloc_stack_id, function_builder.func);
        let inst = function_builder.ins().call(alloc_stack_func_ref, &[]);
        let stack_base = function_builder.inst_results(inst)[0];

        let main_id = self
            .local_functions
            .get(&FunctionFlavor::Specialized.decorate_name("main"))
            .unwrap();
        let main_func_ref = self
            .module
            .declare_func_in_func(*main_id, function_builder.func);
        let inst = function_builder.ins().call(main_func_ref, &[stack_base]);
        let return_value = function_builder.inst_results(inst)[0];
        function_builder.ins().return_(&[return_value]);

        function_builder.finalize();

        self.define_function("__main__", main_wrapper_id, clir);
        self.module.clear_context(&mut self.ctx);
    }

    pub fn define_function(
        &mut self,
        name: &str,
        func_id: FuncId,
        clir: &mut Option<&mut Vec<(String, String)>>,
    ) {
        if let Some(clir) = clir {
            clir.push((name.to_owned(), format!("{}", self.ctx.func.display())));
        }
        self.module.define_function(func_id, &mut self.ctx).unwrap();
    }
}
