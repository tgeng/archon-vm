use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};
use crate::signature::FunctionDefinition;
use crate::visitor::Visitor;

/// The basic JIT class.
pub struct Compiler<M: Module> {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data description, which is to data objects what `ctx` is to functions.
    data_description: DataDescription,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: M,
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
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        let module = JITModule::new(builder);
        Self::new(module)
    }
}

impl<M: Module> Compiler<M> {
    fn new(module: M) -> Self {
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
        }
    }

    fn process(&mut self, name: &str, function_definition: &FunctionDefinition) {
        FunctionTranslator {
            compiler: self,
            local_vars: &mut vec![VarState::Undefined; function_definition.var_bound],
        }.visit_c_term(&function_definition.body);
    }
}

#[derive(Debug, Clone, PartialEq)]
enum VarState {
    Undefined,
    Defined(Value),
    Param(usize),
}

struct FunctionTranslator<'a, M: Module> {
    compiler: &'a mut Compiler<M>,
    local_vars: &'a mut [VarState],
}

impl<'a, M: Module> FunctionTranslator<'a, M> {}

impl<'a, M: Module> Visitor for FunctionTranslator<'a, M> {}
