use cranelift::prelude::*;
use cranelift::prelude::types::I64;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Module};
use crate::signature::FunctionDefinition;
use crate::term::{CTerm, VTerm};

static INT: Type = I64;

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
        // All functions have the same signature `i64 -> ()`, where the single argument is the
        // base address of the parameter stack. Actual parameters can be obtained by offsetting this
        // base address.
        // The return value should be stored at the address computed by adding the total size of the
        // parameters to the callee base address. See diagram below for the following call
        //
        // fn caller(x, y) {
        //   ...
        //   let i = callee(a, b, c);
        //   ...
        // }
        //
        //  |---------| <- caller return address
        //  |    y    |
        //  |---------|
        //  |    x    |
        //  |---------| <- caller base address
        //  |    i    |
        //  |---------| <- callee return address: the address where callee put the return value
        //  |    c    |
        //  |---------|
        //  |    b    |
        //  |---------|
        //  |    a    |
        //  |---------| <- callee base address: the address from which callee finds arguments

        self.ctx.func.signature.params.push(AbiParam::new(INT));

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
            local_vars: &mut vars,
            base_address,
        };
        translator.translate_c_term(&function_definition.body, TranslationContext {
            current_type: INT,
            is_tail: true,
        });
    }
}

struct TranslationContext {
    current_type: Type,
    is_tail: bool,
}

impl TranslationContext {
    fn with_current_type(&mut self, current_type: Type) -> &mut Self {
        self.current_type = current_type;
        self
    }
    fn with_is_tail(&mut self, is_tail: bool) -> &mut Self {
        self.is_tail = is_tail;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum ValueOrParam {
    Value(Value),
    Param(usize),
}

struct FunctionTranslator<'a, M: Module> {
    module: &'a mut M,
    function_builder: &'a mut FunctionBuilder<'a>,
    local_vars: &'a mut [Option<ValueOrParam>],
    base_address: Value,
}

impl<'a, M: Module> FunctionTranslator<'a, M> {
    fn translate_c_term(&mut self, c_term: &CTerm, ctx: TranslationContext) -> ValueOrParam {
        match c_term {
            CTerm::Redex { .. } => todo!(),
            CTerm::Return { .. } => todo!(),
            CTerm::Force { .. } => todo!(),
            CTerm::Let { .. } => todo!(),
            CTerm::Def { .. } => todo!(),
            CTerm::CaseInt { .. } => todo!(),
            CTerm::CaseTuple { .. } => todo!(),
            CTerm::CaseStr { .. } => todo!(),
            CTerm::Primitive { .. } => todo!(),
        }
    }

    fn translate_v_term(&mut self, v_term: &VTerm, ctx: TranslationContext) -> ValueOrParam {
        match v_term {
            VTerm::Var { index } => self.local_vars[*index].unwrap(),
            VTerm::Thunk { .. } => todo!(),
            VTerm::Int { value } => ValueOrParam::Value(self.function_builder.ins().iconst(INT, *value)),
            VTerm::Str { .. } => todo!(),
            VTerm::Tuple { .. } => todo!(),
        }
    }

    fn translate_value_or_param(&mut self, value_or_param: ValueOrParam, ctx: TranslationContext) -> Value {
        match value_or_param {
            ValueOrParam::Value(v) => v,
            ValueOrParam::Param(param_index) => {
                let base_address = self.base_address;
                let offset = (param_index + 1) * 8;
                let address = self.function_builder.ins().iadd_imm(base_address, offset as i64);
                let value = self.function_builder.ins().load(ctx.current_type, MemFlags::new(), address, 0);
                value
            }
        }
    }
}
