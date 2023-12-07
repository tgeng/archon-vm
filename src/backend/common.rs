use cranelift::codegen::Context;
use cranelift::codegen::isa::CallConv;
use cranelift::frontend::Switch;
use cbpv_runtime::runtime_utils::{runtime_alloc, runtime_force_thunk, runtime_alloc_stack, runtime_handle_simple_operation, runtime_prepare_operation, runtime_pop_handler, runtime_register_handler, runtime_add_simple_handler, runtime_add_complex_handler, runtime_prepare_resume_continuation};
use cranelift::prelude::*;
use cranelift::prelude::types::{F32, F64, I32, I64};
use cranelift_jit::{JITBuilder};
use cranelift_module::{FuncId, Linkage, Module};
use crate::ast::term::{VType, SpecializedType, PType};
use strum_macros::EnumIter;
use enum_map::{Enum};
use VType::{Specialized, Uniform};
use SpecializedType::{Integer, PrimitivePtr, StructPtr};

/// None means the function call is a tail call or returned so no value is returned.
pub type TypedValue = (Value, VType);
pub type TypedReturnValue = Option<TypedValue>;

pub trait HasType {
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
pub enum BuiltinFunction {
    // from rust runtime lib
    Alloc,
    ForceThunk,
    AllocStack,
    HandleSimpleOperation,
    PrepareOperation,
    PopHandler,
    RegisterHandlerAndGetTransformContinuation,
    AddSimpleHandler,
    AddComplexHandler,
    PrepareResumeContinuation,

    // generated
    GetTrivialContinuation,
    ConvertCapturedContinuationThunkImpl,
    /// Special CPS implementation that invokes pop handler and pass the "last result" as an
    /// argument to the actual transform function.
    TransformLoaderCpsImpl,
}

impl BuiltinFunction {
    fn func_name(&self) -> &'static str {
        match self {
            BuiltinFunction::Alloc => "__runtime_alloc__",
            BuiltinFunction::ForceThunk => "__runtime_force_thunk__",
            BuiltinFunction::AllocStack => "__runtime_alloc_stack__",
            BuiltinFunction::HandleSimpleOperation => "__runtime_handle_simple_operation",
            BuiltinFunction::PrepareOperation => "__runtime_prepare_complex_operation",
            BuiltinFunction::PopHandler => "__runtime_pop_handler",
            BuiltinFunction::RegisterHandlerAndGetTransformContinuation => "__runtime_register_handler_and_get_transform_continuation",
            BuiltinFunction::AddSimpleHandler => "__runtime_add_simple_handler",
            BuiltinFunction::AddComplexHandler => "__runtime_add_complex_handler",
            BuiltinFunction::PrepareResumeContinuation => "__runtime_prepare_resume_continuation",
            BuiltinFunction::GetTrivialContinuation => "__runtime_get_trivial_continuation",
            BuiltinFunction::ConvertCapturedContinuationThunkImpl => "__runtime_convert_captured_continuation_thunk_impl",
            BuiltinFunction::TransformLoaderCpsImpl => "__runtime_transform_loader_cps_impl"
        }
    }

    pub fn declare_symbol(&self, builder: &mut JITBuilder) {
        let func_ptr = match self {
            BuiltinFunction::Alloc => runtime_alloc as *const u8,
            BuiltinFunction::ForceThunk => runtime_force_thunk as *const u8,
            BuiltinFunction::AllocStack => runtime_alloc_stack as *const u8,
            BuiltinFunction::HandleSimpleOperation => runtime_handle_simple_operation as *const u8,
            BuiltinFunction::PrepareOperation => runtime_prepare_operation as *const u8,
            BuiltinFunction::PopHandler => runtime_pop_handler as *const u8,
            BuiltinFunction::RegisterHandlerAndGetTransformContinuation => runtime_register_handler as *const u8,
            BuiltinFunction::AddSimpleHandler => runtime_add_simple_handler as *const u8,
            BuiltinFunction::AddComplexHandler => runtime_add_complex_handler as *const u8,
            BuiltinFunction::PrepareResumeContinuation => runtime_prepare_resume_continuation as *const u8,
            BuiltinFunction::GetTrivialContinuation => return,
            BuiltinFunction::ConvertCapturedContinuationThunkImpl => return,
            BuiltinFunction::TransformLoaderCpsImpl => return,
        };

        builder.symbol(self.func_name(), func_ptr);
    }

    pub fn declare<M: Module>(&self, m: &mut M) -> FuncId {
        let mut sig = m.make_signature();
        let mut declare_external_func = |arg_count: usize, return_count: usize| {
            for _ in 0..arg_count {
                sig.params.push(AbiParam::new(I64));
            }
            for _ in 0..return_count {
                sig.returns.push(AbiParam::new(I64));
            }
            m.declare_function(self.func_name(), Linkage::Import, &sig).unwrap()
        };
        match self {
            BuiltinFunction::Alloc => declare_external_func(1, 1),
            BuiltinFunction::ForceThunk => declare_external_func(2, 1),
            BuiltinFunction::AllocStack => declare_external_func(0, 1),
            BuiltinFunction::HandleSimpleOperation => declare_external_func(3, 1),
            BuiltinFunction::PrepareOperation => declare_external_func(5, 1),
            BuiltinFunction::PopHandler => declare_external_func(0, 1),
            BuiltinFunction::RegisterHandlerAndGetTransformContinuation => declare_external_func(7, 1),
            BuiltinFunction::AddSimpleHandler => declare_external_func(3, 0),
            BuiltinFunction::AddComplexHandler => declare_external_func(3, 0),
            BuiltinFunction::PrepareResumeContinuation => declare_external_func(5, 1),
            BuiltinFunction::GetTrivialContinuation => {
                let mut ctx = m.make_context();
                let mut builder_context = FunctionBuilderContext::new();
                let impl_func_id = Self::generate_trivial_continuation_impl(m, &mut ctx, &mut builder_context);
                m.define_function(impl_func_id, &mut ctx).unwrap();
                ctx.clear();
                let func_id = Self::generate_trivial_continuation_helper(m, impl_func_id, &mut ctx, &mut builder_context);
                m.define_function(func_id, &mut ctx).unwrap();
                func_id
            }
            BuiltinFunction::ConvertCapturedContinuationThunkImpl => {
                let mut ctx = m.make_context();
                let mut builder_context = FunctionBuilderContext::new();
                let impl_func_id = Self::convert_captured_continuation_to_thunk_impl(m, &mut ctx, &mut builder_context);
                m.define_function(impl_func_id, &mut ctx).unwrap();
                impl_func_id
            }
            BuiltinFunction::TransformLoaderCpsImpl => {
                let mut ctx = m.make_context();
                let mut builder_context = FunctionBuilderContext::new();
                let impl_func_id = Self::transform_loader_cps_impl(m, &mut ctx, &mut builder_context);
                m.define_function(impl_func_id, &mut ctx).unwrap();
                impl_func_id
            }
        }
    }


    // TODO: replace this with a constant value to avoid allocations.
    fn generate_trivial_continuation_impl<M: Module>(module: &mut M, ctx: &mut Context, builder_ctx: &mut FunctionBuilderContext) -> FuncId {
        let sig = create_cps_impl_signature(module);
        let func_id = module.declare_function("__runtime_get_trivial_continuation_impl", Linkage::Local, &sig).unwrap();
        ctx.func.signature = sig;
        let mut builder = FunctionBuilder::new(&mut ctx.func, builder_ctx);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        // We don't need to do anything other than just return the last result because the height
        // of this trivial continuation should be exactly the number of arguments of the function
        // that accepts this trivial continuation. Hence, when that function calls this trivial
        // continuation, it will place the result right below the base address, which is where this
        // trivial continuation should be placing the result.
        let last_result_ptr = builder.block_params(entry_block)[2];
        builder.ins().return_(&[last_result_ptr]);
        builder.finalize();
        func_id
    }

    fn generate_trivial_continuation_helper<M: Module>(module: &mut M, impl_func_id: FuncId, ctx: &mut Context, builder_ctx: &mut FunctionBuilderContext) -> FuncId {
        let mut sig = module.make_signature();
        // The pointer to the created continuation object.
        sig.returns.push(AbiParam::new(I64));
        let func_id = module.declare_function(BuiltinFunction::GetTrivialContinuation.func_name(), Linkage::Local, &sig).unwrap();
        ctx.func.signature = sig;
        let mut builder = FunctionBuilder::new(&mut ctx.func, builder_ctx);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let alloc_func_id = BuiltinFunction::Alloc.declare(module);
        let alloc_func_ref = module.declare_func_in_func(alloc_func_id, builder.func);
        // We only need the first two words for the trivial continuation. The height is only
        // allocated so that it can be written to. Its value does not matter because trivial
        // continuation does not care about base address as it doesn't have any arguments.
        let continuation_size = builder.ins().iconst(I64, 2);
        let inst = builder.ins().call(alloc_func_ref, &[continuation_size]);
        let continuation_ptr = builder.inst_results(inst)[0];

        // first word of continuation is the continuation implementation
        let impl_func_ref = module.declare_func_in_func(impl_func_id, builder.func);
        let impl_func_ptr = builder.ins().func_addr(I64, impl_func_ref);
        builder.ins().store(MemFlags::new(), impl_func_ptr, continuation_ptr, 0);

        // second word is the frame height, whose value does not matter.
        let frame_height = builder.ins().iconst(I64, 0);
        builder.ins().store(MemFlags::new(), frame_height, continuation_ptr, 8);

        // return the pointer to the continuation object
        builder.ins().return_(&[continuation_ptr]);
        builder.finalize();
        func_id
    }

    fn convert_captured_continuation_to_thunk_impl<M: Module>(module: &mut M, ctx: &mut Context, builder_ctx: &mut FunctionBuilderContext) -> FuncId {
        // This function is the implementation function of the captured continuation record thunk.
        // It has the signature of a normal CPS function and it takes the following arguments on the
        // stack:
        //   * captured continuation object (this should be placed inside the thunk object)
        //   * record field (resume, dispose, or replicate), which affects the next argument(s)
        //
        // if resume
        //   * the handler parameter
        //   * the result used to resume the captured continuation
        //
        // if dispose or replicate
        //   * the handler parameter
        //
        // This function then invokes the built-in functions that implement the corresponding action
        // and keeps the execution forward.

        let sig = create_cps_signature(module);
        let func_id = module.declare_function(BuiltinFunction::ConvertCapturedContinuationThunkImpl.func_name(), Linkage::Local, &sig).unwrap();
        ctx.func.signature = sig;
        let mut builder = FunctionBuilder::new(&mut ctx.func, builder_ctx);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let base_address = builder.block_params(entry_block)[0];
        let next_continuation = builder.block_params(entry_block)[1];
        let captured_continuation = builder.ins().load(I64, MemFlags::new(), base_address, 0);
        let captured_continuation = builder.ins().iadd_imm(captured_continuation, -0b01); // untag SPtr
        // Field is the field of the captured continuation record. Background: a captured
        // continuation is represented as a record in the core type theory. There are three fields:
        // resume, dispose, and replicate. Since records are compiled to function with integer input
        // corresponding to all the fields. We just load that compiled integer value and dispatch to
        // the corresponding action.
        let field = builder.ins().load(I64, MemFlags::new(), base_address, 8);
        // The field is a tagged integer, so we need to shift it to the right by one to get the
        // actual value.
        let field = builder.ins().sshr_imm(field, 1);
        // All three actions assume that there is a handler parameter argument
        let handler_parameter = builder.ins().load(I64, MemFlags::new(), base_address, 16);

        let mut switch = Switch::new();
        let resume_block = builder.create_block();
        switch.set_entry(0, resume_block);
        let dispose_block = builder.create_block();
        switch.set_entry(1, resume_block);
        let replicate_block = builder.create_block();
        switch.set_entry(2, resume_block);
        let default_block = builder.create_block();
        switch.emit(&mut builder, field, default_block);

        builder.seal_all_blocks();

        // resume
        builder.switch_to_block(resume_block);
        let result = builder.ins().load(I64, MemFlags::new(), base_address, 24);
        let prepare_func_id = BuiltinFunction::PrepareResumeContinuation.declare(module);
        let prepare_func_ref = module.declare_func_in_func(prepare_func_id, builder.func);
        let inst = builder.ins().call(prepare_func_ref, &[base_address, next_continuation, captured_continuation, handler_parameter, result]);
        let prepare_result_ptr = builder.inst_results(inst)[0];
        let continuation = builder.ins().load(I64, MemFlags::new(), prepare_result_ptr, 0);
        let new_base_address = builder.ins().load(I64, MemFlags::new(), prepare_result_ptr, 8);
        let last_result_ptr = builder.ins().load(I64, MemFlags::new(), prepare_result_ptr, 16);
        let continuation_impl = builder.ins().load(I64, MemFlags::new(), continuation, 0);
        let cps_impl_sig = create_cps_impl_signature(module);
        let cps_impl_sig_ref = builder.import_signature(cps_impl_sig);
        builder.ins().return_call_indirect(cps_impl_sig_ref, continuation_impl, &[new_base_address, continuation, last_result_ptr]);

        // dispose
        builder.switch_to_block(dispose_block);
        // TODO: implement dispose
        let zero = builder.ins().iconst(I64, 0);
        builder.ins().return_(&[zero]);

        // replicate
        builder.switch_to_block(replicate_block);
        // TODO: implement replicate
        builder.ins().return_(&[zero]);

        // default
        builder.switch_to_block(default_block);
        builder.ins().trap(TrapCode::UnreachableCodeReached);
        builder.finalize();

        func_id
    }

    fn transform_loader_cps_impl<M: Module>(module: &mut M, ctx: &mut Context, builder_ctx: &mut FunctionBuilderContext) -> FuncId {
        let sig = create_cps_impl_signature(module);
        let func_id = module.declare_function(BuiltinFunction::TransformLoaderCpsImpl.func_name(), Linkage::Local, &sig).unwrap();
        ctx.func.signature = sig;
        let mut builder = FunctionBuilder::new(&mut ctx.func, builder_ctx);
        let entry_block = builder.create_block();
        let tip_address_slot = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8));
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let base_address = builder.block_params(entry_block)[0];
        let current_continuation = builder.block_params(entry_block)[1];
        let last_result_ptr = builder.block_params(entry_block)[2];
        let last_result = builder.ins().load(I64, MemFlags::new(), last_result_ptr, 0);

        // transform thunk is set on the argument stack by `runtime_register_handler` when it
        // creates the transform loader continuation.
        let transform_thunk = builder.ins().load(I64, MemFlags::new(), base_address, 0);

        let pop_handler_func_id = BuiltinFunction::PopHandler.declare(module);
        let pop_handler_func_ref = module.declare_func_in_func(pop_handler_func_id, builder.func);
        let inst = builder.ins().call(pop_handler_func_ref, &[]);
        let handler_parameter = builder.inst_results(inst)[0];

        // set the tip address to base address + 8 so that the only argument of transform loader
        // is taken for the final tail call to the transform function.
        let tip_address = builder.ins().iadd_imm(base_address, 8);

        let force_thunk_func_id = BuiltinFunction::ForceThunk.declare(module);
        let force_thunk_func_ref = module.declare_func_in_func(force_thunk_func_id, builder.func);

        // push all the thunk arguments to the stack
        builder.ins().stack_store(tip_address, tip_address_slot, 0);
        let tip_address_ptr = builder.ins().stack_addr(I64, tip_address_slot, 0);
        let inst = builder.ins().call(force_thunk_func_ref, &[transform_thunk, tip_address_ptr]);
        let transform_ptr = builder.inst_results(inst)[0];
        let tip_address = builder.ins().stack_load(I64, tip_address_slot, 0);

        // write handler parameter and result on stack. The first parameter is written closer to the
        // base address, so it's lower on the stack.
        builder.ins().store(MemFlags::new(), handler_parameter, tip_address, -16);
        builder.ins().store(MemFlags::new(), last_result, tip_address, -8);
        let tip_address = builder.ins().iadd_imm(tip_address, -16);

        let next_continuation = builder.ins().load(I64, MemFlags::new(), current_continuation, 16);
        // update frame height of the next continuation to account for the transform arguments
        // pushed to the stack
        let next_continuation_frame_height = builder.ins().load(I64, MemFlags::new(), next_continuation, 8);
        let next_continuation_frame_height_delta_bytes = builder.ins().isub(tip_address, base_address);
        let next_continuation_frame_height_delta = builder.ins().ushr_imm(next_continuation_frame_height_delta_bytes, 3);
        let next_continuation_frame_height = builder.ins().iadd(next_continuation_frame_height, next_continuation_frame_height_delta);
        builder.ins().store(MemFlags::new(), next_continuation_frame_height, next_continuation, 8);

        // call the next continuation
        let sig = create_cps_signature(module);
        let sig_ref = builder.import_signature(sig);
        builder.ins().return_call_indirect(sig_ref, transform_ptr, &[tip_address, next_continuation]);

        builder.finalize();

        func_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionFlavor {
    /// The function arguments are pushed via pushing to the argument stack and a continuation is
    /// passed to accept the return value. This is the default mode and all functions are compiled
    /// to this mode.
    Cps,
    /// The CPS implementation of the function. This function transitions the continuation object
    /// from one state to the next.
    CpsImpl,
    /// The function arguments are passed via pushing to the argument stack. The function does not
    /// perform any complex effects so return value is returned directly. Only functions that can
    /// possibly perform no or just simple effects are compiled to this mode.
    Simple,
    /// The function arguments are passed directly and return value is returned directly. Only
    /// functions that may only perform no or simple effects and are specializable are compiled to
    /// this mode.
    Specialized,
}

impl FunctionFlavor {
    pub fn decorate_name(&self, function_name: &str) -> String {
        match self {
            FunctionFlavor::Cps => format!("{}__cps", function_name),
            FunctionFlavor::CpsImpl => format!("{}__cps_impl", function_name),
            FunctionFlavor::Simple => format!("{}__simple", function_name),
            FunctionFlavor::Specialized => format!("{}__specialized", function_name),
        }
    }
}

pub fn create_cps_impl_signature<M: Module>(module: &M) -> Signature {
    let mut uniform_cps_impl_func_signature = module.make_signature();
    uniform_cps_impl_func_signature.params.push(AbiParam::new(I64)); // base address
    uniform_cps_impl_func_signature.params.push(AbiParam::new(I64)); // the current continuation object
    uniform_cps_impl_func_signature.params.push(AbiParam::new(I64)); // the last result
    uniform_cps_impl_func_signature.returns.push(AbiParam::new(I64));
    uniform_cps_impl_func_signature.call_conv = CallConv::Tail;
    uniform_cps_impl_func_signature
}

pub fn create_cps_signature<M: Module>(module: &M) -> Signature {
    let mut uniform_cps_func_signature = module.make_signature();
    uniform_cps_func_signature.params.push(AbiParam::new(I64)); // base address
    uniform_cps_func_signature.params.push(AbiParam::new(I64)); // the next continuation object
    uniform_cps_func_signature.returns.push(AbiParam::new(I64));
    uniform_cps_func_signature.call_conv = CallConv::Tail;
    uniform_cps_func_signature
}

