use cranelift::codegen::ir::Inst;
use cranelift::codegen::isa::CallConv;
use cranelift::frontend::Switch;
use cbpv_runtime::runtime_utils::{runtime_alloc, runtime_force_thunk, runtime_alloc_stack, debug_helper, runtime_prepare_operation, runtime_pop_handler, runtime_register_handler, runtime_add_simple_handler, runtime_add_complex_handler, runtime_prepare_resume_continuation, runtime_process_simple_handler_result, runtime_dispose_continuation, runtime_mark_handler};
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
    DebugHelper,
    PrepareOperation,
    PopHandler,
    RegisterHandler,
    AddSimpleHandler,
    AddComplexHandler,
    PrepareResumeContinuation,
    DisposeContinuation,
    ProcessSimpleHandlerResult,
    MarkHandler,

    // generated
    GetTrivialContinuation,
    TrivialContinuationImpl,
    /// CPS function implementing reified continuation record. This function takes a captured
    /// continuation object, followed by a field projection and the needed arguments on the argument
    /// stack. The signature is a normal CPS function signature ((base address, next continuation) -> final result).
    CapturedContinuationRecordImpl,
    /// CPS function that invokes the corresponding handler and processes the result. Simple
    /// handlers always returns a pair: (new handler parameter, result), where result is either
    /// a normal value (tag 1) or an exception value (tag 0).
    /// The signature is a normal CPS function signature ((base address, next continuation) -> final result).
    /// This function takes the following arguments from the argument stack (last argument list on
    /// top):
    /// - explicit handler arguments
    /// - handler parameter
    /// - implicit handler arguments (those captured in handler thunk)
    /// - pointer to the matching handler object in raw pointer representation
    /// - handler implementation function pointer in raw pointer representation
    SimpleHandlerRunnerImpl,
    /// Special CPS implementation that invokes pop handler and pass the "last result" as an
    /// argument to the actual transform function. The signature follows that of a normal CPS impl
    /// ((base address, current continuation, last result) -> final result).
    TransformLoaderCpsImpl,
    /// Arguments: CPS function pointer, base address
    /// Returns: result pointer, used to update the base address
    InvokeCpsFunctionWithTrivialContinuation,
}

impl BuiltinFunction {
    fn func_name(&self) -> &'static str {
        match self {
            BuiltinFunction::DebugHelper => "__runtime_debug_helper",
            BuiltinFunction::Alloc => "__runtime_alloc__",
            BuiltinFunction::ForceThunk => "__runtime_force_thunk__",
            BuiltinFunction::AllocStack => "__runtime_alloc_stack__",
            BuiltinFunction::PrepareOperation => "__runtime_prepare_complex_operation",
            BuiltinFunction::PopHandler => "__runtime_pop_handler",
            BuiltinFunction::RegisterHandler => "__runtime_register_handler",
            BuiltinFunction::AddSimpleHandler => "__runtime_add_simple_handler",
            BuiltinFunction::AddComplexHandler => "__runtime_add_complex_handler",
            BuiltinFunction::PrepareResumeContinuation => "__runtime_prepare_resume_continuation",
            BuiltinFunction::DisposeContinuation => "__runtime_dispose_continuation",
            BuiltinFunction::ProcessSimpleHandlerResult => "__runtime_process_simple_handler_result",
            BuiltinFunction::MarkHandler => "__runtime_mark_handler",

            BuiltinFunction::GetTrivialContinuation => "__runtime_get_trivial_continuation",
            BuiltinFunction::TrivialContinuationImpl => "__runtime_trivial_continuation_impl",
            BuiltinFunction::CapturedContinuationRecordImpl => "__runtime_captured_continuation_record_impl",
            BuiltinFunction::SimpleHandlerRunnerImpl => "__runtime_simple_handler_runner_impl",
            BuiltinFunction::TransformLoaderCpsImpl => "__runtime_transform_loader_cps_impl",
            BuiltinFunction::InvokeCpsFunctionWithTrivialContinuation => "__runtime_invoke_cps_function",
        }
    }

    pub fn declare_symbol(&self, builder: &mut JITBuilder) {
        let func_ptr = match self {
            BuiltinFunction::DebugHelper => debug_helper as *const u8,
            BuiltinFunction::Alloc => runtime_alloc as *const u8,
            BuiltinFunction::ForceThunk => runtime_force_thunk as *const u8,
            BuiltinFunction::AllocStack => runtime_alloc_stack as *const u8,
            BuiltinFunction::PrepareOperation => runtime_prepare_operation as *const u8,
            BuiltinFunction::PopHandler => runtime_pop_handler as *const u8,
            BuiltinFunction::RegisterHandler => runtime_register_handler as *const u8,
            BuiltinFunction::AddSimpleHandler => runtime_add_simple_handler as *const u8,
            BuiltinFunction::AddComplexHandler => runtime_add_complex_handler as *const u8,
            BuiltinFunction::PrepareResumeContinuation => runtime_prepare_resume_continuation as *const u8,
            BuiltinFunction::DisposeContinuation => runtime_dispose_continuation as *const u8,
            BuiltinFunction::ProcessSimpleHandlerResult => runtime_process_simple_handler_result as *const u8,
            BuiltinFunction::MarkHandler => runtime_mark_handler as *const u8,

            BuiltinFunction::GetTrivialContinuation => return,
            BuiltinFunction::TrivialContinuationImpl => return,
            BuiltinFunction::CapturedContinuationRecordImpl => return,
            BuiltinFunction::SimpleHandlerRunnerImpl => return,
            BuiltinFunction::TransformLoaderCpsImpl => return,
            BuiltinFunction::InvokeCpsFunctionWithTrivialContinuation => return,
        };

        builder.symbol(self.func_name(), func_ptr);
    }

    pub fn declare<M: Module>(&self, m: &mut M) -> (FuncId, Signature, Linkage) {
        let mut sig = m.make_signature();
        let mut declare_func_with_call_conv = |m: &mut M, arg_count: usize, return_count: usize, linkage: Linkage, call_conv: CallConv| {
            for _ in 0..arg_count {
                sig.params.push(AbiParam::new(I64));
            }
            for _ in 0..return_count {
                sig.returns.push(AbiParam::new(I64));
            }
            sig.call_conv = call_conv;
            (m.declare_function(self.func_name(), linkage, &sig).unwrap(), linkage)
        };

        let mut declare_func = |arg_count: usize, return_count: usize, linkage: Linkage| {
            declare_func_with_call_conv(m, arg_count, return_count, linkage, m.isa().default_call_conv())
        };

        let (func_id, linkage) = match self {
            BuiltinFunction::DebugHelper => declare_func(3, 1, Linkage::Import),
            BuiltinFunction::Alloc => declare_func(1, 1, Linkage::Import),
            BuiltinFunction::ForceThunk => declare_func(2, 1, Linkage::Import),
            BuiltinFunction::AllocStack => declare_func(0, 1, Linkage::Import),
            BuiltinFunction::PrepareOperation => declare_func(7, 1, Linkage::Import),
            BuiltinFunction::PopHandler => declare_func(0, 1, Linkage::Import),
            BuiltinFunction::RegisterHandler => declare_func(7, 1, Linkage::Import),
            BuiltinFunction::AddSimpleHandler => declare_func(3, 0, Linkage::Import),
            BuiltinFunction::AddComplexHandler => declare_func(3, 0, Linkage::Import),
            BuiltinFunction::PrepareResumeContinuation => declare_func(7, 1, Linkage::Import),
            BuiltinFunction::DisposeContinuation => declare_func(7, 1, Linkage::Import),
            BuiltinFunction::ProcessSimpleHandlerResult => declare_func(2, 1, Linkage::Import),
            BuiltinFunction::MarkHandler => declare_func(4, 1, Linkage::Import),

            BuiltinFunction::GetTrivialContinuation => declare_func(0, 1, Linkage::Local),
            BuiltinFunction::TrivialContinuationImpl => declare_func_with_call_conv(m, 3, 1, Linkage::Local, CallConv::Tail),
            BuiltinFunction::CapturedContinuationRecordImpl => declare_func_with_call_conv(m, 2, 1, Linkage::Local, CallConv::Tail),
            BuiltinFunction::SimpleHandlerRunnerImpl => declare_func_with_call_conv(m, 2, 1, Linkage::Local, CallConv::Tail),
            BuiltinFunction::TransformLoaderCpsImpl => declare_func_with_call_conv(m, 3, 1, Linkage::Local, CallConv::Tail),
            BuiltinFunction::InvokeCpsFunctionWithTrivialContinuation => declare_func(2, 1, Linkage::Local),
        };
        (func_id, sig, linkage)
    }

    pub fn declare_or_define<M: Module>(&self, m: &mut M) -> FuncId {
        let (func_id, sig, linkage) = self.declare(m);
        if linkage == Linkage::Import {
            return func_id;
        }
        let mut ctx = m.make_context();
        ctx.func.signature = sig;
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
        match self {
            BuiltinFunction::GetTrivialContinuation => Self::get_trivial_continuation(m, &mut builder),
            BuiltinFunction::TrivialContinuationImpl => Self::trivial_continuation_impl(&mut builder),
            BuiltinFunction::CapturedContinuationRecordImpl => Self::captured_continuation_record_impl(m, &mut builder),
            BuiltinFunction::SimpleHandlerRunnerImpl => Self::simple_handler_runner_impl(m, &mut builder),
            BuiltinFunction::TransformLoaderCpsImpl => Self::transform_loader_cps_impl(m, &mut builder),
            BuiltinFunction::InvokeCpsFunctionWithTrivialContinuation => Self::invoke_cps_function_with_trivial_continuation(m, &mut builder),
            _ => { unreachable!() }
        }
        builder.finalize();
        m.define_function(func_id, &mut ctx).unwrap();
        func_id
    }


    // TODO: replace this with a constant value to avoid allocations.
    fn trivial_continuation_impl(builder: &mut FunctionBuilder) {
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
    }

    fn get_trivial_continuation<M: Module>(m: &mut M, builder: &mut FunctionBuilder) {
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // We only need the first two words for the trivial continuation. The height is only
        // allocated so that it can be written to. Its value does not matter because trivial
        // continuation does not care about base address as it doesn't have any arguments.
        let continuation_size = builder.ins().iconst(I64, 2);
        let inst = Self::call_built_in(m, builder, BuiltinFunction::Alloc, &[continuation_size]);
        let continuation_ptr = builder.inst_results(inst)[0];

        // first word of continuation is the continuation implementation
        let impl_func_ptr = Self::get_built_in_func_ptr(m, builder, BuiltinFunction::TrivialContinuationImpl);
        builder.ins().store(MemFlags::new(), impl_func_ptr, continuation_ptr, 0);

        // second word is the frame height, whose value does not matter. But we set it to a large
        // value so that it won't overflow or underflow easily (this only matters for debug build,
        // where it panics when arithmetics overflows or underflows). Note that we can't go too
        // close to the maximum value of I64 because we often shifts the frame height by 3 bits to
        // the right to get the size in bytes for computation.
        let frame_height = builder.ins().iconst(I64, 1 << 59);
        builder.ins().store(MemFlags::new(), frame_height, continuation_ptr, 8);

        // return the pointer to the continuation object
        builder.ins().return_(&[continuation_ptr]);
    }

    fn captured_continuation_record_impl<M: Module>(m: &mut M, builder: &mut FunctionBuilder) {
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

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let frame_pointer = builder.ins().get_frame_pointer(I64);
        let stack_pointer = builder.ins().get_stack_pointer(I64);

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
        let field = builder.ins().ushr_imm(field, 1);
        // All three actions assume that there is a handler parameter argument
        let handler_parameter = builder.ins().load(I64, MemFlags::new(), base_address, 16);

        let mut switch = Switch::new();
        let resume_block = builder.create_block();
        switch.set_entry(0, resume_block);
        let dispose_block = builder.create_block();
        switch.set_entry(1, dispose_block);
        let replicate_block = builder.create_block();
        switch.set_entry(2, replicate_block);
        let default_block = builder.create_block();
        switch.emit(builder, field, default_block);

        builder.seal_all_blocks();

        // resume
        builder.switch_to_block(resume_block);
        let result = builder.ins().load(I64, MemFlags::new(), base_address, 24);
        let inst = Self::call_built_in(
            m,
            builder,
            BuiltinFunction::PrepareResumeContinuation,
            &[base_address, next_continuation, captured_continuation, handler_parameter, result, frame_pointer, stack_pointer],
        );
        Self::tail_call_continuation(m, builder, inst);

        // dispose
        builder.switch_to_block(dispose_block);
        let invoke_cps_function_with_trivial_continuation = Self::get_built_in_func_ptr(m, builder, BuiltinFunction::InvokeCpsFunctionWithTrivialContinuation);
        let inst = Self::call_built_in(
            m,
            builder,
            BuiltinFunction::DisposeContinuation,
            &[base_address, next_continuation, captured_continuation, handler_parameter, invoke_cps_function_with_trivial_continuation, frame_pointer, stack_pointer],
        );
        Self::tail_call_continuation(m, builder, inst);

        // replicate
        builder.switch_to_block(replicate_block);
        // TODO: implement replicate
        let zero = builder.ins().iconst(I64, 0);
        builder.ins().return_(&[zero]);

        // default
        builder.switch_to_block(default_block);
        builder.ins().trap(TrapCode::UnreachableCodeReached);
    }

    fn tail_call_continuation<M: Module>(m: &mut M, builder: &mut FunctionBuilder, inst: Inst) {
        let prepare_result_ptr = builder.inst_results(inst)[0];
        let continuation = builder.ins().load(I64, MemFlags::new(), prepare_result_ptr, 0);
        let new_base_address = builder.ins().load(I64, MemFlags::new(), prepare_result_ptr, 8);
        let last_result_ptr = builder.ins().load(I64, MemFlags::new(), prepare_result_ptr, 16);
        let continuation_impl = builder.ins().load(I64, MemFlags::new(), continuation, 0);
        let cps_impl_sig = create_cps_impl_signature(m);
        let cps_impl_sig_ref = builder.import_signature(cps_impl_sig);
        builder.ins().return_call_indirect(cps_impl_sig_ref, continuation_impl, &[new_base_address, continuation, last_result_ptr]);
    }

    fn simple_handler_runner_impl<M: Module>(m: &mut M, builder: &mut FunctionBuilder) {
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let base_address = builder.block_params(entry_block)[0];
        let next_continuation = builder.block_params(entry_block)[1];

        let handler_function_ptr = builder.ins().load(I64, MemFlags::new(), base_address, 0);
        let handler_index = builder.ins().load(I64, MemFlags::new(), base_address, 8);

        let inst = Self::call_built_in(m, builder, BuiltinFunction::GetTrivialContinuation, &[]);
        let trivial_continuation = builder.inst_results(inst)[0];

        let new_base_address = builder.ins().iadd_imm(base_address, 16);
        let sig = create_cps_signature(m);
        let sig_ref = builder.import_signature(sig);

        let inst = builder.ins().call_indirect(sig_ref, handler_function_ptr, &[new_base_address, trivial_continuation]);
        let result_ptr = builder.inst_results(inst)[0];
        let simple_handler_result = builder.ins().load(I64, MemFlags::new(), result_ptr, 0);
        let simple_handler_result_ptr = builder.ins().band_imm(simple_handler_result, !0b11);

        let inst = Self::call_built_in(
            m, builder, BuiltinFunction::ProcessSimpleHandlerResult,
            &[handler_index, simple_handler_result_ptr]);
        let result = builder.inst_results(inst)[0];

        let new_base_address = builder.ins().iadd_imm(result_ptr, 8);
        let next_continuation_impl_ptr = builder.ins().load(I64, MemFlags::new(), next_continuation, 0);
        builder.ins().store(MemFlags::new(), result, result_ptr, 0);
        let cps_impl_sig = create_cps_impl_signature(m);
        let cps_impl_sig_ref = builder.import_signature(cps_impl_sig);
        builder.ins().return_call_indirect(cps_impl_sig_ref, next_continuation_impl_ptr, &[new_base_address, next_continuation, result_ptr]);
    }

    fn transform_loader_cps_impl<M: Module>(m: &mut M, builder: &mut FunctionBuilder) {
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

        let inst = Self::call_built_in(m, builder, BuiltinFunction::PopHandler, &[]);
        let handler_parameter = builder.inst_results(inst)[0];

        // write handler parameter and result on stack. The first parameter is written closer to the
        // base address, so it's lower on the stack.
        builder.ins().store(MemFlags::new(), handler_parameter, base_address, -8);
        builder.ins().store(MemFlags::new(), last_result, base_address, 0);
        // set the tip address to base address + 8 so that the only argument of transform loader
        // is taken for the final tail call to the transform function.
        let tip_address = builder.ins().iadd_imm(base_address, -8);

        // push all the thunk arguments to the stack
        builder.ins().stack_store(tip_address, tip_address_slot, 0);
        let tip_address_ptr = builder.ins().stack_addr(I64, tip_address_slot, 0);
        let inst = Self::call_built_in(m, builder, BuiltinFunction::ForceThunk, &[transform_thunk, tip_address_ptr]);
        let transform_ptr = builder.inst_results(inst)[0];
        let tip_address = builder.ins().stack_load(I64, tip_address_slot, 0);

        let next_continuation = builder.ins().load(I64, MemFlags::new(), current_continuation, 16);

        // update frame height of the next continuation to account for the transform arguments
        // pushed to the stack
        let next_continuation_frame_height = builder.ins().load(I64, MemFlags::new(), next_continuation, 8);
        let next_continuation_frame_height_delta_bytes = builder.ins().isub(base_address, tip_address);
        let next_continuation_frame_height_delta = builder.ins().ushr_imm(next_continuation_frame_height_delta_bytes, 3);
        let next_continuation_frame_height = builder.ins().iadd(next_continuation_frame_height, next_continuation_frame_height_delta);
        builder.ins().store(MemFlags::new(), next_continuation_frame_height, next_continuation, 8);

        // call the next continuation
        let sig = create_cps_signature(m);
        let sig_ref = builder.import_signature(sig);
        builder.ins().return_call_indirect(sig_ref, transform_ptr, &[tip_address, next_continuation]);
    }

    fn invoke_cps_function_with_trivial_continuation<M: Module>(m: &mut M, builder: &mut FunctionBuilder) {
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let func_ptr = builder.block_params(entry_block)[0];
        let base_address = builder.block_params(entry_block)[1];

        let inst = Self::call_built_in(m, builder, BuiltinFunction::GetTrivialContinuation, &[]);
        let trivial_continuation = builder.inst_results(inst)[0];

        let sig = create_cps_signature(m);
        let sig_ref = builder.import_signature(sig);
        let inst = builder.ins().call_indirect(sig_ref, func_ptr, &[base_address, trivial_continuation]);
        let result_ptr = builder.inst_results(inst)[0];
        builder.ins().return_(&[result_ptr]);
    }

    fn call_built_in<M: Module>(m: &mut M, builder: &mut FunctionBuilder, func: BuiltinFunction, args: &[Value]) -> Inst {
        let func_id = func.declare(m).0;
        let func_ref = m.declare_func_in_func(func_id, builder.func);
        let inst = builder.ins().call(func_ref, args);
        inst
    }

    fn get_built_in_func_ptr<M: Module>(m: &mut M, builder: &mut FunctionBuilder, func: BuiltinFunction) -> Value {
        let func_id = func.declare(m).0;
        let func_ref = m.declare_func_in_func(func_id, builder.func);
        builder.ins().func_addr(I64, func_ref)
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

