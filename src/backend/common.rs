use cranelift::codegen::isa::CallConv;
use cbpv_runtime::runtime_utils::{runtime_alloc, runtime_force_thunk, runtime_alloc_stack, runtime_handle_simple_operation, runtime_prepare_complex_operation, runtime_pop_handler, runtime_register_handler_and_get_transform_continuation, runtime_get_current_handler, runtime_add_simple_handler, runtime_add_complex_handler, runtime_prepare_resume_continuation};
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
    PrepareComplexOperation,
    PopHandler,
    RegisterHandlerAndGetTransformContinuation,
    GetCurrentHandler,
    AddSimpleHandler,
    AddComplexHandler,
    PrepareResumeContinuation,

    // generated
    GetTrivialContinuation,
    ConvertCapturedContinuationToThunk,
}

impl BuiltinFunction {
    fn func_name(&self) -> &'static str {
        match self {
            BuiltinFunction::Alloc => "__runtime_alloc__",
            BuiltinFunction::ForceThunk => "__runtime_force_thunk__",
            BuiltinFunction::AllocStack => "__runtime_alloc_stack__",
            BuiltinFunction::HandleSimpleOperation => "__runtime_handle_simple_operation",
            BuiltinFunction::PrepareComplexOperation => "__runtime_prepare_complex_operation",
            BuiltinFunction::PopHandler => "__runtime_pop_handler",
            BuiltinFunction::RegisterHandlerAndGetTransformContinuation => "__runtime_register_handler_and_get_transform_continuation",
            BuiltinFunction::GetCurrentHandler => "__runtime_get_current_handler",
            BuiltinFunction::AddSimpleHandler => "__runtime_add_simple_handler",
            BuiltinFunction::AddComplexHandler => "__runtime_add_complex_handler",
            BuiltinFunction::PrepareResumeContinuation => "__runtime_prepare_resume_continuation",
            BuiltinFunction::GetTrivialContinuation => "__runtime_get_trivial_continuation",
            BuiltinFunction::ConvertCapturedContinuationToThunk => "__runtime_convert_captured_continuation_to_thunk",
        }
    }

    pub fn declare_symbol(&self, builder: &mut JITBuilder) {
        let func_ptr = match self {
            BuiltinFunction::Alloc => runtime_alloc as *const u8,
            BuiltinFunction::ForceThunk => runtime_force_thunk as *const u8,
            BuiltinFunction::AllocStack => runtime_alloc_stack as *const u8,
            BuiltinFunction::HandleSimpleOperation => runtime_handle_simple_operation as *const u8,
            BuiltinFunction::PrepareComplexOperation => runtime_prepare_complex_operation as *const u8,
            BuiltinFunction::PopHandler => runtime_pop_handler as *const u8,
            BuiltinFunction::RegisterHandlerAndGetTransformContinuation => runtime_register_handler_and_get_transform_continuation as *const u8,
            BuiltinFunction::GetCurrentHandler => runtime_get_current_handler as *const u8,
            BuiltinFunction::AddSimpleHandler => runtime_add_simple_handler as *const u8,
            BuiltinFunction::AddComplexHandler => runtime_add_complex_handler as *const u8,
            BuiltinFunction::PrepareResumeContinuation => runtime_prepare_resume_continuation as *const u8,
            BuiltinFunction::GetTrivialContinuation => return,
            BuiltinFunction::ConvertCapturedContinuationToThunk => return,
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
            BuiltinFunction::HandleSimpleOperation => declare_external_func(2, 1),
            BuiltinFunction::PrepareComplexOperation => declare_external_func(3, 1),
            BuiltinFunction::PopHandler => declare_external_func(0, 1),
            BuiltinFunction::RegisterHandlerAndGetTransformContinuation => declare_external_func(8, 1),
            BuiltinFunction::GetCurrentHandler => declare_external_func(8, 1),
            BuiltinFunction::AddSimpleHandler => declare_external_func(4, 0),
            BuiltinFunction::AddComplexHandler => declare_external_func(4, 0),
            BuiltinFunction::PrepareResumeContinuation => declare_external_func(5, 1),
            BuiltinFunction::GetTrivialContinuation => {
                let mut ctx = m.make_context();
                let mut builder_context = FunctionBuilderContext::new();
                let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);

                let impl_func_id = Self::generate_trivial_continuation_impl(m, &mut builder);
                m.define_function(impl_func_id, &mut ctx).unwrap();
                m.clear_context(&mut ctx);
                let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);
                let func_id = Self::generate_trivial_continuation_helper(m, impl_func_id, &mut builder);
                m.define_function(func_id, &mut ctx).unwrap();
                func_id
            }
            BuiltinFunction::ConvertCapturedContinuationToThunk => {
                let mut ctx = m.make_context();
                let mut builder_context = FunctionBuilderContext::new();
                let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);

                let impl_func_id = Self::convert_captured_continuation_to_thunk_impl(m, &mut builder);
                m.define_function(impl_func_id, &mut ctx).unwrap();
                m.clear_context(&mut ctx);
                let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);
                let func_id = Self::convert_captured_continuation_to_thunk(m, impl_func_id, &mut builder);
                m.define_function(func_id, &mut ctx).unwrap();
                func_id
            }
        }
    }


    fn generate_trivial_continuation_impl<M: Module>(module: &mut M, builder: &mut FunctionBuilder) -> FuncId {
        let sig = create_cps_impl_signature(module);
        let func_id = module.declare_function(BuiltinFunction::GetTrivialContinuation.func_name(), Linkage::Local, &sig).unwrap();
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
        func_id
    }

    fn generate_trivial_continuation_helper<M: Module>(module: &mut M, impl_func_id: FuncId, builder: &mut FunctionBuilder) -> FuncId {
        let mut sig = module.make_signature();
        // the frame height, which is the number of arguments passed to the function accepting the
        // returned continuation object.
        sig.params.push(AbiParam::new(I64));
        // The pointer to the created continuation object.
        sig.returns.push(AbiParam::new(I64));
        let func_id = module.declare_function(BuiltinFunction::GetTrivialContinuation.func_name(), Linkage::Local, &sig).unwrap();
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let alloc_func_id = module.declare_function(BuiltinFunction::Alloc.func_name(), Linkage::Import, &sig);
        let alloc_func_ref = module.declare_func_in_func(alloc_func_id.unwrap(), builder.func);
        // We only need the first two words for the trivial continuation.
        let continuation_size = builder.ins().iconst(I64, 2);
        let inst = builder.ins().call(alloc_func_ref, &[continuation_size]);
        let continuation_ptr = builder.inst_results(inst)[0];

        // first word of continuation is the continuation implementation
        let impl_func_ref = module.declare_func_in_func(impl_func_id, builder.func);
        let impl_func_ptr = builder.ins().func_addr(I64, impl_func_ref);
        builder.ins().store(MemFlags::new(), impl_func_ptr, continuation_ptr, 0);

        // second word is the frame height, which should be the given parameter
        let frame_height = builder.block_params(entry_block)[0];
        builder.ins().store(MemFlags::new(), frame_height, continuation_ptr, 8);

        // return the pointer to the continuation object
        builder.ins().return_(&[continuation_ptr]);
        func_id
    }

    fn convert_captured_continuation_to_thunk_impl<M: Module>(module: &mut M, builder: &mut FunctionBuilder) -> FuncId {
        todo!()
    }

    fn convert_captured_continuation_to_thunk<M: Module>(module: &mut M, impl_func_id: FuncId, builder: &mut FunctionBuilder) -> FuncId {
        todo!()
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
            FunctionFlavor::Cps => function_name.to_owned(),
            FunctionFlavor::CpsImpl => format!("{}__cps_impl", function_name),
            FunctionFlavor::Simple => format!("{}__simple", function_name),
            FunctionFlavor::Specialized => format!("{}__specialized", function_name),
        }
    }
}

pub fn create_cps_impl_signature<M: Module>(module: &M) -> Signature {
    let mut uniform_cps_impl_func_signature = module.make_signature();
    uniform_cps_impl_func_signature.params.push(AbiParam::new(I64)); // base address
    uniform_cps_impl_func_signature.params.push(AbiParam::new(I64)); // the continuation object
    uniform_cps_impl_func_signature.params.push(AbiParam::new(I64)); // the last result
    uniform_cps_impl_func_signature.returns.push(AbiParam::new(I64));
    uniform_cps_impl_func_signature.call_conv = CallConv::Tail;
    uniform_cps_impl_func_signature
}
