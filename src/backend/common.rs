use cbpv_runtime::runtime_utils::{runtime_alloc, runtime_force_thunk, runtime_alloc_stack};
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
    Alloc,
    ForceThunk,
    AllocStack,
}

impl BuiltinFunction {
    fn func_name(&self) -> &'static str {
        match self {
            BuiltinFunction::Alloc => "__runtime_alloc__",
            BuiltinFunction::ForceThunk => "__runtime_force_thunk__",
            BuiltinFunction::AllocStack => "__runtime_alloc_stack__",
        }
    }

    pub fn declare_symbol(&self, builder: &mut JITBuilder) {
        let func_ptr = match self {
            BuiltinFunction::Alloc => runtime_alloc as *const u8,
            BuiltinFunction::ForceThunk => runtime_force_thunk as *const u8,
            BuiltinFunction::AllocStack => runtime_alloc_stack as *const u8
        };

        builder.symbol(self.func_name(), func_ptr);
    }

    pub fn signature<M: Module>(&self, m: &mut M) -> Signature {
        let mut sig = m.make_signature();
        match self {
            BuiltinFunction::Alloc => {
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            BuiltinFunction::ForceThunk => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            BuiltinFunction::AllocStack => {
                sig.returns.push(AbiParam::new(I64));
            }
        }
        sig
    }

    pub fn declare<M: Module>(&self, m: &mut M) -> FuncId {
        let signature = &self.signature(m);
        m.declare_function(self.func_name(), Linkage::Import, signature).unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionFlavor {
    /// The function arguments are pushed via pushing to the argument stack and a continuation is
    /// passed to accept the return value. This is the default mode and all functions are compiled
    /// to this mode.
    Cps,
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
            FunctionFlavor::Simple => format!("{}__simple", function_name),
            FunctionFlavor::Specialized => format!("{}__specialized", function_name),
        }
    }
}
