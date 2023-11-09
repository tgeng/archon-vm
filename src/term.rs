use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PType {
    I64,
    I32,
    F64,
    F32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimitiveType {
    Integer,
    StructPtr,
    PrimitivePtr,
    Primitive(PType),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VType {
    /// Uniform representation of values. See [[cbpv_runtime::types::UniformType]] for details.
    /// Uniform representation is used across function boundaries (call args and return values)
    /// unless the call is specialized (aka a [CTerm::PrimitiveCall] or [CTerm::FunctionCall]).
    Uniform,
    /// Values of specialized are represented in their "natural" form. That is, pointers are raw
    /// pointers that can be dereferenced. Integer and floats are unboxed values. This is only used
    /// inside functions or across specialized function calls (aka a [CTerm::PrimitiveCall] or
    /// [CTerm::FunctionCall]).
    Specialized(PrimitiveType),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CType {
    /// Default type for computations. The computation may return a value or consumes more arguments
    /// to produce a value (aka a tail call). All thunks have this type to support flexible ways of
    /// calling them.
    Uniform,
    /// A computation that returns a value. A function having this as the return type can be
    /// specialized to be called by passing values instead of pushing values to the argument stack.
    SpecializedF(VType),
}

#[derive(Debug, Clone)]
pub enum VTerm {
    Var { index: usize },
    Thunk { t: Box<CTerm> },
    /// 51 bit integer represented as a machine word with highest bits sign-extended
    Int { value: i64 },
    Str { value: String },
    Struct { values: Vec<VTerm> },
    // TODO: the following types are not yet implemented in the AST yet
    // PrimitiveArray { values: Vec<VTerm> },
    // F64 { value: f64 },
    // F32 { value: f32 },
    // I64 { value: i64 },
    // I32 { value: i32 },
}

#[derive(Debug, Clone)]
pub enum CTerm {
    Redex { function: Box<CTerm>, args: Vec<VTerm> },
    Return { value: VTerm },
    Force { thunk: VTerm },
    Let { t: Box<CTerm>, bound_type: VType, bound_index: usize, body: Box<CTerm> },
    Def { name: String },
    CaseInt { t: VTerm, branches: HashMap<i64, CTerm>, default_branch: Option<Box<CTerm>> },
    MemGet { base: VTerm, offset: VTerm },
    MemSet { base: VTerm, offset: VTerm, value: VTerm },
    // TODO: implement the following for setting and getting primitive values
    // PMemGet { base: VTerm, offset: VTerm, p_type: PType },
    // PMemSet { base: VTerm, offset: VTerm, value: VTerm, p_type: PType },
    PrimitiveCall { name: &'static str, args: Vec<VTerm> },
    SpecializedFunctionCall { name: String, args: Vec<VTerm> },
}