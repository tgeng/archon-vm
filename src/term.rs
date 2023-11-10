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
    ///
    /// This is necessary because functions can be polymorphic (or even more expressive with large
    /// elimination where an argument can have unrelated types) and hence neither caller or receiver
    /// may know the exact type of the argument (or return value). Specialization is only possible
    /// when the caller-callee relationship is known statically, aka a [CTerm::PrimitiveCall] or
    /// [CTerm::SpecializedFunctionCall].
    Uniform,
    /// Values of specialized are represented in their "natural" form. That is, pointers are raw
    /// pointers that can be dereferenced. Integer and floats are unboxed values. This is only used
    /// inside functions or across specialized function calls (aka a [CTerm::PrimitiveCall] or
    /// [CTerm::FunctionCall]).
    Specialized(PrimitiveType),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CType {
    /// Default type for computations. The computation may return a value or continue another
    /// computation (for example consuming more arguments to produce a value through a tail call).
    /// All thunks have this type to support flexible ways of calling them.
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
    Let { t: Box<CTerm>, bound_index: usize, body: Box<CTerm> },
    Def { name: String },
    /// Note on result type, different branches can have different computation types. For example,
    /// for record instance or function with large elimination, one branch may return a value while
    /// another consumes more arguments (aka tail call). In this case the result type is
    /// [CType::Uniform].
    CaseInt { t: VTerm, result_type: CType, branches: HashMap<i64, CTerm>, default_branch: Option<Box<CTerm>> },
    MemGet { base: VTerm, offset: VTerm },
    MemSet { base: VTerm, offset: VTerm, value: VTerm },
    // TODO: implement the following for setting and getting primitive values
    // PMemGet { base: VTerm, offset: VTerm, p_type: PType },
    // PMemSet { base: VTerm, offset: VTerm, value: VTerm, p_type: PType },
    PrimitiveCall { name: &'static str, args: Vec<VTerm> },
    SpecializedFunctionCall { name: String, args: Vec<VTerm> },
}