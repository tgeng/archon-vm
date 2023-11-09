use std::collections::HashMap;

use cranelift::prelude::Type;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimitiveType {
    Integer,
    StructPtr,
    PrimitivePtr,
    Primitive(Type),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VType {
    /// Uniform representation of values. See [[cbpv_runtime::types::UniformType]] for details.
    Uniform,
    // TODO: it's possible to have functions using specialized types for better performance. It
    //  probably makes sense to do this when we have specialized functions whose arguments are not
    //  passed through the argument stack.
    /// Values of specialized are represented in their "natural" form. That is, pointers are raw
    /// pointers that can be dereferenced. Integer and floats are unboxed values.
    Special(PrimitiveType),
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
    CaseInt { t: VTerm, branches: HashMap<i64, CTerm>, default_branch: Option<Box<CTerm>> },
    // TODO: add data type to mem get and set
    MemGet { base: VTerm, offset: VTerm },
    MemSet { base: VTerm, offset: VTerm, value: VTerm },
    Primitive { name: &'static str },
}