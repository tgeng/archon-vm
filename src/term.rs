use std::collections::HashMap;

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
    CaseStr { t: VTerm, branches: HashMap<String, CTerm>, default_branch: Option<Box<CTerm>> },
    MemGet { base: VTerm, offset: VTerm },
    MemSet { base: VTerm, offset: VTerm, value: VTerm },
    // TODO: add the following for primitive accesses
    // I64Get
    // I64Set
    // I32Get
    // I32Set
    // F64Get
    // F64Set
    // F32Get
    // F32Set
    Primitive { name: &'static str },
}