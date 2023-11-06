use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum VTerm {
    Var { index: usize },
    Thunk { t: Box<CTerm> },
    Int { value: i64 },
    Str { value: String },
    Array { values: Vec<VTerm> },
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
    Primitive { name: &'static str, arity: u8 },
}