use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum VTerm {
    Var { name: String },
    Thunk { t: Box<CTerm> },
    Int { value: i32 },
    Str { value: String },
    Tuple { values: Vec<VTerm> },
}

#[derive(Debug, Clone)]
pub enum CTerm {
    App { function: Box<CTerm>, arg: VTerm },
    Return { value: VTerm },
    Force { thunk: VTerm },
    Let { t: Box<CTerm>, bound_name: String, body: Box<CTerm> },
    Def { name: String },
    CaseInt { t: VTerm, branches: HashMap<i32, CTerm>, default_branch: Option<Box<CTerm>> },
    CaseTuple { t: VTerm, bound_names: Vec<String>, branch: Box<CTerm> },
    CaseStr { t: VTerm, branches: HashMap<String, CTerm>, default_branch: Option<Box<CTerm>> },
    Primitive { name: &'static str, arity: u8 },
}