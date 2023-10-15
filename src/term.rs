use std::iter::Map;

enum VTerm {
    Var { name: String },
    Thunk { t: CTerm },
    Int { value: i32 },
    Str { value: String },
    Tuple { values: Vec<VTerm> },
}

enum CTerm {
    Lambda { arg_name: String, body: CTerm },
    App { function: CTerm, arg: VTerm },
    Return { value: VTerm },
    Force { thunk: VTerm },
    Let { t: CTerm, bound_name: String, body: CTerm },
    Def { name: String },
    CaseInt { t: VTerm, branches: Map<i32, CTerm>, default_branch: Option<CTerm> },
    CaseTuple { t: VTerm, bound_names: Vec<String>, branch: CTerm },
    CaseStr { t: VTerm, branches: Map<String, CTerm>, default_branch: Option<CTerm> },
    Primitive { name: String, arity: i32 },
}