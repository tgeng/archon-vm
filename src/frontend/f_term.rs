use crate::ast::term::{CType, VType};

#[derive(Debug, Clone, PartialEq)]
pub enum FTerm {
    Identifier { name: String, may_have_complex_effects: bool },
    Int { value: i64 },
    Str { value: String },
    Struct { values: Vec<FTerm> },
    Lambda { arg_names: Vec<(String, VType)>, body: Box<FTerm>, may_have_complex_effects: bool },
    Redex { function: Box<FTerm>, args: Vec<FTerm> },
    Force { thunk: Box<FTerm>, may_have_complex_effects: bool },
    Thunk { computation: Box<FTerm>, may_have_complex_effects: bool },
    CaseInt { t: Box<FTerm>, result_type: CType, branches: Vec<(i64, FTerm)>, default_branch: Option<Box<FTerm>> },
    MemGet { base: Box<FTerm>, offset: Box<FTerm> },
    MemSet { base: Box<FTerm>, offset: Box<FTerm>, value: Box<FTerm> },
    Let { name: String, t: Box<FTerm>, body: Box<FTerm> },
    Defs { defs: Vec<(String, Def)>, body: Option<Box<FTerm>> },
    OperationCall { eff: Box<FTerm>, args: Vec<FTerm>, simple: bool },
    Handler {
        parameter: Box<FTerm>,
        parameter_disposer: Box<FTerm>,
        parameter_replicator: Box<FTerm>,
        transform: Box<FTerm>,
        simple_handlers: Vec<(Box<FTerm>, Box<FTerm>)>,
        complex_handlers: Vec<(Box<FTerm>, Box<FTerm>)>,
        input: Box<FTerm>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub args: Vec<(String, VType)>,
    pub body: Box<FTerm>,
    pub c_type: CType,
}