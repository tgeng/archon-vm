use cbpv_runtime::runtime::HandlerType;
use crate::ast::term::{CType, Effect, VType};

#[derive(Debug, Clone, PartialEq)]
pub enum FTerm {
    Identifier { name: String, effect: Effect },
    Int { value: i64 },
    Str { value: String },
    Struct { values: Vec<FTerm> },
    Lambda { arg_names: Vec<(String, VType)>, body: Box<FTerm>, effect: Effect },
    Redex { function: Box<FTerm>, args: Vec<FTerm> },
    Force { thunk: Box<FTerm>, effect: Effect },
    Thunk { computation: Box<FTerm>, effect: Effect },
    CaseInt { t: Box<FTerm>, result_type: CType, branches: Vec<(i64, FTerm)>, default_branch: Option<Box<FTerm>> },
    MemGet { base: Box<FTerm>, offset: Box<FTerm> },
    MemSet { base: Box<FTerm>, offset: Box<FTerm>, value: Box<FTerm> },
    Let { name: String, t: Box<FTerm>, body: Box<FTerm> },
    Defs { defs: Vec<(String, Def)>, body: Option<Box<FTerm>> },
    OperationCall { eff: Box<FTerm>, args: Vec<FTerm>, effect: Effect },
    Handler {
        parameter: Box<FTerm>,
        parameter_disposer: Option<Box<FTerm>>,
        parameter_replicator: Option<Box<FTerm>>,
        transform: Box<FTerm>,
        handlers: Vec<(FTerm, FTerm, HandlerType)>,
        input: Box<FTerm>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub args: Vec<(String, VType)>,
    pub body: Box<FTerm>,
    pub c_type: CType,
}