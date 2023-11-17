use crate::ast::term::{CType, VType};

#[derive(Debug, Clone, PartialEq)]
pub enum FTerm {
    Identifier { name: String },
    Int { value: i64 },
    Str { value: String },
    Struct { values: Vec<FTerm> },
    Lambda { arg_names: Vec<(String, VType)>, body: Box<FTerm> },
    Redex { function: Box<FTerm>, args: Vec<FTerm> },
    Force { thunk: Box<FTerm> },
    Thunk { computation: Box<FTerm> },
    CaseInt { t: Box<FTerm>, result_type: CType, branches: Vec<(i64, FTerm)>, default_branch: Option<Box<FTerm>> },
    MemGet { base: Box<FTerm>, offset: Box<FTerm> },
    MemSet { base: Box<FTerm>, offset: Box<FTerm>, value: Box<FTerm> },
    Let { name: String, t: Box<FTerm>, body: Box<FTerm> },
    Defs { defs: Vec<(String, Def)>, body: Option<Box<FTerm>> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub args: Vec<(String, VType)>,
    pub body: Box<FTerm>,
    pub c_type: CType,
}