use std::collections::HashMap;

enum VType {
    /// Any type, a unified representation through using bit tags can represent any values.
    /// Specifically,
    /// * 51-bit integers. The highest 13 bits must all be 1. That is, we are leveraging the unused
    ///   bits of IEEE754 double −∞)
    /// * Ptr has the lowest 48 bits as the address and the highest 14 bits like 0111 1111 1111 11.
    ///   That is, we are leveraging the unused bits of IEEE754 double NaN. Pointers are always
    ///   aligned to 8 bytes (aka the lowest 3 bits are always 0).
    /// * FPtr is just like Ptr except the lowest 3 bits are 001
    /// * TPtr is just like Ptr except the lowest 3 bits are 010
    /// * SPtr is just like Ptr except the lowest 3 bits are 011
    /// * APtr is just like Ptr except the lowest 3 bits are 100
    /// * F64 is represented as it is.
    /// * I64 is represented as a pointer to a boxed integer where the highest 14 bits are 0111 1111
    ///   1111 01.
    /// The uniform representation is used inside structs, thunks and function input and output.
    /// Arrays, on the other hand, have additional tagging bits to represent the element type and
    /// length.
    /// TODO: it's possible to have functions using specialized types for better performance. It
    ///  probably makes sense to do this when we have specialized functions whose arguments are not
    ///  passed through the argument stack.
    Any,
    /// A 51-bit integer represented as a machine word (64 bit), the highest 13 bits must be
    /// sign-extended.
    Int,
    /// A pointer to struct. Note that the highest 14 bits are all 0 for
    /// proper dereferencing, unlike the uniform representation
    Ptr,
    /// A pointer to a raw function. Note that the highest 14 bits and the lowest 3 bits are 0 for
    /// proper dereferencing, unlike the uniform representation
    FPtr,
    /// A pointer to a thunk. Note that the highest 14 bits and the lowest 3 bits are 0 for
    /// proper dereferencing, unlike the uniform representation
    TPtr,
    /// A pointer to a string. Note that the highest 14 bits and the lowest 3 bits are 0 for
    /// proper dereferencing, unlike the uniform representation
    SPtr,
    // TODO: the following types are not yet implemented in the AST
    /// A pointer to a thunk. Note that the highest 14 bits and the lowest 3 bits are 0 for
    /// proper dereferencing, unlike the uniform representation
    APtr,
    /// A 64-bit floating point number represented as a machine word (64 bit).
    F64,
    /// A 64-bit integer represented as a machine word
    I64,
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
    // Array { values: Vec<VTerm> },
    // F64 { value: f64 },
    // I64 { value: i64 },
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
    Primitive { name: &'static str, arity: u8 },
}