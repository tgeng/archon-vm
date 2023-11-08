use std::collections::HashMap;

enum PrimitiveType {
    /// 61-bit integer. Lowest bits are 000 under uniform representation.
    Int,

    /// Pointer to some struct whose fields are all in uniform representation. Lowest bits are 001
    /// under uniform representation.
    /// The -1 word contains the byte length of this struct. This size is needed to check effect
    /// argument equality at runtime. It should be possible to leverage the information from the
    /// memory allocator to avoid storing this size in future.
    Ptr,

    /// Raw function pointer. Lowest bits are 010 under uniform representation.
    Fun,

    /// Pointer to a primitive array or string. Lowest bits are 011 under uniform representation.
    /// The -1 word contains the byte length of this array or string. This size is needed to check
    /// effect argument equality at runtime. It should be possible to leverage the information from
    /// the memory allocator to avoid storing this size in future.
    Arr,

    /// A 64-bit integer. Boxed when under uniform representation with lowest bits 100 in the
    /// pointer.
    I64,

    /// A 32-bit integer. Highest bits are used when under uniform representation, lowest bits are
    /// 101.
    I32,

    /// A 64-bit float. Boxed when under uniform representation with lowest bits 110 in the pointer.
    F64,

    /// A 32-bit float. Highest bits are used when under uniform representation, lowest bits are
    /// 111.
    F32,
}

enum VType {
    /// Uniform representation of values. See [PrimitiveType] for details.
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
    // Array { values: Vec<VTerm> },
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
    Primitive { name: &'static str, arity: u8 },
}