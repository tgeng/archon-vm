#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PType {
    I64,
    I32,
    F64,
    F32,
}

/// See [[cbpv_runtime::types::UniformType]] for how these are represented in uniform representation.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpecializedType {
    /// 61-bit integer
    Integer,

    /// A pointer to a struct, whose members are all in uniform representation.
    StructPtr,

    /// A pointer to an array of non-pointers. The array may contain raw functions, 64-bit integers,
    /// double precision floating point numbers, or arrays of such values. It may also point to a
    /// UTF-8 encoded string.
    PrimitivePtr,

    /// A primitive value using the highest bits represented in the standard format.
    Primitive(PType),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VType {
    /// Uniform representation of values. See [[cbpv_runtime::types::UniformType]] for details.
    /// Uniform representation is used across function boundaries (call args and return values)
    /// unless the call is specialized (aka a [CTerm::PrimitiveCall] or [CTerm::FunctionCall]).
    ///
    /// This is necessary because functions can be polymorphic (or even more expressive with large
    /// elimination where an argument can have unrelated types) and hence neither caller or receiver
    /// may know the exact type of the argument (or return value). Specialization is only possible
    /// when the caller-callee relationship is known statically, aka a [CTerm::PrimitiveCall] or
    /// [CTerm::SpecializedFunctionCall].
    Uniform,
    /// Values of specialized are represented in their "natural" form. That is, pointers are raw
    /// pointers that can be dereferenced. Integer and floats are unboxed values. This is only used
    /// inside functions or across specialized function calls (aka a [CTerm::PrimitiveCall] or
    /// [CTerm::FunctionCall]).
    Specialized(SpecializedType),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CType {
    /// Default type for computations. The computation may return a value or continue another
    /// computation (for example consuming more arguments to produce a value through a tail call).
    /// All thunks have this type to support flexible ways of calling them.
    Default,
    /// A computation that returns a value. A function having this as the return type can be
    /// specialized to be called by passing values instead of pushing values to the argument stack.
    SpecializedF(VType),
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
    Force { thunk: VTerm, may_have_complex_effects: bool },
    Let { t: Box<CTerm>, bound_index: usize, body: Box<CTerm> },
    /// Note: the flag means whether the function has complex effects that need to be handled by
    /// some handlers. System effects like IO do not count because they appear to be a simple call.
    /// This value should be conservatively set to true if side effects are unknown. For example,
    /// the containing redex may not have enough arguments to determine the effects of this
    /// computation.
    Def { name: String, may_have_complex_effects: bool },
    /// Note on result type, different branches can have different computation types. For example,
    /// for record instance or function with large elimination, one branch may return a value while
    /// another consumes more arguments (aka tail call). In this case the result type is
    /// [CType::Default].
    CaseInt { t: VTerm, result_type: CType, branches: Vec<(i64, CTerm)>, default_branch: Option<Box<CTerm>> },
    // TODO: add first class lambda and lifting
    MemGet { base: VTerm, offset: VTerm },
    MemSet { base: VTerm, offset: VTerm, value: VTerm },
    // TODO: implement the following for setting and getting primitive values
    // PMemGet { base: VTerm, offset: VTerm, p_type: PType },
    // PMemSet { base: VTerm, offset: VTerm, value: VTerm, p_type: PType },
    PrimitiveCall { name: &'static str, args: Vec<VTerm> },
    OperationCall { eff: VTerm, args: Vec<VTerm>, simple: bool },
    Handler {
        parameter: VTerm,
        /// parameter -> 0
        parameter_disposer: Box<(usize, CTerm)>,
        /// parameter -> (parameter, parameter)
        parameter_replicator: Box<(usize, CTerm)>,
        /// (parameter, input) -> output
        transform: Box<(usize, usize, CTerm)>,
        /// each handler: (effect, parameter index, operation arg indexes...,  continuation index) -> output
        complex_handlers: Vec<(VTerm, usize, Vec<usize>, usize, CTerm)>,
        /// each handler: (effect, parameter index, operation_arg indexes...) -> output
        simple_handlers: Vec<(VTerm, usize, Vec<usize>, CTerm)>,
        input: Box<CTerm>,
    },

    // Internal operations used during optimization and code generation.

    /// This can only appear inside a simple operation implementation. It means that the operation
    /// returns to the return address of the caller of the matching simple effect handler. This is
    /// used to implement simple exceptional effects, where a normal return means good call result
    /// and a long return means exceptional call result.
    LongJump { value: VTerm },

    /// Special operation created during handler component lifting. This is only used inside helper
    /// function created for handler transform component. This operation returns the handler
    /// parameter.
    PopHandler,

    /// Special operation that gets the last result of the current continuation. This is only used
    /// inside helper function created for handler transform component. The purpose is that with
    /// this operation, the transform implementation can retrieve the input value in the very first
    /// basic block.
    GetLastResult,
}