use cbpv_runtime::runtime::HandlerType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PType {
    I64,
    I32,
    F64,
    F32,
}

/// See [[cbpv_runtime::types::UniformType]] for how these are represented in uniform representation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CType {
    /// Default type for computations. The computation may return a value or continue another
    /// computation (for example consuming more arguments to produce a value through a tail call).
    /// All thunks have this type to support flexible ways of calling them.
    Default,
    /// A computation that returns a value. A function having this as the return type can be
    /// specialized to be called by passing values instead of pushing values to the argument stack.
    SpecializedF(VType),
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum Effect {
    /// A computation that may only have simple effects and hence the function call can be compiled via CBPV convention
    /// or is specialized.
    Simple,
    /// A computation that may have complex effects and hence the function call must be compiled via CPS transformation.
    Complex,
}

impl Effect {
    pub fn union(self, other: Effect) -> Effect {
        match (self, other) {
            (Effect::Complex, _) | (_, Effect::Complex) => Effect::Complex,
            _ => Effect::Simple,
        }
    }
    pub fn intersect(self, other: Effect) -> Effect {
        match (self, other) {
            (Effect::Simple, _) | (_, Effect::Simple) => Effect::Simple,
            _ => Effect::Complex,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VTerm {
    Var { index: usize },
    Thunk { t: Box<CTerm>, effect: Effect },
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CTerm {
    Redex { function: Box<CTerm>, args: Vec<VTerm> },
    Return { value: VTerm },
    Force { thunk: VTerm, effect: Effect },
    Let { t: Box<CTerm>, bound_index: usize, body: Box<CTerm> },
    /// Note: the flag means whether the function has complex effects that need to be handled by
    /// some handlers. System effects like IO do not count because they appear to be a simple call.
    /// This value should be conservatively set to true if side effects are unknown. For example,
    /// the containing redex may not have enough arguments to determine the effects of this
    /// computation.
    Def { name: String, effect: Effect },
    /// Note on result type, different branches can have different computation types. For example,
    /// for record instance or function with large elimination, one branch may return a value while
    /// another consumes more arguments (aka tail call). In this case the result type is
    /// [CType::Default].
    CaseInt { t: VTerm, result_type: CType, branches: Vec<(i64, CTerm)>, default_branch: Option<Box<CTerm>> },
    Lambda { args: Vec<(usize, VType)>, body: Box<CTerm>, effect: Effect },
    MemGet { base: VTerm, offset: VTerm },
    MemSet { base: VTerm, offset: VTerm, value: VTerm },
    // TODO: implement the following for setting and getting primitive values
    // PMemGet { base: VTerm, offset: VTerm, p_type: PType },
    // PMemSet { base: VTerm, offset: VTerm, value: VTerm, p_type: PType },
    PrimitiveCall { name: &'static str, args: Vec<VTerm> },
    /// Note: effect cannot be pure for operation calls.
    OperationCall { eff: VTerm, args: Vec<VTerm>, effect: Effect },
    Handler {
        parameter: VTerm,
        /// thunk of lambda: parameter -> 0
        parameter_disposer: Option<VTerm>,
        /// thunk of lambda: parameter -> (parameter, parameter)
        parameter_replicator: Option<VTerm>,
        /// thunk of lambda: (parameter, input) -> output
        transform: VTerm,
        /// each linear handler: (effect, thunk of lambda: (parameter index, operation_arg indexes...) -> (param, result type))
        /// each exceptional handler: (effect, thunk of lambda: (parameter index, operation_arg indexes...) -> (param, output type))
        /// each affine handler: (effect, thunk of lambda: (parameter index, operation_arg indexes...) -> (param, (tag, output type | result type)))
        /// each compex handler: (effect, thunk of lambda: (parameter index, operation arg indexes...,  continuation index) -> output)
        handlers: Vec<(VTerm, VTerm, HandlerType)>,
        /// A thunk that returns some value. A thunk is used because this would leverage the thunk
        /// lifting logic to lift this logic to a top level function, which is necessary to tuck in
        /// the transform continuation between this input and the parent term of this handler.
        input: VTerm,
    },
}