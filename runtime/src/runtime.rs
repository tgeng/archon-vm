use enum_ordinalize::Ordinalize;

/// Data of unknown representation. This could be a primitive or pointer.
pub type Generic = usize;
/// Data in uniform representation. This is a tagged primitive or pointer.
pub type Uniform = usize;
/// A pointer to a thunk. The pointer itself must be in uniform representation in order to tell it
/// apart from a raw function pointer.
pub type ThunkPtr = *const usize;
/// A raw function pointer without any uniform representation tags.
pub type RawFuncPtr = *const usize;
/// A pointer to a continuation implementation, which takes a corrresponding continuation and a
/// result value and calls the next continuation in the end.
pub type ContImplPtr = *const usize;
/// A value in uniform representation.
pub type Eff = usize;

/// 0 is exceptional, 1 is linear, 2 is affine
pub type HandlerTypeOrdinal = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Ordinalize)]
pub enum HandlerType {
    Exceptional,
    Linear,
    Affine,
    Complex,
}

/// A state object of a function. This also points to the caller state so it effectively represents
/// a continuation. Hence the name.
#[repr(C, align(8))]
pub struct Continuation {
    pub func: ContImplPtr,
    /// The number of slots taken on the argument stack by this continuation. The unit is machine
    /// word (aka 8 bytes).
    pub arg_stack_frame_height: usize,
    pub next: *mut Continuation,
    pub state: usize,
    // local variables follows from here.
}

// TODO: use custom allocator that allocates through Boehm GC for vecs
#[repr(C, align(8))]
#[derive(Clone)]
pub struct Handler<T> {
    pub transform_loader_continuation: *mut Continuation,
    pub transform_loader_base_address: T,
    pub parameter: Uniform,
    pub parameter_disposer: ThunkPtr,
    pub parameter_replicator: ThunkPtr,
    // The following three fields are set by runtime_mark_handler and used by runtime_jump
    pub stack_pointer: *const u8,
    pub frame_pointer: *const u8,
    pub return_address: *const u8,
    // There are 10 general purpose callee saved registers on ARM64
    pub general_callee_saved_registers: [usize; 10],
    // There are 8 vector callee saved registers on ARM64
    pub vector_callee_saved_registers: [u128; 8],
    pub simple_handler: Vec<(Eff, ThunkPtr, HandlerTypeOrdinal)>,
    pub complex_handler: Vec<(Eff, ThunkPtr)>,
}

pub enum HandlerEntry {
    Handler(Handler<*mut Uniform>),
    SimpleOperationMarker { handler_index: usize },
}

#[derive(Clone)]
pub struct CapturedContinuation {
    pub tip_continuation: *mut Continuation,
    pub handler_fragment: Vec<Handler<usize>>,
    /// This contains all the arguments passed to the matching handler's transform component. But
    /// it does NOT contain any arguments passed to the handler implementation. That is,
    pub stack_fragment: Vec<Generic>,
}
