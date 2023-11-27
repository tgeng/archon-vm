/// Data of unknown representation. This could be a primitive or pointer.
pub type Generic = usize;
/// Data in uniform representation. This is a tagged primitive or pointer.
pub type Uniform = usize;
pub type ThunkPtr = *const usize;
/// A pointer to a continuation implementation, which takes a corrresponding continuation and a
/// result value and calls the next continuation in the end.
pub type ContImplPtr = *const usize;
/// A pointer to a struct consisting of the effect struct
pub type Eff = usize;


/// A state object of a function. This also points to the caller state so it effectively represents
/// a continuation. Hence the name.
#[repr(C, align(8))]
pub struct Continuation {
    pub func: ContImplPtr,
    pub next: *mut Continuation,
    /// The number of slots taken on the argument stack by this continuation.
    pub arg_stack_frame_height: usize,
    pub state: usize,
    // local variables follows from here.
}

// TODO: use custom allocator that allocates through Boehm GC for vecs
pub struct Handler {
    pub transform_continuation: *mut Continuation,
    pub transform_num_args: usize,
    pub parameter: Uniform,
    pub parameter_disposer: ThunkPtr,
    pub parameter_replicator: ThunkPtr,
    pub simple_handler: Vec<(Eff, ThunkPtr, usize)>,
    pub complex_handler: Vec<(Eff, ThunkPtr, usize)>,
}

pub enum HandlerEntry {
    Handler(Handler),
    SimpleOperationMarker { handler_index: usize },
}

pub struct CapturedContinuation {
    /// The number of arguments passed to the complex operation handler that creates this captured
    /// continuation. This is used to determine where the result of the operation is stored, which
    /// will be passed as the "last_result" of the tip continuation.
    pub tip_operation_num_args: usize,
    pub tip: *mut Continuation,
    pub base: *mut Continuation,
    pub handler_fragment: Vec<HandlerEntry>,
    pub stack_fragment: Vec<Generic>,
}
