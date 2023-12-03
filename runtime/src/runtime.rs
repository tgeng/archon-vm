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


/// A state object of a function. This also points to the caller state so it effectively represents
/// a continuation. Hence the name.
#[repr(C, align(8))]
pub struct Continuation {
    pub func: ContImplPtr,
    /// The number of slots taken on the argument stack by this continuation.
    pub arg_stack_frame_height: usize,
    pub next: *mut Continuation,
    pub state: usize,
    // local variables follows from here.
}

// TODO: use custom allocator that allocates through Boehm GC for vecs
#[repr(C, align(8))]
pub struct Handler<T> {
    pub transform_loader_continuation: *mut Continuation,
    pub transform_loader_base_address: T,
    pub transform_loader_num_args: usize,
    pub parameter: Uniform,
    pub parameter_disposer: ThunkPtr,
    pub parameter_replicator: ThunkPtr,
    pub simple_handler: Vec<(Eff, ThunkPtr)>,
    pub complex_handler: Vec<(Eff, ThunkPtr)>,
}

pub enum HandlerEntry {
    Handler(Handler<*const usize>),
    SimpleOperationMarker { handler_index: usize },
}

pub struct CapturedContinuation {
    pub tip_continuation: *mut Continuation,
    /// The base continuation is the continuation created from the matching handler's transform
    /// component.
    pub base_continuation: *mut Continuation,
    pub handler_fragment: Vec<Handler<usize>>,
    /// This contains all the arguments passed to the matching handler's transform component. But
    /// it does NOT contain any arguments passed to the handler implementation. That is,
    pub stack_fragment: Vec<Generic>,
}
