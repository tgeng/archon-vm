/// Data of unknown representation. This could be a primitive or pointer.
type Generic = usize;
/// Data in uniform representation. This is a tagged primitive or pointer.
type Uniform = usize;
type ThunkPtr = *const usize;
/// A pointer to a continuation implementation, which takes a corrresponding continuation and a
/// result value and calls the next continuation in the end.
type ContImplPtr = *const usize;
/// A pointer to a struct consisting of the effect struct
type Eff = *const usize;


/// A state object of a function. This also points to the caller state so it effectively represents
/// a continuation. Hence the name.
#[repr(C, align(8))]
struct Continuation {
    func: ContImplPtr,
    next: *mut Continuation,
    /// The number of slots taken on the argument stack by this continuation.
    arg_stack_frame_height: usize,
    state: usize,
    local_vars: [Generic],
}

struct Handler {
    transform_continuation: *mut Continuation,
    parameter: *mut Uniform,
    parameter_disposer: ThunkPtr,
    parameter_replicator: ThunkPtr,
    transform: ThunkPtr,
    simple_handler: *const [(Eff, ThunkPtr)],
    complex_handler: *const [(Eff, ThunkPtr)],
}

enum HandlerEntry {
    Handler(Handler),
    SimpleOperationMarker { handler_index: usize },
}
