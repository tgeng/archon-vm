/// Data of unknown representation. This could be a primitive or pointer.
type Generic = usize;
/// Data in uniform representation. This is a tagged primitive or pointer.
type Uniform = usize;
type ThunkPtr = *const usize;
/// A pointer to a continuation implementation, which takes a corrresponding continuation and a
/// result value and calls the next continuation in the end.
type ContImplPtr = *const usize;
/// A pointer to a struct consisting of the effect name and all the arguments to the effect.
type Eff = *const usize;


/// A state object of a function. This also points to the caller state so it effectively represents
/// a continuation. Hence the name.
struct Continuation {
    next: *mut Continuation,
    func: ContImplPtr,
    state: usize,
    local_vars: *mut [Generic],
}

struct Handler {
    parent: *mut Handler,
    /// The return address of the function that created this handler. Returning to this address
    /// means jumping out of the handler. So the value returned should match the output type of this
    /// handler.
    return_address: *const usize,
    frame_pointer: *const usize,
    transform_continuation: *mut Continuation,
    parameter: *mut Uniform,
    parameter_disposer: ThunkPtr,
    parameter_replicator: ThunkPtr,
    transform: ThunkPtr,
    simple_handler: Vec<(Eff, ThunkPtr)>,
    complex_handler: Vec<(Eff, ThunkPtr)>,
}
