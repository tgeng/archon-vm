type Uniform = usize;
type FuncPtr = usize;


/// A state object of a function. This also points to the caller state so it effectively represents
/// a continuation. Hence the name.
struct Continuation {
    next: *mut Continuation,
    func: *const FuncPtr,
    progress: usize,
    // local variables follows here right after this struct
}

struct Handler {
    parent: *mut Handler,
    transform_continuation: *mut Continuation,
    parameter: *mut Uniform,
    parameter_disposer: *const FuncPtr,
    parameter_replicator: *const FuncPtr,
    transform: *const FuncPtr,
    simple_handler: Vec<(Uniform, *const FuncPtr)>,
    complex_handler: Vec<(Uniform, *const FuncPtr)>,
}
