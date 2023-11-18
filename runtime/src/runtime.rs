/// Data of unknown representation. This could be a primitive or pointer.
type Generic = usize;
/// Data in uniform representation. This is a tagged primitive or pointer.
type Uniform = usize;
/// A function pointer that is not tagged.
type FuncPtr = *const usize;
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
    transform_continuation: *mut Continuation,
    parameter: *mut Uniform,
    parameter_disposer: FuncPtr,
    parameter_replicator: FuncPtr,
    transform: FuncPtr,
    simple_handler: Vec<(Eff, FuncPtr)>,
    complex_handler: Vec<(Eff, FuncPtr)>,
}
