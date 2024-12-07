use enum_ordinalize::Ordinalize;
use std::cell::RefCell;
use std::rc::Rc;

/// Data of unknown representation. This could be a primitive or pointer.
pub type Generic = usize;
/// Data in uniform representation. This is a tagged primitive or pointer.
pub type Uniform = usize;
/// A pointer to a thunk. The pointer itself must be in uniform representation in order to tell it
/// apart from a raw function pointer.
pub type ThunkPtr = *const usize;
/// A raw function pointer without any uniform representation tags.
pub type RawFuncPtr = *const usize;
/// A pointer to a continuation implementation, which takes a corresponding continuation and a
/// result value and calls the next continuation in the end.
pub type ContImplPtr = *const usize;
/// A value in uniform representation.
#[repr(C, align(8))]
pub struct EffIns {
    pub handler: *mut Handler<*mut Uniform>,
    pub ops_offset: usize,
}

/// 0 is exceptional, 1 is linear, 2 is affine, 3 is complex
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
    pub parent_handler: Option<Rc<RefCell<Handler<T>>>>,
    pub transform_loader_continuation: *mut Continuation,
    /// An alternative is to use a Vec<Uniform> to store the argument stack inside the handler
    /// instead of having a per-thread argument stack. That way, the per-handler stack would store
    /// arguments between this handler and the immediate sub-handler. The major downside of this
    /// approach is that through the function calls, we will have to pass the pointer to the vector
    /// and all argument access needs to be done indirectly through vector access. Pointers directly
    /// to the element in the vector must be prohibited because they become invalid after the vector
    /// extends its capacity.
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
    pub handlers: Vec<(ThunkPtr, HandlerTypeOrdinal)>,
}

#[derive(Clone)]
pub struct CapturedContinuation {
    pub tip_continuation: *mut Continuation,
    pub base_handler: *mut Handler<*mut Uniform>,
    pub tip_handler: Rc<RefCell<Handler<*mut Uniform>>>,
    /// This contains all the arguments passed to the matching handler's transform component. But
    /// it does NOT contain any arguments passed to the handler implementation. That is,
    pub stack_fragment: Vec<Uniform>,
    /// Parameters and transform loader continuations corresponding to the captured handlers. If not
    /// set, those values in the handlers are used as they are. If set, these values will overwrite
    /// those in the handlers. The first element is for the tip handler.
    /// The purpose of this is that if a captured continuation is replicated, the handlers will be
    /// reused, and hence the runtime must reset the stateful pieces when a (replicated) captured
    /// continuation is unpacked.
    pub overwrites: Option<Vec<(Uniform, *mut Continuation)>>,
}
