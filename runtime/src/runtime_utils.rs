use enum_ordinalize::Ordinalize;
use std::arch::global_asm;
use std::cell::RefCell;
use std::iter::Peekable;
use std::ops::{DerefMut};
use std::slice::IterMut;
use crate::runtime::{Continuation, HandlerEntry, Handler, Uniform, ThunkPtr, Eff, Generic, CapturedContinuation, RawFuncPtr, ContImplPtr, HandlerTypeOrdinal, HandlerType};
use crate::runtime::HandlerEntry::SimpleOperationMarker;
use crate::types::{UPtr, UniformPtr, UniformType};

// TODO: use custom allocator that allocates through Boehm GC for vecs
thread_local!(
    static HANDLERS: RefCell<Vec<HandlerEntry>> = RefCell::new(Vec::new());
);

#[cfg(target_arch = "aarch64")]
global_asm!(r#"
    .global _runtime_mark_handler
    .global _long_jump

    _runtime_mark_handler:
        add x16, x5, #40 ; get sp address in the handler
        mov x17, sp
        str x17, [x16], #8 ; store sp to the handler and bump the address

        ; store fp (x29) and lr (x30) to the handler
        stp x29, x30, [x16], #16

        ; store the general purpose callee-saved registers
        stp x19, x20, [x16], #16
        stp x21, x22, [x16], #16
        stp x23, x24, [x16], #16
        stp x25, x26, [x16], #16
        stp x27, x28, [x16], #16

        ; store the float/simd callee-saved registers
        stp q8,  q9,  [x16], #32
        stp q10, q11, [x16], #32
        stp q12, q13, [x16], #32
        stp q14, q15, [x16], #32

        br  x4

    _long_jump:
        add x16, x3, #40 ; get sp address in the handler
        ldr x17, [x16], #8 ; restore sp from the handler
        mov sp, x17

        ; restore fp and lr from the handler
        ldp x29, x30, [x16], #16

        ; restore the general purpose callee-saved registers
        ldp x19, x20, [x16], #16
        ldp x21, x22, [x16], #16
        ldp x23, x24, [x16], #16
        ldp x25, x26, [x16], #16
        ldp x27, x28, [x16], #16

        ; restore the vector callee-saved registers
        ldp q8,  q9,  [x16], #32
        ldp q10, q11, [x16], #32
        ldp q12, q13, [x16], #32
        ldp q14, q15, [x16], #32

        ldr x16, [x1] ; get the function pointer to the next continuation

        mov x4, x2 ; shift arguments to match Cranelift's tail call convention
        mov x3, x1
        mov x2, x0
        br x16
"#);

extern "C" {
    /// Store FP, SP, and return address to the handler entry, then invoke the input function.
    /// This function follows Cranelift's tail call convention. It also invokes the given input_func_ptr, which is also
    /// in Cranelifts' tail call convention.
    /// On ARM64, the first argument starts at x2.
    pub fn runtime_mark_handler(
        input_base_address: *mut Uniform, // x2
        next_continuation: *mut Continuation, // x3
        input_func_ptr: RawFuncPtr, // x4
        handler: *const Handler<*mut Uniform>, // x5
    ) -> *const Uniform;

    /// Restore FP and SP, then jump to the return address.
    /// This function follows the normal call convention. But it ends up calling the next continuation, which is in
    /// Cranelift's tail call convention. So on ARM64, it needs to shift the arguments.
    fn long_jump(
        next_base_address: *mut Uniform, // x0
        next_continuation: *const Continuation, // x1
        result_ptr: *const Uniform, // x2
        handler: *const Handler<*mut Uniform>, // x3
    ) -> !;
}


static EMPTY_STRUCT: usize = 0;
const TRANSFORM_LOADER_NUM_ARGS: usize = 1;

unsafe fn empty_struct_ptr() -> *const usize {
    (&EMPTY_STRUCT as *const usize).offset(1)
}

#[no_mangle]
pub unsafe extern "C" fn runtime_alloc(num_words: usize) -> *mut usize {
    if num_words == 0 {
        return empty_struct_ptr() as *mut usize;
    }
    let mut vec = Vec::with_capacity(num_words + 1);
    let ptr: *mut usize = vec.as_mut_ptr();
    // Write the size of this allocation
    ptr.write(num_words);
    std::mem::forget(vec);
    ptr.add(1)
}

#[no_mangle]
pub unsafe extern "C" fn runtime_word_box() -> *mut usize {
    let mut vec = Vec::with_capacity(1);
    let ptr = vec.as_mut_ptr();
    std::mem::forget(vec);
    ptr
}


/// Takes a pointer to a function or thunk, push any arguments to the tip of the stack, and return
/// a pointer to the underlying raw function.
#[no_mangle]
pub unsafe extern "C" fn runtime_force_thunk(thunk: ThunkPtr, tip_address_ptr: *mut *mut usize) -> RawFuncPtr {
    let thunk_ptr = thunk.to_normal_ptr();
    match UniformType::from_bits(thunk as usize) {
        UniformType::PPtr => thunk_ptr,
        UniformType::SPtr => {
            let next_thunk = thunk_ptr.read() as ThunkPtr;
            let num_args = thunk_ptr.add(1).read();
            let mut tip_address = tip_address_ptr.read();
            for i in (0..num_args).rev() {
                let arg = thunk_ptr.add(2 + i).read();
                tip_address = tip_address.sub(1);
                tip_address.write(arg);
            }
            tip_address_ptr.write(tip_address);
            runtime_force_thunk(next_thunk, tip_address_ptr)
        }
        _ => unreachable!("bad thunk pointer")
    }
}

/// Alocate
#[no_mangle]
pub unsafe extern "C" fn runtime_alloc_stack() -> *mut usize {
    // Allocate a 1M words for the stack, which is 8MB of space
    let stack_size = 1 << 20;
    let mut vec: Vec<usize> = Vec::with_capacity(stack_size);
    let start: *mut usize = vec.as_mut_ptr();
    std::mem::forget(vec);
    let stack_end = start.add(stack_size);
    // write magic word for debugging
    let last_word = stack_end.sub(1);
    last_word.write(0xDEADBEEFEFBEADDE);
    last_word
}

/// Returns the result of the operation in uniform representation
pub unsafe fn debug_helper(base: *const usize, last_result_ptr: *const usize, thunk: *const usize) -> usize {
    return 1 + 1;
}


/// Returns the following results on the argument stack.
/// - ptr + 0: the function pointer to the matched handler implementationon
/// - ptr + 8: the base address used to find the arguments when invoking the handler implementation
/// - ptr + 16: the next continuation after handler finishes execution
///
#[no_mangle]
pub unsafe extern "C" fn runtime_prepare_operation(
    eff: Uniform,
    handler_call_base_address: *mut usize,
    tip_continuation: &mut Continuation,
    handler_num_args: usize,
    captured_continuation_thunk_impl: RawFuncPtr,
    simple_handler_runner_impl_ptr: RawFuncPtr,
    may_be_complex: usize,
) -> *const usize {
    let (handler_index, handler_impl, handler_type) = find_matching_handler(eff, may_be_complex);
    let (handler_function_ptr, new_base_address, next_continuation) =
        if handler_type == HandlerType::Complex {
            prepare_complex_operation(
                handler_call_base_address,
                tip_continuation,
                handler_num_args,
                captured_continuation_thunk_impl,
                handler_index,
                handler_impl,
            )
        } else {
            prepare_simple_operation(
                handler_call_base_address,
                tip_continuation,
                simple_handler_runner_impl_ptr,
                handler_index,
                handler_impl,
                handler_type,
            )
        };
    let result_ptr = new_base_address.sub(4);
    result_ptr.write(handler_function_ptr as usize);
    result_ptr.add(1).write(new_base_address as usize);
    result_ptr.add(2).write(next_continuation as usize);
    result_ptr
}

unsafe fn prepare_complex_operation(
    handler_call_base_address: *const usize,
    tip_continuation: &mut Continuation,
    handler_num_args: usize,
    captured_continuation_thunk_impl: RawFuncPtr,
    handler_index: usize,
    handler_impl: ThunkPtr,
) -> (*const usize, *mut usize, *mut Continuation) {
    let handler_entry_fragment = HANDLERS.with(|handler| handler.borrow_mut().split_off(handler_index));
    let matching_handler = match handler_entry_fragment.first().unwrap() {
        HandlerEntry::Handler(handler) => handler,
        _ => panic!("Expect a handler entry")
    };

    // Update the tip continuation so that its height no longer includes the arguments passed to
    // the handler because the captured continuation won't include them in the stack fragment. Later
    // when the captured continuation is resumed, the tip (tip - 8) of the argument stack will be
    // where the operation result is placed.
    tip_continuation.arg_stack_frame_height -= handler_num_args;

    let matching_parameter = matching_handler.parameter;
    // Split the continuation chain at the matching handler.
    let base_continuation = matching_handler.transform_loader_continuation;
    let next_continuation = (*base_continuation).next;
    // Tie up the captured continuation end.
    (*base_continuation).next = std::ptr::null_mut::<Continuation>();

    // Update the next continuation to make it ready for calling the handler implementation
    // Plus 2 for handler parameter and reified continuation
    (*next_continuation).arg_stack_frame_height += handler_num_args + 2 - TRANSFORM_LOADER_NUM_ARGS;

    // Copy the stack fragment.
    let stack_fragment_end = matching_handler.transform_loader_base_address.add(TRANSFORM_LOADER_NUM_ARGS);
    let stack_fragment_start = handler_call_base_address.add(handler_num_args);
    let stack_fragment_length = stack_fragment_end.offset_from(stack_fragment_start);
    assert!(stack_fragment_length >= 0);
    let stack_fragment: Vec<Generic> = std::slice::from_raw_parts(stack_fragment_start, stack_fragment_length as usize).to_vec();

    // Copy the handler fragment.
    let matching_base_address = matching_handler.transform_loader_base_address;
    let handler_fragment: Vec<Handler<usize>> = handler_entry_fragment.into_iter().map(|handler_entry| {
        match handler_entry {
            HandlerEntry::Handler(handler) => {
                let transform_base_address = handler.transform_loader_base_address;
                let mut handler: Handler<usize> = std::mem::transmute(handler);
                handler.transform_loader_base_address = matching_base_address.offset_from(transform_base_address) as usize;
                handler
            }
            _ => panic!("Expect a handler entry")
        }
    }).collect();

    let captured_continuation_thunk = create_captured_continuation(captured_continuation_thunk_impl, tip_continuation, stack_fragment, handler_fragment);

    let mut new_tip_address = stack_fragment_end;

    // Set up arguments for invoking the handler. Note that arguments on the stack are in reverse
    // order.
    new_tip_address = new_tip_address.sub(1);
    new_tip_address.write(UniformType::to_uniform_sptr(captured_continuation_thunk));

    // Then set up explicit handler arguments.
    for i in (0..handler_num_args).rev() {
        new_tip_address = new_tip_address.sub(1);
        new_tip_address.write(handler_call_base_address.add(i).read());
    }

    // Lastly set up the handler parameter.
    new_tip_address = new_tip_address.sub(1);
    new_tip_address.write(matching_parameter);

    // Set up return values.
    let tip_address_before_forcing_handler_impl = new_tip_address;
    let handler_function_ptr = runtime_force_thunk(handler_impl, &mut new_tip_address);
    // unpacking captured arguments of the handler would affect frame height of the next continuation as well.
    (*next_continuation).arg_stack_frame_height += tip_address_before_forcing_handler_impl.offset_from(new_tip_address) as usize;
    (handler_function_ptr, new_tip_address, next_continuation)
}

unsafe fn create_captured_continuation(
    captured_continuation_thunk_impl: RawFuncPtr,
    tip_continuation: *mut Continuation,
    stack_fragment: Vec<Generic>,
    handler_fragment: Vec<Handler<usize>>,
) -> *mut usize {
    let captured_continuation = runtime_alloc((std::mem::size_of::<CapturedContinuation>()) / 8) as *mut CapturedContinuation;

    *captured_continuation = CapturedContinuation {
        tip_continuation,
        handler_fragment,
        stack_fragment,
    };

    create_captured_continuation_thunk(captured_continuation_thunk_impl, captured_continuation)
}

unsafe fn create_captured_continuation_thunk(captured_continuation_thunk_impl: RawFuncPtr, captured_continuation: *mut CapturedContinuation) -> *mut usize {
    let captured_continuation_thunk = runtime_alloc(3);
    captured_continuation_thunk.write(UniformType::to_uniform_pptr(captured_continuation_thunk_impl));
    captured_continuation_thunk.add(1).write(1);
    captured_continuation_thunk.add(2).write(UniformType::to_uniform_sptr(captured_continuation));
    captured_continuation_thunk
}

#[repr(C, align(8))]
pub struct SimpleResult<'a> {
    handler_parameter: Uniform,
    /// Result value could be one of the following:
    /// 1. an exceptional value
    /// 2. a result value from the simpler linear operation
    /// 3. a compound value of either an exception or a result value from an affine operation
    /// The type used here provides easy access to the third case. But one can also call [UPtr.as_uniform] to get the
    /// uniform representation for the first two cases.
    result_value: UPtr<&'a SimpleResultValue>,
}

#[repr(C, align(8))]
struct SimpleResultValue {
    /// 0 means exceptional. 0b10 means normal (aka 1 in uniform representation).
    tag: Uniform,
    value: Uniform,
}

unsafe fn prepare_simple_operation(
    handler_call_base_address: *mut usize,
    next_continuation: &mut Continuation,
    simple_handler_runner_impl_ptr: RawFuncPtr,
    handler_index: usize,
    handler_impl: ThunkPtr,
    handler_type: HandlerType,
) -> (*const usize, *mut usize, *mut Continuation) {
    assert!(handler_type != HandlerType::Complex);
    let matching_handler = HANDLERS.with(|handler| match handler.borrow_mut().get_mut(handler_index).unwrap() {
        HandlerEntry::Handler(handler) => handler as *mut Handler<*mut Uniform>,
        _ => panic!("Expect a handler entry")
    });

    HANDLERS.with(|handler| handler.borrow_mut().push(SimpleOperationMarker { handler_index }));

    let mut tip_address = handler_call_base_address;
    tip_address = tip_address.sub(1);
    tip_address.write((*matching_handler).parameter);
    let handler_impl_ptr = runtime_force_thunk(handler_impl, &mut tip_address);

    tip_address = tip_address.sub(1);
    tip_address.write(handler_type.ordinal() as usize);

    tip_address = tip_address.sub(1);
    tip_address.write(handler_index);

    tip_address = tip_address.sub(1);
    tip_address.write(handler_impl_ptr as usize);

    // Note, here we do not need to update the frame height of the next continuation because that will be derived from
    // the last result pointer inside `simple_handler_runner_impl`. And this is fine because an operation must return
    // a value (rather than a computation). Hence, when it's done, it must place the result right below the base
    // address of the next continuation.
    // We can still update the frame height of the next continuation here, but that would be redundant and hence left
    // out.

    (simple_handler_runner_impl_ptr, tip_address, next_continuation)
}

/// Special function that may do long jump instead of normal return if the result is exceptional. If
/// the result is exceptional. This function also takes care of disposing the handler parameters of
/// all the evicted handlers.
#[no_mangle]
pub unsafe extern "C" fn runtime_process_simple_handler_result(
    handler_index: usize,
    simple_handler_type: HandlerTypeOrdinal,
    simple_result: &SimpleResult,
    simple_exception_continuation_impl: RawFuncPtr,
    runtime_disposer_loader_cps_impl: ContImplPtr,
) -> Uniform {
    let matching_handler = HANDLERS.with(|handlers| {
        let mut handlers = handlers.borrow_mut();
        let handler = match handlers.get_mut(handler_index).unwrap() {
            HandlerEntry::Handler(handler) => {
                handler.parameter = simple_result.handler_parameter;
                handler as *mut Handler<*mut Uniform>
            }
            _ => panic!("Expect a handler entry")
        };
        // pop the simple handler entry marker
        let last_entry = handlers.pop().unwrap();
        assert!(matches!(last_entry, SimpleOperationMarker { .. }));
        handler
    });

    let (value, tag) = match simple_handler_type {
        0 => (simple_result.result_value.as_uniform(), 0),
        1 => (simple_result.result_value.as_uniform(), 1),
        2 => {
            let ptr = simple_result.result_value;
            (ptr.value, ptr.tag >> 1)
        }
        _ => unreachable!("bad simple operation type")
    };

    match tag {
        0 => unsafe {
            let handler_output_result = value;
            let next_continuation = (*(*matching_handler).transform_loader_continuation).next;
            let simple_exception_continuation = runtime_alloc(4) as *mut Continuation;
            *simple_exception_continuation = Continuation {
                func: simple_exception_continuation_impl,
                arg_stack_frame_height: 0,
                next: next_continuation,
                // The exceptional value is stored inside the state field of the continuation.
                state: handler_output_result,
            };
            let (next_continuation, next_base_address) = convert_handler_transformers_to_disposers(
                (*matching_handler).transform_loader_base_address,
                handler_index,
                simple_exception_continuation,
                runtime_disposer_loader_cps_impl,
            );
            // Actually the result is not needed since the continuation is a disposer loader continuation, which always
            // ignores the last result.
            let result_ptr = next_base_address.sub(1);
            result_ptr.write(UniformType::to_uniform_sptr(empty_struct_ptr()));
            // Here we use the matching handler because the jump needs to restore execution to the state at the matching
            // handler. This mismatch between fp, sp, and lr with the continuation and argument stack is fine because
            // the continuation is a disposer loader continuation, which does not care about fp, sp, or lr.
            long_jump(next_base_address, next_continuation, result_ptr, matching_handler);
        }
        1 => value,
        _ => unreachable!("bad simple result tag")
    }
}

#[no_mangle]
pub unsafe extern "C" fn runtime_pop_handler() -> Uniform {
    HANDLERS.with(|handlers| {
        let handler = handlers.borrow_mut().pop().unwrap();
        match handler {
            HandlerEntry::Handler(handler) => handler.parameter,
            _ => panic!("Expect a handler entry")
        }
    })
}

/// Convert transform continuations in the handler stack to disposer continuations. Technically
/// we don't have to implement disposers this way. Instead, we can just invoke the disposer directly
/// since they are guranteed to not cause any effects with my current plan. But if somehow I decide
/// to allow disposer and simple operations to perform simple exceptional effects, this
/// implementation will be useful.
unsafe fn convert_handler_transformers_to_disposers(
    base_address: *mut Uniform,
    matching_handler_index: usize,
    mut next_continuation: *mut Continuation,
    runtime_disposer_loader_cps_impl: ContImplPtr,
) -> (*mut Continuation, *mut Uniform) {
    let mut next_base_address = if (*next_continuation).state == usize::MAX {
        // Disregard the height of trivial continuations because they are not supposed to be used.
        // Changes to the frame height of trivial continuations is useful. But the same information
        // is also inside the last result address passed to the trivial continuation. Hence,
        // consumer of the trivial continuation can recover the tip address from the last result
        // value instead.
        base_address
    } else {
        base_address.add((*next_continuation).arg_stack_frame_height)
    };

    HANDLERS.with(|handlers| {
        let mut ref_mut = handlers.borrow_mut();
        let handlers = ref_mut.deref_mut();
        let mut index = 0;
        handlers.retain(|entry| {
            if index < matching_handler_index {
                index += 1;
                true
            } else {
                index += 1;
                matches!(entry, HandlerEntry::Handler { .. })
            }
        });
        let mut last_handler = std::ptr::null();
        for entry in handlers[matching_handler_index..].iter_mut() {
            let HandlerEntry::Handler(handler) = entry else { unreachable!() };
            let parameter_disposer = handler.parameter_disposer as Uniform;
            (*handler.transform_loader_continuation).next = next_continuation;
            (*handler.transform_loader_continuation).func = runtime_disposer_loader_cps_impl;
            if (*next_continuation).state != usize::MAX {
                // trivial continuation has state equal to 0xffffffffffffffff and we don't want to
                // update the frame height of trivial continuations because this field is not
                // supposed to be consumed. And for debug build, it's set to a very large value so
                // that it can allow arbitrary increment and decrement without overflow or
                // underflow.
                (*next_continuation).arg_stack_frame_height = next_base_address.offset_from(handler.transform_loader_base_address) as usize;
            }

            next_continuation = handler.transform_loader_continuation;
            next_base_address = handler.transform_loader_base_address;
            handler.transform_loader_base_address.write(parameter_disposer);
            last_handler = handler;
        }
        ((*last_handler).transform_loader_continuation, (*last_handler).transform_loader_base_address)
    })
}

fn find_matching_handler(eff: Eff, may_be_complex: usize) -> (usize, ThunkPtr, HandlerType) {
    HANDLERS.with(|handlers| {
        let handlers = handlers.borrow();
        let mut i = handlers.len();
        while i > 0 {
            let handler_index = i - 1;
            match handlers.get(handler_index).unwrap() {
                HandlerEntry::Handler(handler) => {
                    if may_be_complex != 0 {
                        for (e, handler_impl) in &handler.complex_handler {
                            if unsafe { compare_uniform(eff, *e) } {
                                return (handler_index, *handler_impl, HandlerType::Complex);
                            }
                        }
                    }
                    for (e, handler_impl, simple_operation_type) in &handler.simple_handler {
                        if unsafe { compare_uniform(eff, *e) } {
                            return (handler_index, *handler_impl, match simple_operation_type {
                                0 => HandlerType::Exceptional,
                                1 => HandlerType::Linear,
                                2 => HandlerType::Affine,
                                _ => unreachable!("bad simple operation type")
                            });
                        }
                    }
                    i = handler_index;
                }
                HandlerEntry::SimpleOperationMarker { handler_index } => {
                    i = *handler_index;
                }
            }
        }
        panic!("No matching handler found")
    })
}


#[no_mangle]
pub unsafe extern "C" fn runtime_register_handler(
    tip_address_ptr: *mut *mut usize,
    next_continuation: &mut Continuation,
    parameter: Uniform,
    parameter_disposer: ThunkPtr,
    parameter_replicator: ThunkPtr,
    transform: ThunkPtr,
    transform_loader_cps_impl: RawFuncPtr,
) -> *const Handler<*mut Uniform> {
    let transform_loader_continuation = &mut *(runtime_alloc(4) as *mut Continuation);
    transform_loader_continuation.func = transform_loader_cps_impl;
    transform_loader_continuation.next = next_continuation;
    // Initially the transform loader is chained with handler input. At that point there are no
    // arguments passed from transform loader to the input function. Hence the frame height is 0.
    // Later transform loader invokes the handler transform function. But that invocation is a tail
    // call, so the frame height is never updated later.
    transform_loader_continuation.arg_stack_frame_height = 0;
    transform_loader_continuation.state = 0;

    // This is needed because by joining transform loader continuation with the caller continuation,
    // we are essentially making the caller function call the transform loader function with the
    // needed arguments. Hence the arg stack frame height of the caller continuation should be
    // increased by the number of arguments needed by the transform loader function.
    // There is only one argument to the transform loader function, which is the thunk to the actual
    // transform function.
    let transform_loader_num_args = 1;
    next_continuation.arg_stack_frame_height += transform_loader_num_args;

    // write the transform thunk to the stack because it's the single argument that is needed by
    // the transform loader function.
    let new_tip_address = tip_address_ptr.read().sub(1);
    new_tip_address.write(transform as usize);
    tip_address_ptr.write(new_tip_address);

    HANDLERS.with(|handlers| {
        handlers.borrow_mut().push(HandlerEntry::Handler(Handler {
            transform_loader_continuation,
            transform_loader_base_address: new_tip_address,
            parameter,
            parameter_disposer,
            parameter_replicator,
            // TODO: use custom allocator that allocates through Boehm GC for vecs
            simple_handler: Vec::new(),
            complex_handler: Vec::new(),
            // These are updated by runtime_mark_handler
            stack_pointer: std::ptr::null(),
            frame_pointer: std::ptr::null(),
            return_address: std::ptr::null(),
            general_callee_saved_registers: [0; 10],
            vector_callee_saved_registers: [0; 8],
        }));
        match handlers.borrow().last().unwrap()
        {
            HandlerEntry::Handler(handler) => handler as *const Handler<*mut Uniform>,
            _ => panic!("Expect a handler entry")
        }
    })
}

#[no_mangle]
pub extern "C" fn runtime_add_handler(handler: &mut Handler<*mut Uniform>, eff: Eff, handler_impl: ThunkPtr, handler_type: HandlerTypeOrdinal) {
    if handler_type == HandlerType::Complex.ordinal() as usize {
        handler.complex_handler.push((eff, handler_impl))
    } else {
        handler.simple_handler.push((eff, handler_impl, handler_type));
    }
}

/// Returns a pointer pointing to the following:
/// - ptr + 0: the function pointer to the resumed continuation
/// - ptr + 8: the base address for the resumed continuation to find its arguments
/// - ptr + 16: the pointer to the "last result" that should be passed to the resumed continuation
#[no_mangle]
pub unsafe extern "C" fn runtime_prepare_resume_continuation(
    base_address: *mut usize,
    next_continuation: &mut Continuation,
    captured_continuation: *mut CapturedContinuation,
    parameter: Uniform,
    result: Uniform,
    frame_pointer: *const u8,
    stack_pointer: *const u8,
) -> *const usize {
    let (new_base_address, tip_continuation) = unpack_captured_continuation(
        base_address,
        next_continuation,
        captured_continuation.read(),
        parameter,
        frame_pointer,
        stack_pointer,
        4, // 4 arguments passed to captured continuation: captured continuation object, field, handler parameter, and result.
    );

    // Write the last result
    let last_result_address = new_base_address.sub(1);
    last_result_address.write(result);

    // Write the return values of this helper function.
    let tip_address = last_result_address.sub(3);
    tip_address.write(tip_continuation as usize);
    tip_address.add(1).write(new_base_address as usize);
    tip_address.add(2).write(last_result_address as usize);

    tip_address
}


/// Returns a pointer pointing to the following:
/// - ptr + 0: the function pointer to the resumed continuation
/// - ptr + 8: the base address for the resumed continuation to find its arguments
/// - ptr + 16: the pointer to the "last result" that should be passed to the resumed continuation
#[no_mangle]
pub unsafe extern "C" fn runtime_prepare_dispose_continuation(
    base_address: *mut usize,
    next_continuation: &mut Continuation,
    captured_continuation: *mut CapturedContinuation,
    parameter: Uniform,
    frame_pointer: *const u8,
    stack_pointer: *const u8,
    runtime_disposer_loader_cps_impl: ContImplPtr,
) -> *const Uniform {
    let matching_handler_index = HANDLERS.with(|handlers| handlers.borrow().len());

    // 3 arguments passed to captured continuation: captured continuation object, field, and handler parameter.
    let dispose_num_args = 3;
    unpack_captured_continuation(
        base_address,
        next_continuation,
        captured_continuation.read(),
        parameter,
        frame_pointer,
        stack_pointer,
        dispose_num_args,
    );

    let (next_continuation, next_base_address) = convert_handler_transformers_to_disposers(
        base_address,
        matching_handler_index,
        next_continuation,
        runtime_disposer_loader_cps_impl,
    );

    // Write the last result
    let last_result_address = next_base_address.sub(1);
    // Add 1 so it's tagged as an SPtr
    last_result_address.write(UniformType::to_uniform_sptr(empty_struct_ptr()));

    // Write the return values of this helper function.
    let result_ptr = last_result_address.sub(3);

    result_ptr.write(next_continuation as usize);
    result_ptr.add(1).write(next_base_address as usize);
    result_ptr.add(2).write(last_result_address as usize);

    result_ptr
}

unsafe fn unpack_captured_continuation(
    base_address: *mut usize,
    next_continuation: &mut Continuation,
    mut captured_continuation: CapturedContinuation,
    parameter: Uniform,
    frame_pointer: *const u8,
    stack_pointer: *const u8,
    captured_continuation_record_num_args: usize,
) -> (*mut Uniform, *mut Continuation) {
    let base_handler = captured_continuation.handler_fragment.first_mut().unwrap();
    base_handler.parameter = parameter;
    (*base_handler.transform_loader_continuation).next = next_continuation;
    // Chain the base of the captured continuation to the next continuation, where we need to add
    // all the arguments for the handler transform function to the argument stack. Hence we need to
    // update the stack frame height of the next continuation.
    next_continuation.arg_stack_frame_height += TRANSFORM_LOADER_NUM_ARGS;
    next_continuation.arg_stack_frame_height -= captured_continuation_record_num_args;

    // swallow the 4 arguments passed to the captured continuation record:
    // captured continuation object, field, handler parameter, and last result.
    let mut base_address = base_address.add(captured_continuation_record_num_args);

    // The argument to transform loader is the transform thunk, which is set in
    // `runtime_register_handler` and got captured in `runtime_prepare_complex_operation` inside the
    // stack fragment. So this value is restored in the loop right below this statement.
    let transform_loader_base_address = base_address.sub(TRANSFORM_LOADER_NUM_ARGS);
    for arg in captured_continuation.stack_fragment.iter().rev() {
        base_address = base_address.sub(1);
        base_address.write(*arg);
    }
    let tip_continuation = captured_continuation.tip_continuation;
    let tip_continuation_height = (*tip_continuation).arg_stack_frame_height;
    let tip_base_address = base_address.add(tip_continuation_height);

    HANDLERS.with(|handlers| {
        let mut handlers = handlers.borrow_mut();
        for handler in captured_continuation.handler_fragment.into_iter() {
            handlers.push(HandlerEntry::Handler(Handler {
                transform_loader_base_address: transform_loader_base_address.sub(handler.transform_loader_base_address),
                transform_loader_continuation: handler.transform_loader_continuation,
                parameter: handler.parameter,
                parameter_disposer: handler.parameter_disposer,
                parameter_replicator: handler.parameter_replicator,
                simple_handler: handler.simple_handler,
                complex_handler: handler.complex_handler,
                // Captured continuation must be CPS transformed, which means all calls must be tail-optimized, and
                // hence the frame pointer and stack pointer are all the same across all handler entries.
                stack_pointer,
                frame_pointer,
                return_address: handler.return_address,
                general_callee_saved_registers: handler.general_callee_saved_registers,
                vector_callee_saved_registers: handler.vector_callee_saved_registers,
            }));
        }
    });
    (tip_base_address, tip_continuation)
}

/// Returns the pointer to the result of disposer.
#[no_mangle]
pub unsafe extern "C" fn runtime_replicate_continuation(
    base_address: *mut usize,
    next_continuation: &mut Continuation,
    captured_continuation: &mut CapturedContinuation,
    parameter: Uniform,
    frame_pointer: *const u8,
    stack_pointer: *const u8,
    runtime_invoke_cps_function_with_trivial_continuation: extern fn(RawFuncPtr, *mut Uniform) -> *mut Uniform,
    captured_continuation_thunk_impl: RawFuncPtr,
) -> *const Uniform {
    let matching_handler_index = HANDLERS.with(|handlers| handlers.borrow().len());

    // 3 arguments passed to captured continuation: captured continuation object, field, and handler parameter.
    let dispose_num_args = 3;
    unpack_captured_continuation(
        base_address,
        next_continuation,
        captured_continuation.clone(),
        parameter,
        frame_pointer,
        stack_pointer,
        dispose_num_args,
    );

    let mut parameters = Vec::new();
    HANDLERS.with(|handlers| {
        let mut handlers = handlers.borrow_mut();
        while handlers.len() > matching_handler_index {
            let handler = match handlers.pop().unwrap() {
                HandlerEntry::Handler(handler) => handler,
                _ => panic!("Expect a handler entry")
            };
            let parameter_replicator = handler.parameter_replicator;
            if parameter_replicator as Uniform == 0b11 {
                // if parameter replicator is null, then we just shallow copy the parameter (that's fine since
                // everything on the heap is immutable)
                parameters.push((handler.parameter, handler.parameter));
                continue;
            }
            let mut tip_address = base_address.sub(1);
            tip_address.write(handler.parameter);
            let replicator_func_ptr = runtime_force_thunk(parameter_replicator, &mut tip_address);
            let parameter_pair = runtime_invoke_cps_function_with_trivial_continuation(replicator_func_ptr, tip_address);
            parameters.push((parameter_pair.read(), parameter_pair.add(1).read()));
        }
    });

    let mut cloned_handler_fragment = Vec::new();
    for ((parameter1, parameter2), handler) in parameters.into_iter().zip(captured_continuation.handler_fragment.iter_mut()) {
        handler.parameter = parameter1;
        let cloned_handler = Handler {
            transform_loader_continuation: handler.transform_loader_continuation,
            transform_loader_base_address: handler.transform_loader_base_address,
            parameter: parameter2,
            parameter_disposer: handler.parameter_disposer,
            parameter_replicator: handler.parameter_replicator,
            frame_pointer: handler.frame_pointer,
            stack_pointer: handler.stack_pointer,
            return_address: handler.return_address,
            simple_handler: handler.simple_handler.clone(),
            complex_handler: handler.complex_handler.clone(),
            general_callee_saved_registers: handler.general_callee_saved_registers,
            vector_callee_saved_registers: handler.vector_callee_saved_registers,
        };
        cloned_handler_fragment.push(cloned_handler);
    }

    let mut cloned_continuation = std::ptr::null_mut();
    clone_continuation(captured_continuation.tip_continuation, &mut cloned_continuation, cloned_handler_fragment.iter_mut().peekable());

    let captured_continuation_thunk1 = create_captured_continuation_thunk(
        captured_continuation_thunk_impl,
        captured_continuation,
    );
    let captured_continuation_thunk2 = create_captured_continuation(
        captured_continuation_thunk_impl,
        cloned_continuation,
        captured_continuation.stack_fragment.clone(),
        cloned_handler_fragment,
    );

    // Write the last result
    let last_result_address = base_address.sub(1);
    let pair_of_captured_continuation_thuns = runtime_alloc(2);
    pair_of_captured_continuation_thuns.write(UniformType::to_uniform_sptr(captured_continuation_thunk1));
    pair_of_captured_continuation_thuns.add(1).write(UniformType::to_uniform_sptr(captured_continuation_thunk2));

    last_result_address.write(UniformType::to_uniform_sptr(pair_of_captured_continuation_thuns));

    // Write the return values of this helper function.
    let result_ptr = last_result_address.sub(3);

    let next_base_address = base_address.add(next_continuation.arg_stack_frame_height);
    result_ptr.write(next_continuation as *const Continuation as usize);
    result_ptr.add(1).write(next_base_address as usize);
    result_ptr.add(2).write(last_result_address as usize);

    result_ptr
}

unsafe fn clone_continuation(continuation: *mut Continuation, cloned_continuation_ptr: &mut *mut Continuation, mut iter_mut: Peekable<IterMut<Handler<usize>>>) {
    let continuation_addr = continuation as *const usize;
    let length = (continuation_addr).sub(1).read();
    let cloned_continuation_addr = runtime_alloc(length);
    // we must clone like this because continuation struct is not aware of the variable number of
    // local variables that are allocated right after the continuation object.
    std::ptr::copy_nonoverlapping(continuation_addr, cloned_continuation_addr, length);
    let cloned_continuation = cloned_continuation_addr as *mut Continuation;
    if let Some(handler) = iter_mut.peek_mut() {
        if handler.transform_loader_continuation == continuation {
            handler.transform_loader_continuation = cloned_continuation;
            iter_mut.next();
        }
    }
    *cloned_continuation_ptr = cloned_continuation;
    let next_continuation = (*continuation).next;
    if next_continuation.is_null() {
        return;
    }
    clone_continuation(next_continuation, &mut (*cloned_continuation).next, iter_mut)
}

unsafe fn compare_uniform(a: Uniform, b: Uniform) -> bool {
    if a == b {
        return true;
    }
    let a_type = UniformType::from_bits(a);
    let b_type = UniformType::from_bits(b);
    if a_type != b_type {
        return false;
    }
    match a_type {
        UniformType::Raw => false,
        UniformType::SPtr => {
            let a_ptr = a.to_normal_ptr();
            let b_ptr = b.to_normal_ptr();
            let a_size = a_ptr.sub(1).read();
            let b_size = b_ptr.sub(1).read();
            if a_size != b_size {
                return false;
            }
            for i in 0..a_size {
                if !compare_uniform(a_ptr.add(i).read(), b_ptr.add(i).read()) {
                    return false;
                }
            }
            true
        }
        UniformType::PPtr => {
            let a_ptr = a.to_normal_ptr();
            let b_ptr = b.to_normal_ptr();
            let a_size = a_ptr.sub(1).read();
            let b_size = b_ptr.sub(1).read();
            if a_size != b_size {
                return false;
            }
            for i in 0..a_size {
                if a_ptr.add(i).read() != b_ptr.add(i).read() {
                    return false;
                }
            }
            true
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        unsafe {
            let ptr = runtime_alloc(10);
            *ptr = 42;
            assert_eq!(ptr as usize % 8, 0);
            assert_eq!(ptr.read(), 42);
        }
    }
}
