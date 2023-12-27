use std::arch::global_asm;
use std::cell::RefCell;
use std::ops::{DerefMut};
use crate::runtime::{Continuation, HandlerEntry, Handler, Uniform, ThunkPtr, Eff, Generic, CapturedContinuation, RawFuncPtr};
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
        add x16, x5, #40 ; get fp address in the handler
        str x29, [x16] ; store fp to the handler

        add x16, x5, #48 ; get sp address in the handler
        mov x17, sp
        str x17, [x16] ; store sp to the handler

        add x16, x5, #56 ; get lr address in the handler
        str x30, [x16] ; store lr to the handler

        br  x4

    _long_jump:
        add x16, x3, #40 ; get fp address in the handler
        ldr x29, [x16] ; restore fp from the handler

        add x16, x3, #48 ; get sp address in the handler
        ldr x17, [x16] ; restore sp from the handler
        mov sp, x17

        add x16, x3, #56 ; get lr address in the handler
        ldr x30, [x16] ; store lr to the handler

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


static mut EMPTY_STRUCT: usize = 0;
const TRANSFORM_LOADER_NUM_ARGS: usize = 1;

unsafe fn empty_struct_ptr() -> *mut usize {
    (&mut EMPTY_STRUCT as *mut usize).add(1)
}

pub unsafe fn runtime_alloc(num_words: usize) -> *mut usize {
    if num_words == 0 {
        return empty_struct_ptr();
    }
    let mut vec = Vec::with_capacity(num_words + 1);
    let ptr: *mut usize = vec.as_mut_ptr();
    // Write the size of this allocation
    ptr.write(num_words);
    std::mem::forget(vec);
    ptr.add(1)
}

pub unsafe fn runtime_word_box() -> *mut usize {
    let mut vec = Vec::with_capacity(1);
    let ptr = vec.as_mut_ptr();
    std::mem::forget(vec);
    ptr
}


/// Takes a pointer to a function or thunk, push any arguments to the tip of the stack, and return
/// a pointer to the underlying raw function.
pub unsafe fn runtime_force_thunk(thunk: ThunkPtr, tip_address_ptr: *mut *mut usize) -> RawFuncPtr {
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
pub unsafe fn runtime_alloc_stack() -> *mut usize {
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
pub unsafe fn debug_helper(fp: *const usize, sp: *const usize, lr: *const usize) -> usize {
    return 1 + 1;
}

/// Returns the following results on the argument stack.
/// - ptr + 0: the function pointer to the matched handler implementationon
/// - ptr + 8: the base address used to find the arguments when invoking the handler implementation
/// - ptr + 16: the next continuation after handler finishes execution
///
pub unsafe fn runtime_prepare_operation(
    eff: Uniform,
    handler_call_base_address: *mut usize,
    tip_continuation: &mut Continuation,
    handler_num_args: usize,
    captured_continuation_thunk_impl: RawFuncPtr,
    simple_handler_runner_impl_ptr: RawFuncPtr,
    may_be_complex: usize,
) -> *const usize {
    let (handler_index, handler_impl, complex) = find_matching_handler(eff, may_be_complex);
    let (handler_function_ptr, new_base_address, next_continuation) =
        if complex {
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
                handler_impl)
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

    let captured_continuation = runtime_alloc((std::mem::size_of::<CapturedContinuation>()) / 8) as *mut CapturedContinuation;

    *captured_continuation = CapturedContinuation {
        tip_continuation,
        handler_fragment,
        stack_fragment,
    };

    let mut new_tip_address = stack_fragment_end as *mut usize;
    // Set up arguments for invoking the handler. Note that arguments on the stack are in reverse
    // order.

    // Firstly, set up the reified continuation.
    let captured_continuation_thunk = runtime_alloc(3);
    captured_continuation_thunk.write(UniformType::to_uniform_pptr(captured_continuation_thunk_impl));
    captured_continuation_thunk.add(1).write(1);
    captured_continuation_thunk.add(2).write(UniformType::to_uniform_sptr(captured_continuation));

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

#[repr(C, align(8))]
pub struct SimpleResult<'a> {
    handler_parameter: Uniform,
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
    tip_continuation: &mut Continuation,
    simple_handler_runner_impl_ptr: RawFuncPtr,
    handler_index: usize,
    handler_impl: ThunkPtr,
) -> (*const usize, *mut usize, *mut Continuation) {
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
    tip_address.write(handler_index);

    tip_address = tip_address.sub(1);
    tip_address.write(handler_impl_ptr as usize);

    (simple_handler_runner_impl_ptr, tip_address, tip_continuation)
}

/// Special function that may do long jump instead of normal return if the result is exceptional. If
/// the result is exceptional. This function also takes care of disposing the handler parameters of
/// all the evicted handlers.
pub fn runtime_process_simple_handler_result(
    handler_index: usize,
    simple_result: &SimpleResult,
    invoke_cps_function_with_trivial_continuation: fn(RawFuncPtr, *mut Uniform) -> *mut Uniform,
) -> Uniform {
    HANDLERS.with(|handlers| {
        let mut handlers = handlers.borrow_mut();
        match handlers.get_mut(handler_index).unwrap() {
            HandlerEntry::Handler(handler) => {
                handler.parameter = simple_result.handler_parameter;
            }
            _ => panic!("Expect a handler entry")
        }
        // pop the simple handler entry marker
        let last_entry = handlers.pop().unwrap();
        assert!(matches!(last_entry, SimpleOperationMarker { .. }));
    });

    match simple_result.result_value.tag {
        0 => unsafe {
            let handler = dispose_handlers(handler_index, invoke_cps_function_with_trivial_continuation);
            let next_continuation = (*handler.transform_loader_continuation).next;
            let next_base_address = handler.transform_loader_base_address.add((*next_continuation).arg_stack_frame_height);
            let result_ptr = handler.transform_loader_base_address.add(TRANSFORM_LOADER_NUM_ARGS).sub(1);
            result_ptr.write(simple_result.result_value.value);
            long_jump(next_base_address, next_continuation, result_ptr, &handler);
        }
        0b10 => simple_result.result_value.value,
        _ => unreachable!("bad simple result tag")
    }
}

pub unsafe fn runtime_pop_handler() -> Uniform {
    HANDLERS.with(|handlers| {
        let handler = handlers.borrow_mut().pop().unwrap();
        match handler {
            HandlerEntry::Handler(handler) => handler.parameter,
            _ => panic!("Expect a handler entry")
        }
    })
}

unsafe fn dispose_handlers(
    matching_handler_index: usize,
    invoke_cps_function_with_trivial_continuation: fn(RawFuncPtr, *mut Uniform) -> *mut Uniform,
) -> Handler<*mut Uniform> {
    // Extract the value here so that the borrowed value is returned right after the closure closes.
    // This is critical because disposer may be exceptional and hence this function may not return
    // locally, skipping any rust drop calls.
    let handlers = HANDLERS.with(|handlers| {
        handlers.borrow_mut().deref_mut() as *mut Vec<HandlerEntry>
    });

    let mut handler = None;
    while (*handlers).len() > matching_handler_index {
        let handler_entry = (*handlers).pop().unwrap();
        match handler_entry {
            HandlerEntry::Handler(h) => {
                let Handler { parameter, parameter_disposer, transform_loader_base_address, .. } = h;
                let mut tip_address = transform_loader_base_address;
                tip_address = tip_address.sub(1);
                tip_address.write(parameter);
                let func_ptr = runtime_force_thunk(parameter_disposer, &mut tip_address);
                // return value is simply ignored since disposers just return empty structs.
                invoke_cps_function_with_trivial_continuation(func_ptr, tip_address);
                handler = Some(h);
            }
            HandlerEntry::SimpleOperationMarker { .. } => {} // simply skip markers
        }
    }
    handler.unwrap()
}

fn find_matching_handler(eff: Eff, may_be_complex: usize) -> (usize, ThunkPtr, bool) {
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
                                return (handler_index, *handler_impl, true);
                            }
                        }
                    }
                    for (e, handler_impl) in &handler.simple_handler {
                        if unsafe { compare_uniform(eff, *e) } {
                            return (handler_index, *handler_impl, false);
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


pub unsafe fn runtime_register_handler(
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
            frame_pointer: std::ptr::null(),
            stack_pointer: std::ptr::null(),
            return_address: std::ptr::null(),
        }));
        match handlers.borrow().last().unwrap()
        {
            HandlerEntry::Handler(handler) => handler as *const Handler<*mut Uniform>,
            _ => panic!("Expect a handler entry")
        }
    })
}

pub fn runtime_add_simple_handler(handler: &mut Handler<*mut Uniform>, eff: Eff, handler_impl: ThunkPtr) {
    handler.simple_handler.push((eff, handler_impl))
}

pub fn runtime_add_complex_handler(handler: &mut Handler<*mut Uniform>, eff: Eff, handler_impl: ThunkPtr) {
    handler.complex_handler.push((eff, handler_impl))
}

/// Returns a pointer pointing to the following:
/// - ptr + 0: the function pointer to the resumed continuation
/// - ptr + 8: the base address for the resumed continuation to find its arguments
/// - ptr + 16: the pointer to the "last result" that should be passed to the resumed continuation
pub unsafe fn runtime_prepare_resume_continuation(
    mut base_address: *mut usize,
    next_continuation: &mut Continuation,
    captured_continuation: *mut CapturedContinuation,
    parameter: Uniform,
    result: Uniform,
    frame_pointer: *const u8,
    stack_pointer: *const u8,
) -> *const usize {
    let (tip_continuation, new_base_address) = unpack_captured_continuation(
        base_address,
        next_continuation,
        captured_continuation,
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

/// Returns the pointer to the result of disposer.
pub unsafe fn runtime_prepare_dispose_continuation(
    base_address: *mut usize,
    next_continuation: &mut Continuation,
    captured_continuation: *mut CapturedContinuation,
    parameter: Uniform,
    runtime_invoke_cps_function_with_trivial_continuation: fn(RawFuncPtr, *mut Uniform) -> *mut Uniform,
    frame_pointer: *const u8,
    stack_pointer: *const u8,
) -> *const Uniform {
    let matching_handler_index = HANDLERS.with(|handlers| handlers.borrow().len());

    // 3 arguments passed to captured continuation: captured continuation object, field, and handler parameter.
    let dispose_num_args = 3;
    unpack_captured_continuation(
        base_address,
        next_continuation,
        captured_continuation,
        parameter,
        frame_pointer,
        stack_pointer,
        dispose_num_args,
    );

    // swallow the 3 arguments passed to the captured continuation record:
    // captured continuation object, field, handler parameter, and last result.
    let tip_address = base_address.add(dispose_num_args);
    dispose_handlers(matching_handler_index, runtime_invoke_cps_function_with_trivial_continuation);

    // Write the last result
    let last_result_address = tip_address.sub(1);
    // Add 1 so it's tagged as an SPtr
    last_result_address.write(empty_struct_ptr() as usize + 1);

    last_result_address
}

unsafe fn unpack_captured_continuation(base_address: *mut usize, next_continuation: &mut Continuation, captured_continuation: *mut CapturedContinuation, parameter: Uniform, frame_pointer: *const u8, stack_pointer: *const u8, resume_continuation_num_args: usize) -> (*mut Continuation, *mut usize) {
    let mut captured_continuation = captured_continuation.read();
    let base_handler = captured_continuation.handler_fragment.first_mut().unwrap();
    base_handler.parameter = parameter;
    (*base_handler.transform_loader_continuation).next = next_continuation;
    // Chain the base of the captured continuation to the next continuation, where we need to add
    // all the arguments for the handler transform function to the argument stack. Hence we need to
    // update the stack frame height of the next continuation.
    next_continuation.arg_stack_frame_height += TRANSFORM_LOADER_NUM_ARGS;
    next_continuation.arg_stack_frame_height -= resume_continuation_num_args;

    // swallow the 4 arguments passed to the captured continuation record:
    // captured continuation object, field, handler parameter, and last result.
    let mut base_address = base_address.add(resume_continuation_num_args);

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
    let new_base_address = base_address.add(tip_continuation_height);

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
                frame_pointer,
                stack_pointer,
                return_address: handler.return_address,
            }));
        }
    });
    (tip_continuation, new_base_address)
}

/// Returns the pointer to the result of disposer.
pub unsafe fn runtime_replicate_continuation(
    base_address: *mut usize,
    captured_continuation: *mut CapturedContinuation,
    parameter: Uniform,
    runtime_invoke_cps_function_with_trivial_continuation: fn(RawFuncPtr, *mut Uniform) -> *mut Uniform,
    frame_pointer: *const u8,
    stack_pointer: *const u8,
) -> *const Uniform {
    todo!()
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
