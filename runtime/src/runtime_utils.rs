use std::cell::RefCell;
use crate::runtime::{Continuation, HandlerEntry, Handler, Uniform, ThunkPtr, Eff};
use crate::types::{UniformPtr, UniformType};

// TODO: use custom allocator that allocates through Boehm GC for vecs
thread_local!(
    static HANDLERS: RefCell<Vec<HandlerEntry>> = RefCell::new(Vec::new());
);

pub unsafe fn runtime_alloc(num_words: usize) -> *mut usize {
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
pub unsafe fn runtime_force_thunk(thunk: *const usize, tip_address_ptr: *mut *mut usize) -> *const usize {
    let thunk_ptr = thunk.to_normal_ptr();
    if UniformType::from_bits(thunk as usize) == UniformType::PPtr {
        thunk_ptr
    } else {
        let next_thunk = thunk_ptr.read() as *const usize;
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
}

/// Alocate
pub unsafe fn runtime_alloc_stack() -> *mut usize {
    // Allocate a stack with 8MB of space
    let stack_size = 1 << 6;
    let mut vec: Vec<usize> = Vec::with_capacity(stack_size);
    let start: *mut usize = vec.as_mut_ptr();
    std::mem::forget(vec);
    start.add(stack_size)
}

/// Returns the result of the operation in uniform representation
/// TODO: add args
pub unsafe fn runtime_handle_simple_operation(eff: usize, base_address: *const usize) -> usize {
    let (handler_index, handler_impl, num_args) = find_matching_handler(eff, true);
    todo!()
}

/// Returns the following results on the argument stack.
/// - ptr + 0: the function pointer to the matched handler implementationon
/// - ptr + 8: the base address used to find the arguments when invoking the handler implementation
/// - ptr + 16: the next continuation after handler finishes execution
/// The pointer + 8 is the base address that should be passed to this pointed handler implementaion
/// function, which will find its arguments from that base address.
pub unsafe fn runtime_prepare_complex_operation(eff: usize, base_address: *const usize, continuation: *mut Continuation) -> *const usize {
    let (handler_index, handler_impl, num_args) = find_matching_handler(eff, false);

    todo!()
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

fn find_matching_handler(eff: Eff, simple: bool) -> (usize, ThunkPtr, usize) {
    HANDLERS.with(|handler| {
        for (i, e) in handler.borrow().iter().rev().enumerate() {
            if let HandlerEntry::Handler(handler) = e {
                if simple {
                    for (e, handler_impl, num_args) in &handler.simple_handler {
                        if unsafe { compare_uniform(eff, *e) } {
                            return (i, *handler_impl, *num_args);
                        }
                    }
                } else {
                    for (e, handler_impl, num_args) in &handler.complex_handler {
                        if unsafe { compare_uniform(eff, *e) } {
                            return (i, *handler_impl, *num_args);
                        }
                    }
                }
            }
        }
        panic!("No matching handler found")
    })
}


pub unsafe fn runtime_register_handler_and_get_transform_continuation(
    caller_tip_address: *mut usize,
    caller_continuation: &mut Continuation,
    parameter: Uniform,
    parameter_disposer: ThunkPtr,
    parameter_replicator: ThunkPtr,
    transform: ThunkPtr,
    transform_num_args: usize,
    transform_var_bound: usize,
) -> *const Continuation {
    let transform_continuation = &mut *(runtime_alloc(4 + transform_var_bound) as *mut Continuation);
    let mut tip_address = caller_tip_address;
    transform_continuation.func = runtime_force_thunk(transform, &mut tip_address);
    transform_continuation.next = caller_continuation;
    transform_continuation.arg_stack_frame_height = 0;
    transform_continuation.state = 0;

    // This is needed because by joining transform continuation with the caller continuation, we
    // are essentially making the caller function call the transform function with the needed
    // arguments. Hence the arg stack frame height of the caller continuation should be increased
    // by the number of arguments needed by the transform function.
    // Note that the parameter and result arguments of the transform function are obtained via
    // special constructs `PopHandler`, and `GetLastResult` rather than the argument stack. These
    // special constructs are created during signature optimization, which, in addition, also lifts
    // all the components of a handler.
    caller_continuation.arg_stack_frame_height += ((tip_address as usize) - (caller_tip_address as usize) / 8);

    HANDLERS.with(|handlers| handlers.borrow_mut().push(HandlerEntry::Handler(Handler {
        transform_continuation,
        transform_num_args,
        parameter,
        parameter_disposer,
        parameter_replicator,
        // TODO: use custom allocator that allocates through Boehm GC for vecs
        simple_handler: Vec::new(),
        complex_handler: Vec::new(),
    })));
    transform_continuation
}

pub unsafe fn get_current_handler() -> *mut Handler {
    HANDLERS.with(|handlers| {
        let mut handlers = handlers.borrow_mut();
        let handler = handlers.last_mut().unwrap();
        match handler {
            HandlerEntry::Handler(handler) => handler as *mut Handler,
            _ => panic!("Expect a handler entry")
        }
    })
}

pub fn runtime_add_simple_handler(handler: &mut Handler, eff: Eff, handler_impl: ThunkPtr, num_args: usize) {
    handler.simple_handler.push((eff, handler_impl, num_args))
}

pub fn runtime_add_complex_handler(handler: &mut Handler, eff: Eff, handler_impl: ThunkPtr, num_args: usize) {
    handler.complex_handler.push((eff, handler_impl, num_args))
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
            let a_size = a_ptr.sub(8).read();
            let b_size = b_ptr.sub(8).read();
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
            let a_size = a_ptr.sub(8).read();
            let b_size = b_ptr.sub(8).read();
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
