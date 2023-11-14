use std::io;
use std::io::Write;
use crate::types::{UniformPtr, UniformType};

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
