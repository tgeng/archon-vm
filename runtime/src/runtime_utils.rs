use std::ops::BitAnd;

pub unsafe fn runtime_alloc(num_words: usize) -> *mut usize {
    let mut vec = Vec::with_capacity(num_words);
    let ptr = vec.as_mut_ptr();
    std::mem::forget(vec);
    ptr
}

/// Takes a pointer to a function or thunk, push any arguments to the tip of the stack, and return
/// a pointer to the underlying raw function.
pub unsafe fn runtime_force_thunk(thunk: *const usize, tip_address_ptr: *mut *mut usize) -> *const usize {
    if (thunk as usize).bitand(1) == 1 {
        // Last bit being 1 means it's a raw function pointer (rather than a pointer to a thunk on
        // the heap.
        ((thunk as usize) - 1) as *const usize
    } else {
        let next_thunk = thunk.read() as *const usize;
        let num_args = next_thunk.add(1).read();
        let mut tip_address = tip_address_ptr.read();
        for i in (0..num_args).rev() {
            let arg = thunk.add(2 + i).read();
            tip_address = tip_address.sub(1);
            tip_address.write(arg);
        }
        tip_address_ptr.write(tip_address);
        runtime_force_thunk(next_thunk, tip_address_ptr)
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
