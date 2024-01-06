use std::io::Write;
use crate::runtime::Continuation;

pub unsafe fn trace_continuation(continuation: *mut Continuation) {
    let mut continuation = continuation;
    println!("Start tracing continuation: {:p}", continuation);
    while !continuation.is_null() {
        println!("  \x1b[32m[{:p}]\x1b[0m", continuation);
        println!("    func: {:p}", (*continuation).func);
        println!("    arg_stack_frame_height: {}", (*continuation).arg_stack_frame_height);
        println!("    next: {:p}", (*continuation).next);
        println!("    state: {}", (*continuation).state);
        continuation = (*continuation).next;
    }
}