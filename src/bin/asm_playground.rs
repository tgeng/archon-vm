use std::arch::global_asm;
global_asm!(
    r#"
    .global _foo

    _foo:
        mov x16, sp
        str x16, [x1]
        ret
"#
);

extern "C" {
    fn foo(a: i64, b: &mut i64) -> i64;
}

fn main() {
    let mut i = 0;
    unsafe {
        foo(1, &mut i);
    }
    println!("{}", i);
}
