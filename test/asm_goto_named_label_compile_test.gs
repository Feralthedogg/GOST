module main

unsafe fn is_zero(x: i64) -> i32 {
start:
    asm goto("test %0, %0; je %l[zero]" : : "r"(x) : "cc" : zero)
    return 0
zero:
    return 1
}

fn main() -> i32 {
    let v: i64 = 0
    let _r: i32 = unsafe { is_zero(v) }
    return 0
}
