module main

unsafe fn check_zero(x: i64) -> i32 {
start:
    asm goto("test %0, %0; je %l0" : : "r"(x) : "cc" : is_zero);
    return 0;
is_zero:
    return 1;
}

fn main() -> i32 {
    let z: i64 = 0;
    let _v: i32 = unsafe { check_zero(z) };
    return 0;
}
