module main

unsafe fn do_nop(x: i64) {
    asm volatile("test %0, %0" : : "r"(x) : "cc")
}

fn main() -> i32 {
    let v: i64 = 1
    unsafe { do_nop(v) }
    return 0
}
