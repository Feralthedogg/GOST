module main

unsafe fn copy_reg(x: i64) -> i64 {
    let y: i64 = 0
    asm volatile("mov %1, %0" : "=r"(y) : "r"(x))
    return y
}

fn main() -> i32 {
    let v: i64 = 7
    let _out: i64 = unsafe { copy_reg(v) }
    return 0
}
