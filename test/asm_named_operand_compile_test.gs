module main

unsafe fn copy_named(src: i64) -> i64 {
    let dst: i64 = 0;
    asm volatile("mov %[src], %[dst]" : [dst] "=r"(dst) : [src] "r"(src));
    return dst;
}

fn main() -> i32 {
    let x: i64 = 7;
    let _y: i64 = unsafe { copy_named(x) };
    return 0;
}
