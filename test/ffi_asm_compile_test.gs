module main

import "std/fmt"

extern "C" fn rand() -> i32;

unsafe fn c_rand() -> i32 {
    return rand();
}

unsafe fn asm_nop() -> i64 {
    return asm("nop");
}

fn main() -> i32 {
    let r: i32 = unsafe { c_rand() };
    let a: i64 = unsafe { asm_nop() };
    if r >= 0 && a >= 0 {
        println("ffi/asm ok");
    }
    return 0;
}
