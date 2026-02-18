module main

import "std/fmt"

unsafe fn demo(x: i64) -> i64 {
    let y: i64 = asm[i64]("mov $1, $0", "=r,r", x)
    let v: i64 = asm_volatile[i64]("mov $1, $0", "=r,r", y)
    asm[unit]("nop", "")
    let z: i32 = asm_pure[i32]("mov $1, $0", "=r,r", 7)
    if z > 0 {
    }
    return v
}

fn main() -> i32 {
    let n: i64 = 3
    let _v: i64 = unsafe { demo(n) }
    println("asm ext ok")
    return 0
}
