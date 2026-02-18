module main

import "std/math/rand"

fn main() -> i32 {
    let r = new(123 as u64)
    let a = next_u64(&mut r)
    let b = next_u64(&mut r)
    if a == b { return 1 }
    return 0
}

