module main

import "std/hash/fnv"

fn main() -> i32 {
    if sum64("abc") == 0 as u64 { return 1 }
    let h = new64()
    write_string(&mut h, "abc")
    if sum64_state(h) != sum64("abc") { return 2 }
    return 0
}

