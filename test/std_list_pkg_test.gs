module main

import "std/container/list"

fn main() -> i32 {
    let l = new()
    let idx = push_back(&mut l, "x")
    if len(&l) != 1 { return 1 }
    if !remove(&mut l, idx) { return 2 }
    if !is_empty(&l) { return 3 }
    return 0
}

