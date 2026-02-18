module main

import "std/errors"

fn main() -> i32 {
    let base = new("base")
    let wrapped = wrap("top", base)
    if !is(wrapped, base) { return 1 }
    return 0
}

