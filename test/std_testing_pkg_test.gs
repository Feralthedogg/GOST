module main

import "std/testing"

fn main() -> i32 {
    let t = new("dummy")
    assert_true(&mut t, true, "must be true")
    if failed(t) { return 1 }
    return 0
}

