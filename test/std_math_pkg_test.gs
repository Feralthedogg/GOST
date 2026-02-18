module main

import "std/math"

fn main() -> i32 {
    if min_i64(2, 5) != 2 { return 1 }
    if max_i64(2, 5) != 5 { return 2 }
    if floor(2.9) != 2.0 { return 3 }
    if ceil(2.1) != 3.0 { return 4 }
    if pow(2.0, 8) != 256.0 { return 5 }
    return 0
}

