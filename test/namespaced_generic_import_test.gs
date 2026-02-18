module main

import "std/sort" as sort { search_by }

private fn ge3(v: i32) -> bool {
    return v >= 3
}

fn main() -> i32 {
    let z: i64 = string_len("")
    let one: i64 = string_len("a")
    let xs: []i32 = make_slice[i32](z, z)
    slice_push[i32](&mut xs, 1)
    slice_push[i32](&mut xs, 3)
    slice_push[i32](&mut xs, 5)
    let idx: i64 = sort.search_by[i32](&xs, ge3)
    if idx != one {
        return 1
    }
    let idx2: i64 = sort.search_by(&xs, ge3)
    if idx2 != one {
        return 2
    }
    return 0
}
