module main

import "std/path"
import "std/fmt"

fn nop_visit(_p: string) {
}

fn eq_str(a: string, b: string) -> bool {
    let na: i64 = string_len(a)
    let nb: i64 = string_len(b)
    if na != nb { return false
 }
    let i: i64 = 0 as i64
    while i < na {
        if string_get(a, i) != string_get(b, i) { return false
 }
        i = i + 1 as i64
    }
    return true
}

fn main() -> i32 {
    if !eq_str(clean("a//b/"), "a/b") { return 1
 }
    if !eq_str(clean("\\tmp\\x\\"), "/tmp/x") { return 2
 }
    if !eq_str(base("/tmp/a.txt"), "a.txt") { return 3
 }
    if !eq_str(dir("/tmp/a.txt"), "/tmp") { return 4
 }
    if !eq_str(ext("/tmp/a.txt"), ".txt") { return 5
 }
    if !eq_str(join2("/tmp", "x/y"), "/tmp/x/y") { return 6
 }
    if !is_abs("/tmp") { return 7
 }
    let parts = make_slice[string](0, 0)
    slice_push[string](&mut parts, "/tmp")
    slice_push[string](&mut parts, "a")
    slice_push[string](&mut parts, "b.txt")
    if !eq_str(join(parts), "/tmp/a/b.txt") { return 8 }
    let segs = split("/a/b/c")
    if slice_len[string](&segs) != 0 { return 9 }
    walk("std/path", nop_visit)

    fmt.println("std_path_test ok")
    return 0
}
