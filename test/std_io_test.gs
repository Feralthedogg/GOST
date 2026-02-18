module main

import "std/io"
import "std/fmt"

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

fn empty_reader(_max: i32) -> Result[string, error] {
    return Result.Ok[string, error]("")
}

fn count_writer(chunk: string) -> Result[i64, error] {
    return Result.Ok[i64, error](string_len(chunk))
}

fn main() -> i32 {
    let rf: fn(i32) -> Result[string, error] = empty_reader
    let wf: fn(string) -> Result[i64, error] = count_writer

    match copy_all(new_reader(rf), new_writer(wf)) {
        Result.Ok(n) => {
            if n != 0 as i64 { return 1
 }
        },
        Result.Err(_) => { return 2
 },
    }

    match read_all(new_reader(rf)) {
        Result.Ok(v) => {
            if !eq_str(v, "") { return 3
 }
        },
        Result.Err(_) => { return 4
 },
    }

    fmt.println("std_io_test ok")
    return 0
}
