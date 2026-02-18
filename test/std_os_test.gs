module main

import "std/os"
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

fn main() -> i32 {
    let path = "test/tmp_std_os_test.txt"

    // Best-effort cleanup before test.
    match remove(path) {
        Result.Ok(_) => {},
        Result.Err(_) => {},
    }

    match write_file(path, "hello") {
        Result.Ok(_) => {},
        Result.Err(_) => { return 1
 },
    }

    let file_data: string = ""
    match read_file(path) {
        Result.Ok(v) => { file_data = v
 },
        Result.Err(_) => { return 2
 },
    }
    if !eq_str(file_data, "hello") { return 3
 }

    match stat_size(path) {
        Result.Ok(n) => {
            if n != 5 as i64 { return 4
 }
        },
        Result.Err(_) => { return 5
 },
    }

    match setenv("GOST_STD_OS_TEST", "ok") {
        Result.Ok(_) => {},
        Result.Err(_) => { return 6
 },
    }
    if !eq_str(getenv("GOST_STD_OS_TEST"), "ok") { return 7
 }

    let argv = args()
    if slice_len[string](&argv) < 1 as i64 { return 8
 }

    match getwd() {
        Result.Ok(v) => {
            if string_len(v) <= 0 as i64 { return 9
 }
        },
        Result.Err(_) => { return 10
 },
    }

    match remove(path) {
        Result.Ok(_) => {},
        Result.Err(_) => { return 11
 },
    }

    fmt.println("std_os_test ok")
    return 0
}
