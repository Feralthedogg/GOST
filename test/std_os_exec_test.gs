module main

import "std/os"
import "std/strings"
import "std/fmt"

fn main() -> i32 {
    let ex = ExecResult { code = 0, output = "" }
    match exec("echo gost-exec") {
        Result.Ok(v) => { ex = v
 },
        Result.Err(_) => { return 1
 },
    }
    if ex.code != 0 { return 2
 }
    if !contains(ex.output, "gost-exec") { return 3
 }

    match pipe("echo gost-pipe") {
        Result.Ok(out) => {
            if !contains(out, "gost-pipe") { return 4
 }
        },
        Result.Err(_) => { return 5
 },
    }

    fmt.println("std_os_exec_test ok")
    return 0
}
