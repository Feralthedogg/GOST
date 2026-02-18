module main

import "std/encoding/json"
import "std/fmt"

fn main() -> i32 {
    let input = "{\"a\":1,\"b\":[true,null]}"
    match parse(input) {
        Result.Ok(_) => {
            println("json_test ok")
            return 0
        },
        Result.Err(_) => {
            println("json_test err")
            return 1
        },
        _ => { return 2
 },
    }
    return 3
}
