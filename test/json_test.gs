module main

import "std/encoding/json"
import "std/fmt"

fn main() -> i32 {
    let input = "{\"a\":1,\"b\":[true,null]}"
    let r = parse(input)
    let ok = match r {
        Result.Ok(_) => true,
        Result.Err(_) => false,
        _ => false,
    }
    if ok {
        println("json_test ok")
        return 0
    }
    println("json_test err")
    return 1
}
