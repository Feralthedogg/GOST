module main

import "std/encoding/json"
import "std/fmt"

fn str_eq(a: string, b: string) -> bool {
    let na: i64 = string_len(a);
    let nb: i64 = string_len(b);
    if na != nb { return false; }
    let i: i64 = string_len("");
    while i < na {
        if string_get(a, i) != string_get(b, i) { return false; }
        i = i + string_len("a");
    }
    return true;
}

fn main() -> i32 {
    let input = "{\"a\":1,\"b\":[true,null]}";
    match parse(input) {
        Result.Ok(v) => {
            let out = stringify(v);
            if str_eq(out, "{\"a\":1,\"b\":[true,null]}") {
                println("encoding/json ok");
                return 0;
            }
            println("encoding/json mismatch");
            return 1;
        },
        Result.Err(_) => {
            println("encoding/json err");
            return 2;
        },
        _ => { return 3; },
    };
    return 4;
}
