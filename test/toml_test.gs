module main

import "std/encoding/toml"
import "std/fmt"

fn main() -> i32 {
    let input = "name = \"gost\"\narr = [1, 2, 3]\n";
    let r = parse(input);
    let ok = false;
    match r {
        Result.Ok(v) => {
            let out = stringify(v);
            ok = string_len(out) > 0;
        },
        Result.Err(_) => { ok = false; },
        _ => { ok = false; },
    };

    if ok {
        println("toml_test ok");
        return 0;
    }

    println("toml_test err");
    return 1;
}
