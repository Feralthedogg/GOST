module main

import "std/encoding/toml"
import "std/fmt"

fn main() -> i32 {
    let input = "title = \"TOML\"\n[owner]\nname = \"Tom\"\nage = 30\nlist = [1, 2, 3]\n"
    match parse(input) {
        Result.Ok(v) => {
            let out = stringify(v)
            if string_len(out) == string_len("") {
                println("encoding/toml empty")
                return 1
            }
            println("encoding/toml ok")
            return 0
        },
        Result.Err(_) => {
            println("encoding/toml err")
            return 2
        },
        _ => { return 3
 },
    }
    return 4
}
