module main

import "std/encoding/yaml"
import "std/fmt"

fn main() -> i32 {
    let input = "a: 1\nb:\n  - true\n  - null\n";
    match parse(input) {
        Result.Ok(v) => {
            let out = stringify(v);
            if string_len(out) == string_len("") {
                println("encoding/yaml empty");
                return 1;
            }
            // round-trip parse
            match parse(out) {
                Result.Ok(_) => {
                    println("encoding/yaml ok");
                    return 0;
                },
                Result.Err(_) => {
                    println("encoding/yaml roundtrip err");
                    return 2;
                },
                _ => { return 3; },
            };
        },
        Result.Err(_) => {
            println("encoding/yaml err");
            return 4;
        },
        _ => { return 5; },
    };
    return 6;
}
