module main

import "std/encoding/yaml"
import "std/fmt"

fn main() -> i32 {
    let input = "a: 1\nb:\n  - true\n  - null\n";
    match parse(input) {
        Result.Ok(_) => {
            println("yaml_test ok");
            return 0;
        },
        Result.Err(_) => {
            println("yaml_test err");
            return 1;
        },
        _ => { return 2; },
    };
    return 3;
}
