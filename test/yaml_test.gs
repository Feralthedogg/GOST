module main

import "std/encoding/yaml"
import "std/fmt"

fn main() -> i32 {
    let input = "a: 1\nb:\n  - true\n  - null\n"
    let r = parse(input)
    let ok = match r {
        Result.Ok(_) => true,
        Result.Err(_) => false,
        _ => false,
    }
    if ok {
        println("yaml_test ok")
        return 0
    }
    println("yaml_test err")
    return 1
}
