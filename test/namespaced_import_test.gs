module main

import "std/fmt" as fmt { println }

fn main() -> i32 {
    fmt.println("namespaced import ok")
    return 0
}
