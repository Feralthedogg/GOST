module main

import "std/fmt" as fmt { println }

fn main() -> i32 {
    fmt.println("ok")
    fmt.printf("not allowed")
    return 0
}
