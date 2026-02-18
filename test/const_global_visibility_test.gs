module main

import "std/fmt" as fmt { println }

const BASE: i64 = 40
let COUNTER: i64 = 2

fn main() -> i32 {
    const INC: i64 = 1
    COUNTER = COUNTER + INC
    let total: i64 = BASE + COUNTER
    if total == 43 {
        fmt.println("large feature ok")
        return 0
    }
    return 1
}
