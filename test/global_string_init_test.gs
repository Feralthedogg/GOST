module main

let HELLO: string = "hello"
let COMBO: string = "go" + "st"

fn main() -> i32 {
    if string_len(HELLO) != 5 as i64 {
        return 1
    }
    if string_len(COMBO) != 4 as i64 {
        return 2
    }
    return 0
}
