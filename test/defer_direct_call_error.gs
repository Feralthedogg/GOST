module main

fn id(x: i32) -> i32 {
    return x
}

fn main() -> i32 {
    let f = id
    defer f(1)
    return 0
}
