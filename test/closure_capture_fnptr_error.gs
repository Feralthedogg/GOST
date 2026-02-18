module main

fn apply(f: fn(i32)->i32, x: i32) -> i32 {
    return f(x)
}

fn main() -> i32 {
    let base: i32 = 10
    let add = |x: i32| x + base
    let out = apply(add, 1)
    if out != 11 {
        return 1
    }
    return 0
}
