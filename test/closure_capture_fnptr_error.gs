module main

fn apply_closure(f: closure(i32)->i32, x: i32) -> i32 {
    return f(x)
}

fn apply_fnptr(f: fn(i32)->i32, x: i32) -> i32 {
    return f(x)
}

fn main() -> i32 {
    let base: i32 = 10
    let add: closure(i32)->i32 = |x: i32| x + base
    if apply_closure(add, 1) != 11 {
        return 1
    }
    if apply_fnptr(add, 1) != 11 {
        return 2
    }
    return 0
}
