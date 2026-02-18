module main

fn apply_fnptr(f: fn(i32)->i32, x: i32) -> i32 {
    return f(x)
}

fn main() -> i32 {
    let base: i32 = 10
    if apply_fnptr(|x: i32| x + base, 1) != 11 {
        return 1
    }
    let add: closure(i32)->i32 = |x: i32| x + base
    if add(2) != 12 {
        return 2
    }
    return 0
}
