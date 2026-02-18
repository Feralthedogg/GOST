module main

fn id[T](v: T) -> T {
    return v
}

fn pair_left[T, U](a: T, _b: U) -> T {
    return a
}

fn main() -> i32 {
    let a = id[i64](42 as i64)
    let b = id[u32](7 as u32)
    let c = pair_left[i64, u32](a, b)
    if c == 42 as i64 {
        return 0
    }
    return 1
}
