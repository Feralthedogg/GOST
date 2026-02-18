module main

fn id[T](x: T) -> T {
    return x
}

fn main() -> i32 {
    defer id[i32](1)

    let v = id[i32](2)
    if v != 2 {
        return 1
    }
    return 0
}
