module main

fn id[T](x: T) -> T {
    return x
}

fn main() -> i32 {
    go id[i32](1)
    return 0
}
