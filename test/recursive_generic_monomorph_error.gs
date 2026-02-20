module main
fn recurse[T](x: T) -> i32 {
    let y = &x
    return recurse[ref[T]](y)
}
fn main() -> i32 {
    return recurse[i32](0)
}
