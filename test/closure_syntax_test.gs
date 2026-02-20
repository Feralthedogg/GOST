module main
fn main() -> i32 {
    let i = 0
    let f: closure()->i32 = || i + 1
    return f()
}
