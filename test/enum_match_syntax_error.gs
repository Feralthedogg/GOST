module main
enum State { A, B, C }
fn main() -> i32 {
    let s = State.A
    match s {
        State.A => 1,
    }
    return 0
}
