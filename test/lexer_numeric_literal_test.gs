module main

const A: i64 = 1_000
const B: i64 = 0x2A
const C: i64 = 0b101010
const D: i64 = 0o52
const E: f64 = 1_2.5e+0

fn main() -> i32 {
    let x: i64 = A + B + C + D
    let y: f64 = E + 2.5e1

    if x != 1126 as i64 {
        return 1
    }
    if y != 37.5 as f64 {
        return 2
    }
    return 0
}
