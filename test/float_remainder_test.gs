module main

fn main() -> i32 {
    let a: f64 = 7.5 % 2.0
    if a != 1.5 {
        return 1
    }

    let b: f64 = 9.0
    b %= 4.0
    if b != 1.0 {
        return 2
    }

    let c: f32 = 8.5 as f32
    c %= 2.0 as f32
    if c != 0.5 as f32 {
        return 3
    }

    return 0
}
