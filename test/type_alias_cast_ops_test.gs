module main

type ID = i64
type Word = u16

fn mix(x: ID, y: Word) -> i64 {
    let a = x as i64
    let b = y as i64
    return a + b
}

fn main() -> i32 {
    let x = 5 as u32
    x <<= 1 as u32
    x |= 3 as u32
    x &= 7 as u32
    x ^= 1 as u32
    x += 2 as u32
    x -= 1 as u32
    x *= 3 as u32
    x /= 2 as u32
    x %= 3 as u32

    let y = ~x
    let z = (y as i64) + mix(10 as ID, 20 as Word)
    let t = z >> (1 as i64)
    let _dummy = t as i32
    return 0
}
