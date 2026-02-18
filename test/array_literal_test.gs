module main

fn main() -> i32 {
    let a = [1, 2, 3]
    if a[0] != 1 {
        return 1
    }
    if a[2] != 3 {
        return 2
    }

    let b: [3]i32 = [4, 5, 6]
    if b[1] != 5 {
        return 3
    }
    return 0
}
