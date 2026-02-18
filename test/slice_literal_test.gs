module main

fn sum(xs: []i32) -> i32 {
    let acc: i32 = 0
    for v in xs {
        acc = acc + v
    }
    return acc
}

fn main() -> i32 {
    let s = []i32{1, 2, 3,}
    if s[1] != 2 {
        return 1
    }
    if slice_len[i32](&s) != 3 {
        return 2
    }
    if sum(s) != 6 {
        return 3
    }
    let e = []i32{}
    if slice_len[i32](&e) != 0 {
        return 4
    }
    return 0
}
