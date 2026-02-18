module main

copy struct Pair {
    a: i32
    b: i32
}

fn pair(a: i32, b: i32) -> Pair {
    return Pair { a = a, b = b }
}

fn main() -> i32 {
    let m = make_map[Pair, i32](2 as i64)

    let i: i32 = 0
    while i < 300 {
        let k = pair(i, i + 1)
        map_set[Pair, i32](&mut m, k, i + 10 as i32)
        i = i + 1
    }
    if map_len[Pair, i32](&m) != 300 as i64 {
        return 1
    }

    i = 0
    while i < 300 {
        let k = pair(i, i + 1)
        map_set[Pair, i32](&mut m, k, i + 1000 as i32)
        i = i + 1
    }
    if map_len[Pair, i32](&m) != 300 as i64 {
        return 2
    }

    let deleted: i32 = 0
    i = 0
    while i < 300 {
        if i % 2 as i32 == 0 as i32 {
            let k = pair(i, i + 1)
            if !map_del[Pair, i32](&mut m, k) {
                return 3
            }
            if map_del[Pair, i32](&mut m, k) {
                return 4
            }
            deleted = deleted + 1
        }
        i = i + 1
    }
    if deleted != 150 {
        return 5
    }
    if map_len[Pair, i32](&m) != 150 as i64 {
        return 6
    }

    i = 1
    while i < 300 {
        let k = pair(i, i + 1)
        if !map_del[Pair, i32](&mut m, k) {
            return 7
        }
        i = i + 2 as i32
    }
    if map_len[Pair, i32](&m) != 0 as i64 {
        return 8
    }

    return 0
}
