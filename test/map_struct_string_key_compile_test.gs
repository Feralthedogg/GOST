module main

copy struct Key {
    s: string
    n: i32
}

fn main() -> i32 {
    let m = make_map[Key, i32](4 as i64)
    let s = "abc"
    let k = Key { s = s, n = 7 as i32 }
    map_set[Key, i32](&mut m, k, 1 as i32)
    if map_len[Key, i32](&m) != 1 as i64 {
        return 1
    }
    if !map_del[Key, i32](&mut m, k) {
        return 2
    }
    return 0
}
