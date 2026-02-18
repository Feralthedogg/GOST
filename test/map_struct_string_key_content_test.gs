module main

copy struct Key {
    s: string
    n: i32
}

fn main() -> i32 {
    let m = make_map[Key, i32](4 as i64)
    let s1 = string_concat("a", "bc")
    let s2 = string_concat("ab", "c")
    let k1 = Key { s = s1, n = 7 as i32 }
    let k2 = Key { s = s2, n = 7 as i32 }
    map_set[Key, i32](&mut m, k1, 1 as i32)
    if !map_del[Key, i32](&mut m, k2) {
        return 1
    }
    if map_len[Key, i32](&m) != 0 as i64 {
        return 2
    }
    return 0
}
