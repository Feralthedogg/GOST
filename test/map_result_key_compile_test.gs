module main

fn main() -> i32 {
    let m = make_map[Result[i32, error], i32](4 as i64)
    let k1 = Result.Ok[i32, error](3 as i32)
    let k2 = Result.Ok[i32, error](3 as i32)
    map_set[Result[i32, error], i32](&mut m, k1, 9 as i32)
    if !map_del[Result[i32, error], i32](&mut m, k2) {
        return 1
    }
    if map_len[Result[i32, error], i32](&m) != 0 as i64 {
        return 2
    }
    return 0
}
