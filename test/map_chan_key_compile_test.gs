module main

fn main() -> i32 {
    let m = make_map[chan[i32], i32](2 as i64)
    let ch = make_chan[i32](1 as i64)
    map_set[chan[i32], i32](&mut m, ch, 1 as i32)
    if map_len[chan[i32], i32](&m) != 1 as i64 {
        return 1
    }
    if !map_del[chan[i32], i32](&mut m, ch) {
        return 2
    }
    if map_del[chan[i32], i32](&mut m, ch) {
        return 3
    }
    return 0
}
