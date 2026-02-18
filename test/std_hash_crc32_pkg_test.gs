module main

import "std/hash/crc32"

fn main() -> i32 {
    if checksum("abc") == 0 as u32 { return 1 }
    let d = new()
    write_string(&mut d, "abc")
    if sum32_state(d) != checksum("abc") { return 2 }
    return 0
}

