module main

import "std/hash"

fn main() -> i32 {
    if fnv64("abc") == 0 as u64 { return 1 }
    if crc32("abc") == 0 as u32 { return 2 }
    return 0
}

