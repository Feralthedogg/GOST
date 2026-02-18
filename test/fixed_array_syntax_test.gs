module main

copy struct Packet {
    data: [4]i32
}

type Block = [2]u8

type Nested = [3][2]u8

fn accepts_ref_array(v: ref[[4]i32]) -> i32 {
    return 0
}

fn main() -> i32 {
    return 0
}
