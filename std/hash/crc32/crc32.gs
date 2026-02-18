module crc32

copy struct Digest {
    value: u32
}

private fn poly_ieee() -> u32 {
    return 3988292384 as u32 // 0xEDB88320
}

fn new() -> Digest {
    return Digest { value = 4294967295 as u32 }
}

fn reset(d: mutref[Digest]) {
    d.value = 4294967295 as u32
}

private fn update_byte(crc: u32, b: u8) -> u32 {
    let c: u32 = crc ^ (b as u32)
    let i: i32 = 0
    while i < 8 {
        if (c & (1 as u32)) != (0 as u32) {
            c = (c >> 1) ^ poly_ieee()
        } else {
            c = c >> 1
        }
        i = i + 1
    }
    return c
}

fn write_string(d: mutref[Digest], s: string) {
    let i: i64 = 0
    let n: i64 = string_len(s)
    while i < n {
        d.value = update_byte(d.value, string_get(s, i) as u8)
        i = i + 1
    }
}

fn write_bytes(d: mutref[Digest], data: ref[[]u8]) {
    let i: i64 = 0
    let n: i64 = slice_len[u8](data)
    while i < n {
        d.value = update_byte(d.value, slice_get_copy[u8](data, i))
        i = i + 1
    }
}

fn sum32_state(d: Digest) -> u32 {
    return d.value ^ (4294967295 as u32)
}

fn checksum(s: string) -> u32 {
    let d = new()
    write_string(&mut d, s)
    return sum32_state(d)
}

fn checksum_bytes(data: ref[[]u8]) -> u32 {
    let d = new()
    write_bytes(&mut d, data)
    return sum32_state(d)
}

