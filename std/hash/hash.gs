module hash

private fn fnv_offset() -> u64 {
    return 14695981039346656037 as u64
}

private fn fnv_prime() -> u64 {
    return 1099511628211 as u64
}

fn fnv64(data: string) -> u64 {
    let h: u64 = fnv_offset()
    let i: i64 = 0
    let n: i64 = string_len(data)
    while i < n {
        h = (h ^ (string_get(data, i) as u64)) * fnv_prime()
        i = i + 1
    }
    return h
}

fn crc32(data: string) -> u32 {
    let crc: u32 = 4294967295 as u32
    let i: i64 = 0
    let n: i64 = string_len(data)
    while i < n {
        let c: u32 = crc ^ (string_get(data, i) as u32)
        let k: i32 = 0
        while k < 8 {
            if (c & (1 as u32)) != (0 as u32) {
                c = (c >> 1) ^ (3988292384 as u32)
            } else {
                c = c >> 1
            }
            k = k + 1
        }
        crc = c
        i = i + 1
    }
    return crc ^ (4294967295 as u32)
}

