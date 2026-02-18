module fnv

copy struct Hash64 {
    state: u64
}

private fn offset_basis_64() -> u64 {
    return 14695981039346656037 as u64
}

private fn prime_64() -> u64 {
    return 1099511628211 as u64
}

fn new64() -> Hash64 {
    return Hash64 { state = offset_basis_64() }
}

fn reset64(h: mutref[Hash64]) {
    h.state = offset_basis_64()
}

private fn write_u8(h: mutref[Hash64], b: u8) {
    h.state = (h.state ^ (b as u64)) * prime_64()
}

fn write_string(h: mutref[Hash64], s: string) {
    let i: i64 = 0
    let n: i64 = string_len(s)
    while i < n {
        write_u8(h, string_get(s, i) as u8)
        i = i + 1
    }
}

fn write_bytes(h: mutref[Hash64], data: ref[[]u8]) {
    let i: i64 = 0
    let n: i64 = slice_len[u8](data)
    while i < n {
        write_u8(h, slice_get_copy[u8](data, i))
        i = i + 1
    }
}

fn sum64_state(h: Hash64) -> u64 {
    return h.state
}

fn sum64(s: string) -> u64 {
    let h = new64()
    write_string(&mut h, s)
    return h.state
}

fn sum64_bytes(data: ref[[]u8]) -> u64 {
    let h = new64()
    write_bytes(&mut h, data)
    return h.state
}

