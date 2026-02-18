module sha256

private fn rotr(x: u32, n: i32) -> u32 {
    return (x >> n) | (x << (32 - n))
}

private fn ch(x: u32, y: u32, z: u32) -> u32 {
    return (x & y) ^ ((~x) & z)
}

private fn maj(x: u32, y: u32, z: u32) -> u32 {
    return (x & y) ^ (x & z) ^ (y & z)
}

private fn big_sigma0(x: u32) -> u32 {
    return rotr(x, 2) ^ rotr(x, 13) ^ rotr(x, 22)
}

private fn big_sigma1(x: u32) -> u32 {
    return rotr(x, 6) ^ rotr(x, 11) ^ rotr(x, 25)
}

private fn small_sigma0(x: u32) -> u32 {
    return rotr(x, 7) ^ rotr(x, 18) ^ (x >> 3)
}

private fn small_sigma1(x: u32) -> u32 {
    return rotr(x, 17) ^ rotr(x, 19) ^ (x >> 10)
}

private fn k_table() -> [64]u32 {
    return [
        1116352408 as u32, 1899447441 as u32, 3049323471 as u32, 3921009573 as u32,
        961987163 as u32, 1508970993 as u32, 2453635748 as u32, 2870763221 as u32,
        3624381080 as u32, 310598401 as u32, 607225278 as u32, 1426881987 as u32,
        1925078388 as u32, 2162078206 as u32, 2614888103 as u32, 3248222580 as u32,
        3835390401 as u32, 4022224774 as u32, 264347078 as u32, 604807628 as u32,
        770255983 as u32, 1249150122 as u32, 1555081692 as u32, 1996064986 as u32,
        2554220882 as u32, 2821834349 as u32, 2952996808 as u32, 3210313671 as u32,
        3336571891 as u32, 3584528711 as u32, 113926993 as u32, 338241895 as u32,
        666307205 as u32, 773529912 as u32, 1294757372 as u32, 1396182291 as u32,
        1695183700 as u32, 1986661051 as u32, 2177026350 as u32, 2456956037 as u32,
        2730485921 as u32, 2820302411 as u32, 3259730800 as u32, 3345764771 as u32,
        3516065817 as u32, 3600352804 as u32, 4094571909 as u32, 275423344 as u32,
        430227734 as u32, 506948616 as u32, 659060556 as u32, 883997877 as u32,
        958139571 as u32, 1322822218 as u32, 1537002063 as u32, 1747873779 as u32,
        1955562222 as u32, 2024104815 as u32, 2227730452 as u32, 2361852424 as u32,
        2428436474 as u32, 2756734187 as u32, 3204031479 as u32, 3329325298 as u32,
    ]
}

private fn hex_digit(v: i32) -> string {
    if v < 10 {
        return string_from_byte(string_get("0", 0) + v)
    }
    return string_from_byte(string_get("a", 0) + (v - 10))
}

private fn append_u32_hex(out: string, v: u32) -> string {
    let s: i32 = 28
    while s >= 0 {
        let nib: i32 = ((v >> s) & (15 as u32)) as i32
        out = string_concat(out, hex_digit(nib))
        s = s - 4
    }
    return out
}

private fn zero_words() -> [64]u32 {
    return [
        0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32,
        0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32,
        0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32,
        0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32,
        0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32,
        0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32,
        0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32,
        0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32, 0 as u32,
    ]
}

fn sum_hex(data: string) -> string {
    let msg = make_slice[u8](0, 0)
    let i: i64 = 0
    while i < string_len(data) {
        slice_push[u8](&mut msg, string_get(data, i) as u8)
        i = i + 1
    }

    let bit_len: u64 = (slice_len[u8](&msg) * 8) as u64
    slice_push[u8](&mut msg, 128 as u8)
    while (slice_len[u8](&msg) % 64) != 56 {
        slice_push[u8](&mut msg, 0 as u8)
    }

    let shift: i64 = 56
    while shift >= 0 {
        slice_push[u8](&mut msg, ((bit_len >> shift) & (255 as u64)) as u8)
        shift = shift - 8
    }

    let h0: u32 = 1779033703 as u32
    let h1: u32 = 3144134277 as u32
    let h2: u32 = 1013904242 as u32
    let h3: u32 = 2773480762 as u32
    let h4: u32 = 1359893119 as u32
    let h5: u32 = 2600822924 as u32
    let h6: u32 = 528734635 as u32
    let h7: u32 = 1541459225 as u32

    let k = k_table()
    let off: i64 = 0
    while off < slice_len[u8](&msg) {
        let w = zero_words()

        let t: i64 = 0
        while t < 16 {
            let b0: u32 = slice_get_copy[u8](&msg, off + (t * 4)) as u32
            let b1: u32 = slice_get_copy[u8](&msg, off + (t * 4) + 1) as u32
            let b2: u32 = slice_get_copy[u8](&msg, off + (t * 4) + 2) as u32
            let b3: u32 = slice_get_copy[u8](&msg, off + (t * 4) + 3) as u32
            w[t] = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3
            t = t + 1
        }

        while t < 64 {
            let s0: u32 = small_sigma0(w[t - 15])
            let s1: u32 = small_sigma1(w[t - 2])
            w[t] = w[t - 16] + s0 + w[t - 7] + s1
            t = t + 1
        }

        let a: u32 = h0
        let b: u32 = h1
        let c: u32 = h2
        let d: u32 = h3
        let e: u32 = h4
        let f: u32 = h5
        let g: u32 = h6
        let hh: u32 = h7

        let r: i64 = 0
        while r < 64 {
            let t1: u32 = hh + big_sigma1(e) + ch(e, f, g) + k[r] + w[r]
            let t2: u32 = big_sigma0(a) + maj(a, b, c)
            hh = g
            g = f
            f = e
            e = d + t1
            d = c
            c = b
            b = a
            a = t1 + t2
            r = r + 1
        }

        h0 = h0 + a
        h1 = h1 + b
        h2 = h2 + c
        h3 = h3 + d
        h4 = h4 + e
        h5 = h5 + f
        h6 = h6 + g
        h7 = h7 + hh

        off = off + 64
    }

    let out: string = ""
    out = append_u32_hex(out, h0)
    out = append_u32_hex(out, h1)
    out = append_u32_hex(out, h2)
    out = append_u32_hex(out, h3)
    out = append_u32_hex(out, h4)
    out = append_u32_hex(out, h5)
    out = append_u32_hex(out, h6)
    out = append_u32_hex(out, h7)
    return out
}

fn sum(data: string) -> string {
    return sum_hex(data)
}
