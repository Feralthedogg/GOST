module bytes

private fn byte(c: string) -> i32 {
    return string_get(c, 0)
}

fn len(b: []u8) -> i64 {
    return slice_len[u8](&b)
}

fn clone(src: []u8) -> []u8 {
    let n: i64 = slice_len[u8](&src)
    let out = make_slice[u8](0, n)
    let i: i64 = 0
    while i < n {
        slice_push[u8](&mut out, slice_get_copy[u8](&src, i))
        i = i + 1
    }
    return out
}

fn from_string(s: string) -> []u8 {
    let n: i64 = string_len(s)
    let out = make_slice[u8](0, n)
    let i: i64 = 0
    while i < n {
        slice_push[u8](&mut out, string_get(s, i) as u8)
        i = i + 1
    }
    return out
}

fn to_string(b: []u8) -> string {
    let n: i64 = slice_len[u8](&b)
    let out: string = ""
    let i: i64 = 0
    while i < n {
        out = string_concat(out, string_from_byte(slice_get_copy[u8](&b, i) as i32))
        i = i + 1
    }
    return out
}

private fn to_string_ref(b: ref[[]u8]) -> string {
    let n: i64 = slice_len[u8](b)
    let out: string = ""
    let i: i64 = 0
    while i < n {
        out = string_concat(out, string_from_byte(slice_get_copy[u8](b, i) as i32))
        i = i + 1
    }
    return out
}

fn equal(a: ref[[]u8], b: ref[[]u8]) -> bool {
    let na: i64 = slice_len[u8](a)
    let nb: i64 = slice_len[u8](b)
    if na != nb {
        return false
    }
    let i: i64 = 0
    while i < na {
        if slice_get_copy[u8](a, i) != slice_get_copy[u8](b, i) {
            return false
        }
        i = i + 1
    }
    return true
}

fn has_prefix(data: ref[[]u8], prefix: ref[[]u8]) -> bool {
    let nd: i64 = slice_len[u8](data)
    let np: i64 = slice_len[u8](prefix)
    if np > nd {
        return false
    }
    let i: i64 = 0
    while i < np {
        if slice_get_copy[u8](data, i) != slice_get_copy[u8](prefix, i) {
            return false
        }
        i = i + 1
    }
    return true
}

fn has_suffix(data: ref[[]u8], suffix: ref[[]u8]) -> bool {
    let nd: i64 = slice_len[u8](data)
    let ns: i64 = slice_len[u8](suffix)
    if ns > nd {
        return false
    }
    let off: i64 = nd - ns
    let i: i64 = 0
    while i < ns {
        if slice_get_copy[u8](data, off + i) != slice_get_copy[u8](suffix, i) {
            return false
        }
        i = i + 1
    }
    return true
}

fn index(data: ref[[]u8], sub: ref[[]u8]) -> i64 {
    let nd: i64 = slice_len[u8](data)
    let ns: i64 = slice_len[u8](sub)
    if ns == 0 {
        return 0
    }
    if ns > nd {
        return -1 as i64
    }
    let i: i64 = 0
    while i <= nd - ns {
        let j: i64 = 0
        let ok: bool = true
        while j < ns {
            if slice_get_copy[u8](data, i + j) != slice_get_copy[u8](sub, j) {
                ok = false
                break
            }
            j = j + 1
        }
        if ok {
            return i
        }
        i = i + 1
    }
    return -1 as i64
}

private fn index_from(data: ref[[]u8], sub: ref[[]u8], start: i64) -> i64 {
    let nd: i64 = slice_len[u8](data)
    let ns: i64 = slice_len[u8](sub)
    if start < 0 {
        return -1 as i64
    }
    if ns == 0 {
        if start > nd {
            return nd
        }
        return start
    }
    if start >= nd || ns > (nd - start) {
        return -1 as i64
    }
    let i: i64 = start
    while i <= nd - ns {
        let j: i64 = 0
        let ok: bool = true
        while j < ns {
            if slice_get_copy[u8](data, i + j) != slice_get_copy[u8](sub, j) {
                ok = false
                break
            }
            j = j + 1
        }
        if ok {
            return i
        }
        i = i + 1
    }
    return -1 as i64
}

private fn string_index_from(s: string, sub: string, start: i64) -> i64 {
    let ns: i64 = string_len(s)
    let nm: i64 = string_len(sub)
    if start < 0 {
        return -1 as i64
    }
    if nm == 0 {
        if start > ns {
            return ns
        }
        return start
    }
    if start >= ns || nm > (ns - start) {
        return -1 as i64
    }
    let i: i64 = start
    while i <= ns - nm {
        let j: i64 = 0
        let ok: bool = true
        while j < nm {
            if string_get(s, i + j) != string_get(sub, j) {
                ok = false
                break
            }
            j = j + 1
        }
        if ok {
            return i
        }
        i = i + 1
    }
    return -1 as i64
}

fn contains(data: ref[[]u8], sub: ref[[]u8]) -> bool {
    return index(data, sub) >= 0
}

fn repeat(data: []u8, count: i32) -> []u8 {
    if count <= 0 {
        return make_slice[u8](0, 0)
    }
    let n: i64 = slice_len[u8](&data)
    let out = make_slice[u8](0, n * (count as i64))
    let i: i32 = 0
    while i < count {
        let j: i64 = 0
        while j < n {
            slice_push[u8](&mut out, slice_get_copy[u8](&data, j))
            j = j + 1
        }
        i = i + 1
    }
    return out
}

private fn is_space(b: u8) -> bool {
    let x: i32 = b as i32
    if x == byte(" ") { return true }
    if x == byte("\t") { return true }
    if x == byte("\n") { return true }
    if x == byte("\r") { return true }
    return false
}

fn trim_space(data: ref[[]u8]) -> []u8 {
    let n: i64 = slice_len[u8](data)
    if n == 0 {
        return make_slice[u8](0, 0)
    }
    let left: i64 = 0
    while left < n && is_space(slice_get_copy[u8](data, left)) {
        left = left + 1
    }
    if left >= n {
        return make_slice[u8](0, 0)
    }
    let right: i64 = n - 1
    while right > left && is_space(slice_get_copy[u8](data, right)) {
        right = right - 1
    }
    let out_s: string = ""
    let i: i64 = left
    while i <= right {
        out_s = string_concat(out_s, string_from_byte(slice_get_copy[u8](data, i) as i32))
        i = i + 1
    }
    return from_string(out_s)
}

fn to_upper_ascii(data: ref[[]u8]) -> []u8 {
    let n: i64 = slice_len[u8](data)
    let out = make_slice[u8](0, n)
    let i: i64 = 0
    while i < n {
        let b: i32 = slice_get_copy[u8](data, i) as i32
        if b >= byte("a") && b <= byte("z") {
            slice_push[u8](&mut out, (b - 32) as u8)
        } else {
            slice_push[u8](&mut out, b as u8)
        }
        i = i + 1
    }
    return out
}

fn to_lower_ascii(data: ref[[]u8]) -> []u8 {
    let n: i64 = slice_len[u8](data)
    let out = make_slice[u8](0, n)
    let i: i64 = 0
    while i < n {
        let b: i32 = slice_get_copy[u8](data, i) as i32
        if b >= byte("A") && b <= byte("Z") {
            slice_push[u8](&mut out, (b + 32) as u8)
        } else {
            slice_push[u8](&mut out, b as u8)
        }
        i = i + 1
    }
    return out
}

fn replace(data: ref[[]u8], old: ref[[]u8], new: ref[[]u8]) -> []u8 {
    let src: string = to_string_ref(data)
    let olds: string = to_string_ref(old)
    let news: string = to_string_ref(new)
    let ns: i64 = string_len(src)
    let no: i64 = string_len(olds)
    if no == 0 {
        return from_string(src)
    }
    let out: string = ""
    let start: i64 = 0
    let at: i64 = string_index_from(src, olds, start)
    while at >= 0 {
        out = string_concat(out, string_slice(src, start, at - start))
        out = string_concat(out, news)
        start = at + no
        at = string_index_from(src, olds, start)
    }
    if start < ns {
        out = string_concat(out, string_slice(src, start, ns - start))
    }
    return from_string(out)
}
