module strings

private fn strings_zero_i64() -> i64 { return string_len("")
 }
private fn strings_one_i64() -> i64 { return string_len("a")
 }
private fn strings_byte(c: string) -> i32 { return string_get(c, strings_zero_i64())
 }

fn len(s: string) -> i64 {
    return string_len(s)
}

fn is_empty(s: string) -> bool {
    return string_len(s) == strings_zero_i64()
}

private fn eq(a: string, b: string) -> bool {
    let na: i64 = string_len(a)
    let nb: i64 = string_len(b)
    if na != nb {
        return false
    }
    let i: i64 = strings_zero_i64()
    while i < na {
        if string_get(a, i) != string_get(b, i) {
            return false
        }
        i = i + strings_one_i64()
    }
    return true
}

fn has_prefix(s: string, prefix: string) -> bool {
    let ns: i64 = string_len(s)
    let np: i64 = string_len(prefix)
    if np > ns {
        return false
    }
    let i: i64 = strings_zero_i64()
    while i < np {
        if string_get(s, i) != string_get(prefix, i) {
            return false
        }
        i = i + strings_one_i64()
    }
    return true
}

fn has_suffix(s: string, suffix: string) -> bool {
    let ns: i64 = string_len(s)
    let np: i64 = string_len(suffix)
    if np > ns {
        return false
    }
    let off: i64 = ns - np
    let i: i64 = strings_zero_i64()
    while i < np {
        if string_get(s, off + i) != string_get(suffix, i) {
            return false
        }
        i = i + strings_one_i64()
    }
    return true
}

private fn strings_index_from(s: string, sub: string, start: i64) -> i64 {
    let ns: i64 = string_len(s)
    let nm: i64 = string_len(sub)
    if start < strings_zero_i64() {
        return strings_zero_i64() - strings_one_i64()
    }
    if nm == strings_zero_i64() {
        if start > ns {
            return ns
        }
        return start
    }
    if start >= ns || nm > (ns - start) {
        return strings_zero_i64() - strings_one_i64()
    }
    let i: i64 = start
    let end: i64 = ns - nm
    while i <= end {
        let j: i64 = strings_zero_i64()
        let ok: bool = true
        while j < nm {
            if string_get(s, i + j) != string_get(sub, j) {
                ok = false
                break
            }
            j = j + strings_one_i64()
        }
        if ok {
            return i
        }
        i = i + strings_one_i64()
    }
    return strings_zero_i64() - strings_one_i64()
}

fn index(s: string, sub: string) -> i64 {
    return strings_index_from(s, sub, strings_zero_i64())
}

fn contains(s: string, sub: string) -> bool {
    return index(s, sub) >= strings_zero_i64()
}

fn count(s: string, sub: string) -> i32 {
    let ns: i64 = string_len(s)
    let nm: i64 = string_len(sub)
    if nm == strings_zero_i64() {
        return (ns + strings_one_i64()) as i32
    }
    if ns == strings_zero_i64() || nm > ns {
        return 0
    }
    let c: i64 = strings_zero_i64()
    let i: i64 = strings_zero_i64()
    while i <= (ns - nm) {
        let at: i64 = strings_index_from(s, sub, i)
        if at < strings_zero_i64() {
            break
        }
        c = c + strings_one_i64()
        i = at + nm
    }
    return c as i32
}

private fn split_each_byte(s: string) -> []string {
    let ns: i64 = string_len(s)
    let out_chars = make_slice[string](strings_zero_i64(), strings_zero_i64())
    let i: i64 = strings_zero_i64()
    while i < ns {
        slice_push[string](&mut out_chars, string_slice(s, i, strings_one_i64()))
        i = i + strings_one_i64()
    }
    return out_chars
}

private fn split_with_sep(s: string, sep: string) -> []string {
    let ns: i64 = string_len(s)
    let nm: i64 = string_len(sep)
    let out1 = make_slice[string](strings_zero_i64(), strings_zero_i64())
    let start: i64 = strings_zero_i64()
    let at: i64 = strings_index_from(s, sep, start)
    while at >= strings_zero_i64() {
        slice_push[string](&mut out1, string_slice(s, start, at - start))
        start = at + nm
        at = strings_index_from(s, sep, start)
    }
    slice_push[string](&mut out1, string_slice(s, start, ns - start))
    return out1
}

fn split(s: string, sep: string) -> []string {
    if string_len(sep) == strings_zero_i64() {
        return split_each_byte(s)
    }
    return split_with_sep(s, sep)
}

fn join(parts: []string, sep: string) -> string {
    let n: i64 = slice_len[string](&parts)
    if n == strings_zero_i64() {
        return ""
    }
    let out: string = ""
    let i: i64 = strings_zero_i64()
    while i < n {
        if i > strings_zero_i64() {
            out = string_concat(out, sep)
        }
        out = string_concat(out, slice_get_copy[string](&parts, i))
        i = i + strings_one_i64()
    }
    return out
}

fn replace(s: string, old: string, new: string) -> string {
    let ns: i64 = string_len(s)
    let no: i64 = string_len(old)
    if no == strings_zero_i64() {
        return s
    }
    let out: string = ""
    let start: i64 = strings_zero_i64()
    let at: i64 = strings_index_from(s, old, start)
    while at >= strings_zero_i64() {
        out = string_concat(out, string_slice(s, start, at - start))
        out = string_concat(out, new)
        start = at + no
        at = strings_index_from(s, old, start)
    }
    if start < ns {
        out = string_concat(out, string_slice(s, start, ns - start))
    }
    return out
}

private fn strings_is_space(b: i32) -> bool {
    if b == strings_byte(" ") { return true
 }
    if b == strings_byte("\t") { return true
 }
    if b == strings_byte("\n") { return true
 }
    if b == strings_byte("\r") { return true
 }
    return false
}

fn trim_space(s: string) -> string {
    let n: i64 = string_len(s)
    if n == strings_zero_i64() {
        return s
    }
    let left: i64 = strings_zero_i64()
    while left < n {
        if !strings_is_space(string_get(s, left)) {
            break
        }
        left = left + strings_one_i64()
    }
    if left >= n {
        return ""
    }
    let right: i64 = n - strings_one_i64()
    while right >= left {
        if !strings_is_space(string_get(s, right)) {
            break
        }
        if right == strings_zero_i64() {
            return ""
        }
        right = right - strings_one_i64()
    }
    return string_slice(s, left, right - left + strings_one_i64())
}

fn to_upper(s: string) -> string {
    let n: i64 = string_len(s)
    let out: string = ""
    let i: i64 = strings_zero_i64()
    while i < n {
        let b: i32 = string_get(s, i)
        if b >= strings_byte("a") && b <= strings_byte("z") {
            out = string_concat(out, string_from_byte(b - 32))
        } else {
            out = string_concat(out, string_slice(s, i, strings_one_i64()))
        }
        i = i + strings_one_i64()
    }
    return out
}

fn to_lower(s: string) -> string {
    let n: i64 = string_len(s)
    let out: string = ""
    let i: i64 = strings_zero_i64()
    while i < n {
        let b: i32 = string_get(s, i)
        if b >= strings_byte("A") && b <= strings_byte("Z") {
            out = string_concat(out, string_from_byte(b + 32))
        } else {
            out = string_concat(out, string_slice(s, i, strings_one_i64()))
        }
        i = i + strings_one_i64()
    }
    return out
}

fn repeat(s: string, count: i32) -> string {
    if count <= 0 {
        return ""
    }
    let out: string = ""
    let i: i32 = 0
    while i < count {
        out = string_concat(out, s)
        i = i + 1
    }
    return out
}

