module path

private fn byte(c: string) -> i32 {
    return string_get(c, 0)
}

private fn str_eq(a: string, b: string) -> bool {
    let na: i64 = string_len(a)
    let nb: i64 = string_len(b)
    if na != nb {
        return false
    }
    let i: i64 = 0
    while i < na {
        if string_get(a, i) != string_get(b, i) {
            return false
        }
        i = i + 1
    }
    return true
}

private fn is_sep_byte(b: i32) -> bool {
    return b == byte("/") || b == byte("\\")
}

private fn normalize_sep(p: string) -> string {
    let n: i64 = string_len(p)
    let out: string = ""
    let i: i64 = 0
    while i < n {
        let b: i32 = string_get(p, i)
        if b == byte("\\") {
            out = string_concat(out, "/")
        } else {
            out = string_concat(out, string_slice(p, i, 1))
        }
        i = i + 1
    }
    return out
}

private fn is_root(p: string) -> bool {
    return string_len(p) == 1 && string_get(p, 0) == byte("/")
}

fn is_abs(p: string) -> bool {
    let n: i64 = string_len(p)
    if n == 0 {
        return false
    }
    if is_sep_byte(string_get(p, 0)) {
        return true
    }
    if n >= 2 && string_get(p, 1) == byte(":") {
        return true
    }
    return false
}

fn clean(path: string) -> string {
    if string_len(path) == 0 {
        return "."
    }
    let p: string = normalize_sep(path)
    let n: i64 = string_len(p)
    let out: string = ""
    let i: i64 = 0
    let prev_sep: bool = false
    while i < n {
        let b: i32 = string_get(p, i)
        if b == byte("/") {
            if prev_sep {
                i = i + 1
                continue
            }
            prev_sep = true
            out = string_concat(out, "/")
            i = i + 1
            continue
        }
        prev_sep = false
        out = string_concat(out, string_slice(p, i, 1))
        i = i + 1
    }
    let m: i64 = string_len(out)
    if m == 0 {
        return "."
    }
    if m > 1 && string_get(out, m - 1) == byte("/") {
        return string_slice(out, 0, m - 1)
    }
    return out
}

fn base(path: string) -> string {
    let p: string = clean(path)
    if is_root(p) {
        return "/"
    }
    let n: i64 = string_len(p)
    let i: i64 = n
    while i > 0 {
        let j: i64 = i - 1
        if string_get(p, j) == byte("/") {
            return string_slice(p, j + 1, n - (j + 1))
        }
        i = j
    }
    return p
}

fn dir(path: string) -> string {
    let p: string = clean(path)
    if is_root(p) {
        return "/"
    }
    let n: i64 = string_len(p)
    let i: i64 = n
    while i > 0 {
        let j: i64 = i - 1
        if string_get(p, j) == byte("/") {
            if j == 0 {
                return "/"
            }
            return string_slice(p, 0, j)
        }
        i = j
    }
    return "."
}

fn ext(path: string) -> string {
    let b: string = base(path)
    let n: i64 = string_len(b)
    let i: i64 = n
    while i > 0 {
        let j: i64 = i - 1
        let c: i32 = string_get(b, j)
        if c == byte(".") {
            if j == 0 {
                return ""
            }
            return string_slice(b, j, n - j)
        }
        i = j
    }
    return ""
}

fn join2(a: string, b: string) -> string {
    if string_len(a) == 0 {
        return clean(b)
    }
    if string_len(b) == 0 {
        return clean(a)
    }
    let aa: string = normalize_sep(a)
    let bb: string = normalize_sep(b)
    let na: i64 = string_len(aa)
    let out: string = aa
    if na > 0 && string_get(aa, na - 1) != byte("/") {
        out = string_concat(out, "/")
    }
    out = string_concat(out, bb)
    return clean(out)
}

private fn join_from(parts: ref[[]string], idx: i64, acc: string) -> string {
    let n: i64 = slice_len[string](parts)
    if idx >= n {
        return clean(acc)
    }
    let p = slice_get_copy[string](parts, idx)
    let next = acc
    if string_len(next) == 0 {
        next = p
    } else {
        next = join2(next, p)
    }
    return join_from(parts, idx + 1, next)
}

fn join(parts: []string) -> string {
    return join_from(&parts, 0, "")
}

fn split(path: string) -> []string {
    let _ = path
    return make_slice[string](0, 0)
}

fn walk(root: string, visit: fn(string) -> unit) {
    let c = clean(root)
    visit(c)
}
