module filepath

private fn byte(c: string) -> i32 {
    return string_get(c, 0)
}

private fn is_sep_byte(b: i32) -> bool {
    return b == byte("/") || b == byte("\\")
}

private fn is_windows_path_hint(p: string) -> bool {
    let n: i64 = string_len(p)
    if n >= 2 && string_get(p, 1) == byte(":") {
        return true
    }
    if n >= 1 && string_get(p, 0) == byte("\\") {
        return true
    }
    return false
}

private fn is_windows_env() -> bool {
    let wd = __gost_os_getwd()
    if __gost_os_last_status() != 0 {
        return false
    }
    return is_windows_path_hint(wd)
}

fn separator() -> string {
    if is_windows_env() {
        return "\\"
    }
    return "/"
}

fn list_separator() -> string {
    if is_windows_env() {
        return ";"
    }
    return ":"
}

fn to_slash(p: string) -> string {
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

fn from_slash(p: string) -> string {
    if !is_windows_env() {
        return p
    }
    let n: i64 = string_len(p)
    let out: string = ""
    let i: i64 = 0
    while i < n {
        let b: i32 = string_get(p, i)
        if b == byte("/") {
            out = string_concat(out, "\\")
        } else {
            out = string_concat(out, string_slice(p, i, 1))
        }
        i = i + 1
    }
    return out
}

fn volume_name(path: string) -> string {
    let n: i64 = string_len(path)
    if n >= 2 && string_get(path, 1) == byte(":") {
        return string_slice(path, 0, 2)
    }
    return ""
}

fn is_abs(path: string) -> bool {
    let p = to_slash(path)
    let n: i64 = string_len(p)
    if n == 0 {
        return false
    }
    if string_get(p, 0) == byte("/") {
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
    let p = to_slash(path)
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
        return from_slash(string_slice(out, 0, m - 1))
    }
    return from_slash(out)
}

fn base(path: string) -> string {
    let p = to_slash(clean(path))
    let n: i64 = string_len(p)
    if n == 0 {
        return "."
    }
    let i: i64 = n
    while i > 0 {
        let j: i64 = i - 1
        if string_get(p, j) == byte("/") {
            return string_slice(path, j + 1, n - (j + 1))
        }
        i = j
    }
    return from_slash(p)
}

fn dir(path: string) -> string {
    let p = to_slash(clean(path))
    let n: i64 = string_len(p)
    if n == 0 {
        return "."
    }
    let i: i64 = n
    while i > 0 {
        let j: i64 = i - 1
        if string_get(p, j) == byte("/") {
            if j == 0 {
                return from_slash("/")
            }
            return from_slash(string_slice(p, 0, j))
        }
        i = j
    }
    return "."
}

fn ext(path: string) -> string {
    let b = base(path)
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
    let aa = to_slash(a)
    let bb = to_slash(b)
    let out = aa
    if string_get(aa, string_len(aa) - 1) != byte("/") {
        out = string_concat(out, "/")
    }
    out = string_concat(out, bb)
    return clean(out)
}

fn join(parts: []string) -> string {
    let n: i64 = slice_len[string](&parts)
    if n == 0 {
        return ""
    }
    let out: string = ""
    let i: i64 = 0
    while i < n {
        let p = slice_get_copy[string](&parts, i)
        if string_len(out) == 0 {
            out = p
        } else {
            out = join2(out, p)
        }
        i = i + 1
    }
    return clean(out)
}

