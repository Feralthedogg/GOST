module regexp

copy struct Regexp {
    pattern: string
}

private fn err(msg: string) -> Result[Regexp, error] {
    return Result.Err[Regexp, error](__gost_error_new(msg))
}

private fn is_quantifier(b: i32) -> bool {
    return b == string_get("*", 0) || b == string_get("?", 0)
}

private fn char_at(s: string, i: i64) -> i32 {
    if i < 0 {
        return -1
    }
    let n: i64 = string_len(s)
    if i >= n {
        return -1
    }
    return string_get(s, i)
}

private fn validate_pattern(pattern: string) -> bool {
    let n: i64 = string_len(pattern)
    if n == 0 {
        return true
    }
    let i: i64 = 0
    while i < n {
        let b: i32 = char_at(pattern, i)
        if is_quantifier(b) {
            if i == 0 {
                return false
            }
            let prev: i32 = char_at(pattern, i - 1)
            if is_quantifier(prev) {
                return false
            }
        }
        i = i + 1
    }
    return true
}

fn compile(pattern: string) -> Result[Regexp, error] {
    if !validate_pattern(pattern) {
        return err("invalid regexp pattern")
    }
    return Result.Ok[Regexp, error](Regexp { pattern = pattern })
}

fn must_compile(pattern: string) -> Regexp {
    match compile(pattern) {
        Result.Ok(r) => { return r },
        Result.Err(_) => { panic("invalid regexp pattern") },
        _ => { panic("invalid regexp pattern") },
    }
    return Regexp { pattern = "" }
}

private fn char_match(pb: i32, tb: i32) -> bool {
    // '.' wildcard
    if pb == string_get(".", 0) {
        return true
    }
    return pb == tb
}

private fn substr_safe(s: string, start: i64, len: i64) -> string {
    let n: i64 = string_len(s)
    if start < 0 || len <= 0 || start >= n {
        return ""
    }
    let end: i64 = start + len
    let clamped_end: i64 = if end > n { n } else { end }
    let i: i64 = start
    let out: string = ""
    while i < clamped_end {
        out = string_concat(out, string_from_byte(string_get(s, i)))
        i = i + 1
    }
    return out
}

private fn match_here(pattern: string, pi: i64, text: string, ti: i64) -> bool {
    let np: i64 = string_len(pattern)
    let nt: i64 = string_len(text)
    if pi >= np {
        return ti == nt
    }

    let has_next: bool = pi + 1 < np
    if has_next {
        let q: i32 = char_at(pattern, pi + 1)
        if q == string_get("*", 0) {
            if match_here(pattern, pi + 2, text, ti) {
                return true
            }
            let k: i64 = ti
            while k < nt {
                let pb: i32 = char_at(pattern, pi)
                let tb: i32 = char_at(text, k)
                if pb < 0 || tb < 0 || !char_match(pb, tb) {
                    break
                }
                k = k + 1
                if match_here(pattern, pi + 2, text, k) {
                    return true
                }
            }
            return false
        }
        if q == string_get("?", 0) {
            if match_here(pattern, pi + 2, text, ti) {
                return true
            }
            if ti < nt {
                let pb: i32 = char_at(pattern, pi)
                let tb: i32 = char_at(text, ti)
                if pb >= 0 && tb >= 0 && char_match(pb, tb) {
                    return match_here(pattern, pi + 2, text, ti + 1)
                }
            }
            return false
        }
    }

    if ti < nt {
        let pb: i32 = char_at(pattern, pi)
        let tb: i32 = char_at(text, ti)
        if pb >= 0 && tb >= 0 && char_match(pb, tb) {
            return match_here(pattern, pi + 1, text, ti + 1)
        }
        return false
    }
    return false
}

fn is_match(r: Regexp, text: string) -> bool {
    return match_here(r.pattern, 0, text, 0)
}

fn match_string(pattern: string, text: string) -> bool {
    match compile(pattern) {
        Result.Ok(r) => { return is_match(r, text) },
        Result.Err(_) => { return false },
        _ => { return false },
    }
    return false
}

fn find_string(r: Regexp, text: string) -> string {
    let n: i64 = string_len(text)
    if n <= 0 {
        return ""
    }
    let start: i64 = 0
    while start < n {
        let end: i64 = start + 1
        while end <= n {
            let cand = substr_safe(text, start, end - start)
            if is_match(r, cand) {
                return cand
            }
            end = end + 1
        }
        start = start + 1
    }
    return ""
}

fn replace_all_string(r: Regexp, src: string, repl: string) -> string {
    if is_match(r, "") {
        return src
    }
    let out: string = ""
    let n: i64 = string_len(src)
    let i: i64 = 0
    while i < n {
        let matched: bool = false
        let end: i64 = i + 1
        let hit_end: i64 = i
        while end <= n {
            let cand = substr_safe(src, i, end - i)
            if is_match(r, cand) {
                matched = true
                hit_end = end
                break
            }
            end = end + 1
        }
        if matched {
            out = string_concat(out, repl)
            i = hit_end
        } else {
            out = string_concat(out, substr_safe(src, i, 1))
            i = i + 1
        }
    }
    return out
}

fn find(pattern: string, text: string) -> string {
    match compile(pattern) {
        Result.Ok(r) => { return find_string(r, text) },
        Result.Err(_) => { return "" },
        _ => { return "" },
    }
    return ""
}

fn replace_all(pattern: string, src: string, repl: string) -> string {
    match compile(pattern) {
        Result.Ok(r) => { return replace_all_string(r, src, repl) },
        Result.Err(_) => { return src },
        _ => { return src },
    }
    return src
}
