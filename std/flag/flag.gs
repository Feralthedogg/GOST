module flag

import "std/strings"

copy struct FlagSet {
    args_blob: string
}

private fn byte(c: string) -> i32 {
    return string_get(c, 0)
}

private fn starts_with(s: string, prefix: string) -> bool {
    let ns: i64 = string_len(s)
    let np: i64 = string_len(prefix)
    if np > ns {
        return false
    }
    let i: i64 = 0
    while i < np {
        if string_get(s, i) != string_get(prefix, i) {
            return false
        }
        i = i + 1
    }
    return true
}

private fn index_of(s: string, sub: string) -> i64 {
    let ns: i64 = string_len(s)
    let nm: i64 = string_len(sub)
    if nm == 0 {
        return 0
    }
    if nm > ns {
        return -1 as i64
    }
    let i: i64 = 0
    while i <= ns - nm {
        let ok = true
        let j: i64 = 0
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

private fn parse_bool(s: string, def: bool) -> bool {
    if equal(s, "1") || equal(s, "true") || equal(s, "TRUE") || equal(s, "yes") || equal(s, "on") {
        return true
    }
    if equal(s, "0") || equal(s, "false") || equal(s, "FALSE") || equal(s, "no") || equal(s, "off") {
        return false
    }
    return def
}

private fn parse_i64(s: string, def: i64) -> i64 {
    if string_len(s) == 0 {
        return def
    }
    let i: i64 = 0
    let n: i64 = string_len(s)
    let neg = false
    if string_get(s, 0) == byte("-") {
        neg = true
        i = 1
    } else if string_get(s, 0) == byte("+") {
        i = 1
    }
    if i >= n {
        return def
    }
    let out: i64 = 0
    while i < n {
        let c: i32 = string_get(s, i)
        if c < byte("0") || c > byte("9") {
            return def
        }
        out = out * 10 + ((c - byte("0")) as i64)
        i = i + 1
    }
    if neg {
        return -out
    }
    return out
}

private fn each_token(blob: string, out: mutref[[]string]) {
    if string_len(blob) == 0 {
        return
    }
    let n: i64 = string_len(blob)
    let i: i64 = 0
    let start: i64 = 0
    while i < n {
        if string_get(blob, i) == byte("\n") {
            slice_push[string](out, string_slice(blob, start, i - start))
            start = i + 1
        }
        i = i + 1
    }
    if start <= n {
        slice_push[string](out, string_slice(blob, start, n - start))
    }
}

private fn tokens_from(fs: ref[FlagSet]) -> []string {
    let out = make_slice[string](0, 0)
    let blob = ""
    blob = fs.args_blob
    each_token(blob, &mut out)
    return out
}

fn new() -> FlagSet {
    return FlagSet { args_blob = "" }
}

fn parse_args(args: []string) -> FlagSet {
    let blob = ""
    let i: i64 = 0
    let n: i64 = slice_len[string](&args)
    while i < n {
        if i > 0 {
            blob = string_concat(blob, "\n")
        }
        blob = string_concat(blob, slice_get_copy[string](&args, i))
        i = i + 1
    }
    return FlagSet { args_blob = blob }
}

fn parse() -> FlagSet {
    let raw = __gost_os_args()
    if string_len(raw) == 0 {
        return new()
    }
    let blob = ""
    let i: i64 = 0
    let start: i64 = 0
    let tok_idx: i64 = 0
    let n: i64 = string_len(raw)
    while i < n {
        let ch: i32 = string_get(raw, i)
        if ch == 0 || ch == byte("\n") {
            if i > start {
                let tok = string_slice(raw, start, i - start)
                if tok_idx > 0 {
                    if string_len(blob) > 0 {
                        blob = string_concat(blob, "\n")
                    }
                    blob = string_concat(blob, tok)
                }
                tok_idx = tok_idx + 1
            }
            start = i + 1
        }
        i = i + 1
    }
    if start < n {
        let tok = string_slice(raw, start, n - start)
        if tok_idx > 0 {
            if string_len(blob) > 0 {
                blob = string_concat(blob, "\n")
            }
            blob = string_concat(blob, tok)
        }
    }
    return FlagSet { args_blob = blob }
}

fn has(fs: ref[FlagSet], name: string) -> bool {
    let tokens = tokens_from(fs)
    let i: i64 = 0
    let n: i64 = slice_len[string](&tokens)
    let after_dd = false
    while i < n {
        let tok = slice_get_copy[string](&tokens, i)
        if after_dd {
            i = i + 1
            continue
        }
        if equal(tok, "--") {
            after_dd = true
            i = i + 1
            continue
        }
        if starts_with(tok, "--") {
            let body = string_slice(tok, 2, string_len(tok) - 2)
            let eq = index_of(body, "=")
            if eq >= 0 {
                if equal(string_slice(body, 0, eq), name) {
                    return true
                }
            } else if equal(body, name) {
                return true
            }
        } else if starts_with(tok, "-") && string_len(tok) > 1 {
            let cluster = string_slice(tok, 1, string_len(tok) - 1)
            let j: i64 = 0
            while j < string_len(cluster) {
                if equal(string_slice(cluster, j, 1), name) {
                    return true
                }
                j = j + 1
            }
        }
        i = i + 1
    }
    return false
}

fn string_value(fs: ref[FlagSet], name: string, def: string) -> string {
    let tokens = tokens_from(fs)
    let i: i64 = 0
    let n: i64 = slice_len[string](&tokens)
    let after_dd = false
    while i < n {
        let tok = slice_get_copy[string](&tokens, i)
        if after_dd {
            i = i + 1
            continue
        }
        if equal(tok, "--") {
            after_dd = true
            i = i + 1
            continue
        }
        if starts_with(tok, "--") {
            let body = string_slice(tok, 2, string_len(tok) - 2)
            let eq = index_of(body, "=")
            if eq >= 0 {
                if equal(string_slice(body, 0, eq), name) {
                    return string_slice(body, eq + 1, string_len(body) - (eq + 1))
                }
            } else if equal(body, name) {
                return "true"
            }
        } else if starts_with(tok, "-") && string_len(tok) > 1 && string_len(name) == 1 {
            let single = string_slice(name, 0, 1)
            if string_len(tok) == 2 && equal(string_slice(tok, 1, 1), single) {
                return "true"
            }
            let cluster = string_slice(tok, 1, string_len(tok) - 1)
            let j: i64 = 0
            while j < string_len(cluster) {
                if equal(string_slice(cluster, j, 1), single) {
                    return "true"
                }
                j = j + 1
            }
        }
        i = i + 1
    }
    return def
}

fn bool_value(fs: ref[FlagSet], name: string, def: bool) -> bool {
    if !has(fs, name) {
        return def
    }
    return parse_bool(string_value(fs, name, "true"), def)
}

fn int_value(fs: ref[FlagSet], name: string, def: i64) -> i64 {
    if !has(fs, name) {
        return def
    }
    return parse_i64(string_value(fs, name, ""), def)
}

fn positional(fs: ref[FlagSet]) -> []string {
    let out = make_slice[string](0, 0)
    let tokens = tokens_from(fs)
    let i: i64 = 0
    let n: i64 = slice_len[string](&tokens)
    let after_dd = false
    while i < n {
        let tok = slice_get_copy[string](&tokens, i)
        if after_dd {
            slice_push[string](&mut out, tok)
            i = i + 1
            continue
        }
        if equal(tok, "--") {
            after_dd = true
            i = i + 1
            continue
        }
        if starts_with(tok, "--") {
            i = i + 1
            continue
        }
        if starts_with(tok, "-") && string_len(tok) > 1 {
            i = i + 1
            continue
        }
        slice_push[string](&mut out, tok)
        i = i + 1
    }
    return out
}

fn parse_errors(fs: ref[FlagSet]) -> []string {
    let errs = make_slice[string](0, 0)
    let tokens = tokens_from(fs)
    let i: i64 = 0
    let n: i64 = slice_len[string](&tokens)
    let after_dd = false
    while i < n {
        let tok = slice_get_copy[string](&tokens, i)
        if after_dd {
            i = i + 1
            continue
        }
        if equal(tok, "--") {
            after_dd = true
            i = i + 1
            continue
        }
        if starts_with(tok, "--") {
            let body = string_slice(tok, 2, string_len(tok) - 2)
            if string_len(body) == 0 {
                slice_push[string](&mut errs, "empty long flag")
            } else {
                let eq = index_of(body, "=")
                if eq == 0 {
                    slice_push[string](&mut errs, "empty long flag name")
                }
            }
        }
        i = i + 1
    }
    return errs
}
