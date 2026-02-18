module toml

import "std/strings"

// Stable TOML subset AST.
// Composite values are stored as TOML text to avoid nested shared-copy hazards.
copy enum Toml {
    Null
    Bool(bool)
    Number(string)
    String(string)
    Array(string)
 // inline TOML array text
    Table(string)
 // full TOML document/table text
}

// Go-like stream wrappers (string-backed in Gost std for now).
struct Decoder {
    src: string
    done: bool
}

struct Encoder {
    newline: bool
}

fn zero_i64() -> i64 { return string_len("")
 }
fn one_i64() -> i64 { return string_len("a")
 }

fn error_new(msg: string) -> error {
    return __gost_error_new(msg)
}

fn ok_toml(v: Toml) -> Result[Toml, error] {
    return Result.Ok[Toml, error](v)
}

fn err_toml(msg: string) -> Result[Toml, error] {
    return Result.Err[Toml, error](error_new(msg))
}

fn ok_str(s: string) -> Result[string, error] {
    return Result.Ok[string, error](s)
}

fn err_str(msg: string) -> Result[string, error] {
    return Result.Err[string, error](error_new(msg))
}

fn byte(c: string) -> i32 {
    return string_get(c, zero_i64())
}

fn is_space(b: i32) -> bool {
    return b == byte(" ") || b == byte("\t") || b == byte("\r") || b == byte("\n")
}

fn is_digit(b: i32) -> bool {
    return b >= byte("0") && b <= byte("9")
}

fn toml_is_empty(s: string) -> bool {
    return string_len(s) == zero_i64()
}

fn trim_left(s: string) -> string {
    let n: i64 = string_len(s)
    let j: i64 = zero_i64()
    while j < n {
        if !is_space(string_get(s, j)) { break
 }
        j = j + one_i64()
    }
    if j == zero_i64() { return s
 }
    return string_slice(s, j, n - j)
}

fn trim_right(s: string) -> string {
    let n: i64 = string_len(s)
    if n == zero_i64() { return s
 }
    let j: i64 = n - one_i64()
    while j >= zero_i64() {
        if !is_space(string_get(s, j)) { break
 }
        if j == zero_i64() { return ""
 }
        j = j - one_i64()
    }
    return string_slice(s, zero_i64(), j + one_i64())
}

fn trim(s: string) -> string {
    return trim_right(trim_left(s))
}

fn split_lines(s: string) -> []string {
    let lines = make_slice[string](zero_i64(), zero_i64())
    let n: i64 = string_len(s)
    let i: i64 = zero_i64()
    let start: i64 = zero_i64()
    while i < n {
        if string_get(s, i) == byte("\n") {
            slice_push[string](&mut lines, string_slice(s, start, i - start))
            start = i + one_i64()
        }
        i = i + one_i64()
    }
    if start <= n {
        slice_push[string](&mut lines, string_slice(s, start, n - start))
    }
    return lines
}

fn strip_comment(line: string) -> string {
    let n: i64 = string_len(line)
    let i: i64 = zero_i64()
    let in_str: bool = false
    while i < n {
        let b: i32 = string_get(line, i)
        if b == byte("\"") {
            in_str = !in_str
        }
        if !in_str && b == byte("#") {
            return string_slice(line, zero_i64(), i)
        }
        i = i + one_i64()
    }
    return line
}

fn find_eq(line: string) -> i64 {
    let n: i64 = string_len(line)
    let i: i64 = zero_i64()
    let in_str: bool = false
    while i < n {
        let b: i32 = string_get(line, i)
        if b == byte("\"") {
            in_str = !in_str
        }
        if !in_str && b == byte("=") {
            return i
        }
        i = i + one_i64()
    }
    return zero_i64() - one_i64()
}

fn parse_string_raw(s: string, idx: mutref[i64]) -> Result[string, error] {
    let n: i64 = string_len(s)
    let j: i64 = *idx
    if j >= n { return err_str("unexpected end of input")
 }
    if string_get(s, j) != byte("\"") {
        return err_str("expected string")
    }

    let start: i64 = j + one_i64()
    let k: i64 = start
    while k < n {
        let b: i32 = string_get(s, k)
        if b == byte("\\") {
            k = k + one_i64() + one_i64()
            continue
        }
        if b == byte("\"") {
            *idx = k + one_i64()
            return ok_str(string_slice(s, start, k - start))
        }
        k = k + one_i64()
    }

    *idx = k
    return err_str("unterminated string")
}

fn toml_quote_raw(s: string) -> string {
    let t: string = string_concat("\"", s)
    return string_concat(t, "\"")
}

fn parse_scalar_token(t: string) -> Toml {
    let s: string = trim(t)
    if toml_is_empty(s) { return Toml.Null
 }
    if equal(s, "true") { return Toml.Bool(true)
 }
    if equal(s, "false") { return Toml.Bool(false)
 }
    if equal(s, "null") { return Toml.Null
 }

    let n: i64 = string_len(s)
    let ok_num: bool = true
    let i: i64 = zero_i64()
    if i < n && string_get(s, i) == byte("-") { i = i + one_i64()
 }
    if i >= n { ok_num = false
 }
    while i < n {
        let b: i32 = string_get(s, i)
        if !(is_digit(b) || b == byte(".")) { ok_num = false
 break
 }
        i = i + one_i64()
    }
    if ok_num { return Toml.Number(s)
 }

    return Toml.String(s)
}

fn skip_ws(s: string, idx: mutref[i64]) {
    let n: i64 = string_len(s)
    while *idx < n {
        if !is_space(string_get(s, *idx)) { break
 }
        *idx = *idx + one_i64()
    }
}

fn toml_bool_text(b: bool) -> string {
    if b { return "true"
 }
    return "false"
}

fn toml_value_string(v: Toml) -> string {
    let out: string = "null"
    match v {
        Toml.Null => { out = "null"
 },
        Toml.Bool(b) => { out = toml_bool_text(b)
 },
        Toml.Number(s) => { out = s
 },
        Toml.String(s) => { out = toml_quote_raw(s)
 },
        Toml.Array(raw) => { out = raw
 },
        Toml.Table(raw) => { out = raw
 },
        _ => { out = "null"
 },
    }
    return out
}

fn parse_array_inline(s: string, idx: mutref[i64]) -> Result[Toml, error] {
    let n: i64 = string_len(s)
    if string_get(s, *idx) != byte("[") {
        return err_toml("expected '['")
    }

    *idx = *idx + one_i64()
    let out: string = "["

    skip_ws(s, idx)
    if *idx < n && string_get(s, *idx) == byte("]") {
        *idx = *idx + one_i64()
        out = string_concat(out, "]")
        return ok_toml(Toml.Array(out))
    }

    let first: bool = true
    loop {
        let r = parse_value_inline(s, idx)
        let has_err: bool = false
        let err_val: error = error_new("invalid array value")
        let text: string = ""
        match r {
            Result.Ok(v) => { text = toml_value_string(v)
 },
            Result.Err(e) => { has_err = true
 err_val = e
 },
            _ => { has_err = true
 },
        }
        if has_err { return Result.Err[Toml, error](err_val)
 }

        if !first { out = string_concat(out, ", ")
 }
        first = false
        out = string_concat(out, text)

        skip_ws(s, idx)
        if *idx >= n { return err_toml("unterminated array")
 }

        if string_get(s, *idx) == byte(",") {
            *idx = *idx + one_i64()
            skip_ws(s, idx)
            continue
        }
        if string_get(s, *idx) == byte("]") {
            *idx = *idx + one_i64()
            out = string_concat(out, "]")
            break
        }
        return err_toml("expected ',' or ']' in array")
    }

    return ok_toml(Toml.Array(out))
}

fn parse_value_inline(s: string, idx: mutref[i64]) -> Result[Toml, error] {
    let n: i64 = string_len(s)
    skip_ws(s, idx)
    if *idx >= n { return err_toml("unexpected end of input")
 }

    let b: i32 = string_get(s, *idx)
    if b == byte("\"") {
        let r = parse_string_raw(s, idx)
        let out: Result[Toml, error] = err_toml("invalid string")
        match r {
            Result.Ok(v) => { out = ok_toml(Toml.String(v)) },
            Result.Err(e) => { out = Result.Err[Toml, error](e) },
            _ => { out = err_toml("invalid string") },
        }
        return out
    }
    if b == byte("[") {
        return parse_array_inline(s, idx)
    }

    let start: i64 = *idx
    while *idx < n {
        let c: i32 = string_get(s, *idx)
        if c == byte(",") || c == byte("]") {
            break
        }
        *idx = *idx + one_i64()
    }

    let token: string = string_slice(s, start, *idx - start)
    return ok_toml(parse_scalar_token(token))
}

fn parse_value_text(t: string) -> Result[Toml, error] {
    let i: i64 = zero_i64()
    let r = parse_value_inline(t, &mut i)
    match r {
        Result.Ok(v) => {
            skip_ws(t, &mut i)
            if i != string_len(t) {
                return err_toml("trailing characters after value")
            }
            return ok_toml(v)
        },
        Result.Err(e) => { return Result.Err[Toml, error](e) },
        _ => { return err_toml("invalid value") },
    }
    return err_toml("invalid value")
}

fn trim_trailing_nl(s: string) -> string {
    let n: i64 = string_len(s)
    if n == zero_i64() { return s }
    if string_get(s, n - one_i64()) != byte("\n") { return s }
    return string_slice(s, zero_i64(), n - one_i64())
}

fn parse(s: string) -> Result[Toml, error] {
    let lines: []string = split_lines(s)
    let n: i64 = slice_len[string](&lines)
    let out: string = ""
    let prefix: string = ""
    let i: i64 = zero_i64()

    while i < n {
        let raw: string = slice_get_copy[string](&lines, i)
        let line: string = trim(strip_comment(raw))
        if toml_is_empty(line) { i = i + one_i64() continue }

        if string_get(line, zero_i64()) == byte("[") {
            let ln: i64 = string_len(line)
            if ln < one_i64() + one_i64() || string_get(line, ln - one_i64()) != byte("]") {
                return err_toml("invalid table header")
            }
            prefix = trim(string_slice(line, one_i64(), ln - one_i64() - one_i64()))
            out = string_concat(out, "[")
            out = string_concat(out, prefix)
            out = string_concat(out, "]\n")
            i = i + one_i64()
            continue
        }

        let pos: i64 = find_eq(line)
        if pos < zero_i64() { return err_toml("expected '='") }

        let key: string = trim(string_slice(line, zero_i64(), pos))
        let val_text: string = trim(string_slice(line, pos + one_i64(), string_len(line) - (pos + one_i64())))
        if toml_is_empty(key) { return err_toml("empty key") }

        let full_key: string = key
        if !toml_is_empty(prefix) {
            full_key = string_concat(string_concat(prefix, "."), key)
        }

        let vr = parse_value_text(val_text)
        let text_val: string = ""
        let has_err: bool = false
        let err_v: error = error_new("invalid value")
        match vr {
            Result.Ok(v) => { text_val = toml_value_string(v) },
            Result.Err(e) => { has_err = true err_v = e },
            _ => { has_err = true },
        }
        if has_err { return Result.Err[Toml, error](err_v) }

        out = string_concat(out, full_key)
        out = string_concat(out, " = ")
        out = string_concat(out, text_val)
        out = string_concat(out, "\n")

        i = i + one_i64()
    }

    return ok_toml(Toml.Table(trim_trailing_nl(out)))
}

fn stringify(v: Toml) -> string {
    return toml_value_string(v)
}

fn marshal(v: Toml) -> Result[string, error] {
    return Result.Ok[string, error](stringify(v))
}

fn unmarshal(s: string) -> Result[Toml, error] {
    return parse(s)
}

fn valid(s: string) -> bool {
    let r = parse(s)
    match r {
        Result.Ok(_) => { return true },
        Result.Err(_) => { return false },
        _ => { return false },
    }
    return false
}

fn format(s: string) -> Result[string, error] {
    let r = parse(s)
    match r {
        Result.Ok(v) => { return Result.Ok[string, error](stringify(v)) },
        Result.Err(e) => { return Result.Err[string, error](e) },
        _ => { return err_str("invalid TOML") },
    }
    return err_str("invalid TOML")
}

fn new_decoder(src: string) -> Decoder {
    return Decoder { src = src, done = false }
}

fn decode(dec: Decoder) -> Result[Toml, error] {
    return parse(dec.src)
}

fn new_encoder() -> Encoder {
    return Encoder { newline = true }
}

fn encoder_set_newline(enc: Encoder, on: bool) -> Encoder {
    return Encoder { newline = on }
}

fn encode(enc: Encoder, v: Toml) -> Result[string, error] {
    let out: string = stringify(v)
    if enc.newline {
        return Result.Ok[string, error](string_concat(out, "\n"))
    }
    return Result.Ok[string, error](out)
}
