module yaml

// Stable YAML subset AST.
// Composite values are stored as emitted YAML text to avoid nested shared-copy hazards.
copy enum Yaml {
    Null
    Bool(bool)
    Number(string)
    String(string)
    Seq(string)
 // emitted YAML block
    Map(string)
 // emitted YAML block
}

// Go-like stream wrappers (string-backed in Gost std for now).
struct Decoder {
    src: string
    done: bool
}

struct Encoder {
    prefix: string
    indent: string
}

fn zero_i64() -> i64 { return string_len("")
 }
fn one_i64() -> i64 { return string_len("a")
 }

fn error_new(msg: string) -> error {
    return __gost_error_new(msg)
}

fn ok_yaml(v: Yaml) -> Result[Yaml, error] {
    return Result.Ok[Yaml, error](v)
}

fn err_yaml(msg: string) -> Result[Yaml, error] {
    return Result.Err[Yaml, error](error_new(msg))
}

fn byte(c: string) -> i32 {
    return string_get(c, zero_i64())
}

fn is_space(b: i32) -> bool {
    return b == byte(" ")
}

fn is_digit(b: i32) -> bool {
    return b >= byte("0") && b <= byte("9")
}

fn is_empty(s: string) -> bool {
    return string_len(s) == zero_i64()
}

fn str_eq(a: string, b: string) -> bool {
    let na: i64 = string_len(a)
    let nb: i64 = string_len(b)
    if na != nb { return false
 }
    let i: i64 = zero_i64()
    while i < na {
        if string_get(a, i) != string_get(b, i) { return false
 }
        i = i + one_i64()
    }
    return true
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

fn indent_of(line: string) -> i64 {
    let n: i64 = string_len(line)
    let i: i64 = zero_i64()
    while i < n {
        if !is_space(string_get(line, i)) { break
 }
        i = i + one_i64()
    }
    return i
}

fn content_after_indent(line: string, ind: i64) -> string {
    let n: i64 = string_len(line)
    if ind >= n { return ""
 }
    return string_slice(line, ind, n - ind)
}

fn is_blank_or_comment(content: string) -> bool {
    let t: string = trim(content)
    if is_empty(t) { return true
 }
    return string_get(t, zero_i64()) == byte("#")
}

fn parse_scalar(text: string) -> Yaml {
    let t: string = trim(text)
    if is_empty(t) { return Yaml.Null
 }
    if str_eq(t, "null") || str_eq(t, "~") { return Yaml.Null
 }
    if str_eq(t, "true") { return Yaml.Bool(true)
 }
    if str_eq(t, "false") { return Yaml.Bool(false)
 }

    let n: i64 = string_len(t)
    if n >= one_i64() + one_i64() {
        let first: i32 = string_get(t, zero_i64())
        let last: i32 = string_get(t, n - one_i64())
        if (first == byte("\"") && last == byte("\"")) || (first == byte("'") && last == byte("'")) {
            return Yaml.String(string_slice(t, one_i64(), n - one_i64() - one_i64()))
        }
    }

    let ok_num: bool = true
    let i: i64 = zero_i64()
    if i < n && string_get(t, i) == byte("-") {
        i = i + one_i64()
    }
    if i >= n { ok_num = false
 }
    while i < n {
        if !is_digit(string_get(t, i)) { ok_num = false
 break
 }
        i = i + one_i64()
    }
    if ok_num { return Yaml.Number(t)
 }

    return Yaml.String(t)
}

fn find_colon(line: string) -> i64 {
    let n: i64 = string_len(line)
    let i: i64 = zero_i64()
    while i < n {
        if string_get(line, i) == byte(":") { return i
 }
        i = i + one_i64()
    }
    return zero_i64() - one_i64()
}

fn trim_trailing_nl(s: string) -> string {
    let n: i64 = string_len(s)
    if n == zero_i64() { return s
 }
    if string_get(s, n - one_i64()) != byte("\n") { return s
 }
    return string_slice(s, zero_i64(), n - one_i64())
}

fn indent_str(n: i64) -> string {
    let out: string = ""
    let i: i64 = zero_i64()
    while i < n {
        out = string_concat(out, " ")
        i = i + one_i64()
    }
    return out
}

fn indent_block(text: string, n: i64) -> string {
    let pref: string = indent_str(n)
    let out: string = ""
    let lines: []string = split_lines(text)
    let ln: i64 = slice_len[string](&lines)
    let i: i64 = zero_i64()
    while i < ln {
        let line: string = slice_get_copy[string](&lines, i)
        out = string_concat(out, pref)
        out = string_concat(out, line)
        if i + one_i64() < ln { out = string_concat(out, "\n")
 }
        i = i + one_i64()
    }
    return out
}

fn yaml_bool_text(b: bool) -> string {
    if b { return "true"
 }
    return "false"
}

fn yaml_quote(s: string) -> string {
    let t: string = string_concat("\"", s)
    return string_concat(t, "\"")
}

fn yaml_scalar(v: Yaml) -> string {
    return match v {
        Yaml.Null => "null",
        Yaml.Bool(b) => yaml_bool_text(b),
        Yaml.Number(s) => s,
        Yaml.String(s) => yaml_quote(s),
        _ => "null",
    }
}

fn is_scalar(v: Yaml) -> bool {
    return match v {
        Yaml.Null => true,
        Yaml.Bool(_) => true,
        Yaml.Number(_) => true,
        Yaml.String(_) => true,
        _ => false,
    }
}

fn yaml_emit(v: Yaml) -> string {
    return match v {
        Yaml.Seq(s) => s,
        Yaml.Map(s) => s,
        _ => yaml_scalar(v),
    }
}

fn parse_block(lines: ref[[]string], idx: mutref[i64], indent: i64) -> Result[Yaml, error] {
    let n: i64 = slice_len[string](lines)
    while *idx < n {
        let line: string = slice_get_copy[string](lines, *idx)
        let ind: i64 = indent_of(line)
        if ind < indent { break
 }
        let content: string = content_after_indent(line, ind)
        if is_blank_or_comment(content) {
            *idx = *idx + one_i64()
            continue
        }
        let cont: string = trim(content)

        if string_len(cont) >= one_i64() + one_i64() {
            if string_get(cont, zero_i64()) == byte("-") && string_get(cont, one_i64()) == byte(" ") {
                return parse_seq(lines, idx, indent)
            }
        }

        if find_colon(cont) >= zero_i64() {
            return parse_map(lines, idx, indent)
        }

        let v: Yaml = parse_scalar(cont)
        *idx = *idx + one_i64()
        return ok_yaml(v)
    }

    return ok_yaml(Yaml.Null)
}

fn parse_seq(lines: ref[[]string], idx: mutref[i64], indent: i64) -> Result[Yaml, error] {
    let n: i64 = slice_len[string](lines)
    let out: string = ""

    while *idx < n {
        let line: string = slice_get_copy[string](lines, *idx)
        let ind: i64 = indent_of(line)
        if ind < indent { break
 }

        let cont: string = trim(content_after_indent(line, ind))
        if is_empty(cont) || is_blank_or_comment(cont) {
            *idx = *idx + one_i64()
            continue
        }
        if string_len(cont) < one_i64() + one_i64() { break
 }
        if string_get(cont, zero_i64()) != byte("-") { break
 }

        let rest: string = trim(string_slice(cont, one_i64() + one_i64(), string_len(cont) - (one_i64() + one_i64())))
        if !is_empty(out) { out = string_concat(out, "\n")
 }

        if is_empty(rest) {
            *idx = *idx + one_i64()
            let r = parse_block(lines, idx, indent + one_i64() + one_i64())
            match r {
                Result.Ok(v) => {
                    if is_scalar(v) {
                        out = string_concat(out, "- ")
                        out = string_concat(out, yaml_scalar(v))
                    } else {
                        out = string_concat(out, "-")
                        out = string_concat(out, "\n")
                        out = string_concat(out, indent_block(trim_trailing_nl(yaml_emit(v)), one_i64() + one_i64()))
                    }
                },
                Result.Err(e) => { return Result.Err[Yaml, error](e)
 },
                _ => { return err_yaml("invalid sequence value")
 },
            }
        } else {
            let v2: Yaml = parse_scalar(rest)
            out = string_concat(out, "- ")
            out = string_concat(out, yaml_scalar(v2))
            *idx = *idx + one_i64()
        }
    }

    return ok_yaml(Yaml.Seq(out))
}

fn parse_map(lines: ref[[]string], idx: mutref[i64], indent: i64) -> Result[Yaml, error] {
    let n: i64 = slice_len[string](lines)
    let out: string = ""

    while *idx < n {
        let line: string = slice_get_copy[string](lines, *idx)
        let ind: i64 = indent_of(line)
        if ind < indent { break
 }

        let cont: string = trim(content_after_indent(line, ind))
        if is_empty(cont) || is_blank_or_comment(cont) {
            *idx = *idx + one_i64()
            continue
        }

        let pos: i64 = find_colon(cont)
        if pos < zero_i64() { return err_yaml("invalid mapping entry")
 }

        let key: string = trim(string_slice(cont, zero_i64(), pos))
        if is_empty(key) { return err_yaml("invalid mapping entry")
 }
        let val_text: string = trim(string_slice(cont, pos + one_i64(), string_len(cont) - (pos + one_i64())))

        if !is_empty(out) { out = string_concat(out, "\n")
 }

        if is_empty(val_text) {
            *idx = *idx + one_i64()
            let r = parse_block(lines, idx, indent + one_i64() + one_i64())
            match r {
                Result.Ok(v) => {
                    if is_scalar(v) {
                        out = string_concat(out, key)
                        out = string_concat(out, ": ")
                        out = string_concat(out, yaml_scalar(v))
                    } else {
                        out = string_concat(out, key)
                        out = string_concat(out, ":\n")
                        out = string_concat(out, indent_block(trim_trailing_nl(yaml_emit(v)), one_i64() + one_i64()))
                    }
                },
                Result.Err(e) => { return Result.Err[Yaml, error](e)
 },
                _ => { return err_yaml("invalid mapping value")
 },
            }
        } else {
            let v2: Yaml = parse_scalar(val_text)
            out = string_concat(out, key)
            out = string_concat(out, ": ")
            out = string_concat(out, yaml_scalar(v2))
            *idx = *idx + one_i64()
        }
    }

    return ok_yaml(Yaml.Map(out))
}

// Public API: parse YAML subset
fn parse(s: string) -> Result[Yaml, error] {
    let lines: []string = split_lines(s)
    let i: i64 = zero_i64()
    return parse_block(&lines, &mut i, zero_i64())
}

// Public API: stringify YAML subset
fn stringify(v: Yaml) -> string {
    return trim_trailing_nl(yaml_emit(v))
}

fn marshal(v: Yaml) -> Result[string, error] {
    return Result.Ok[string, error](stringify(v))
}

fn unmarshal(s: string) -> Result[Yaml, error] {
    return parse(s)
}

fn valid(s: string) -> bool {
    let r = parse(s)
    match r {
        Result.Ok(_) => { return true
 },
        Result.Err(_) => { return false
 },
        _ => { return false
 },
    }
    return false
}

fn format(s: string) -> Result[string, error] {
    let r = parse(s)
    match r {
        Result.Ok(v) => { return Result.Ok[string, error](stringify(v))
 },
        Result.Err(e) => { return Result.Err[string, error](e)
 },
        _ => { return Result.Err[string, error](error_new("invalid YAML"))
 },
    }
    return Result.Err[string, error](error_new("invalid YAML"))
}

fn apply_prefix_indent(text: string, prefix: string, ind: string) -> string {
    if string_len(prefix) == zero_i64() && string_len(ind) == zero_i64() {
        return text
    }
    let lines: []string = split_lines(text)
    let ln: i64 = slice_len[string](&lines)
    let out: string = ""
    let i: i64 = zero_i64()
    while i < ln {
        if i > zero_i64() { out = string_concat(out, "\n")
 }
        out = string_concat(out, prefix)
        out = string_concat(out, ind)
        out = string_concat(out, slice_get_copy[string](&lines, i))
        i = i + one_i64()
    }
    return out
}

fn new_decoder(src: string) -> Decoder {
    return Decoder { src = src, done = false }
}

fn decode(dec: Decoder) -> Result[Yaml, error] {
    return parse(dec.src)
}

fn new_encoder() -> Encoder {
    return Encoder { prefix = "", indent = "" }
}

fn encoder_set_indent(enc: Encoder, prefix: string, ind: string) -> Encoder {
    return Encoder {
        prefix = prefix,
        indent = ind,
    }
}

fn encode(enc: Encoder, v: Yaml) -> Result[string, error] {
    let out: string = stringify(v)
    let wrapped: string = apply_prefix_indent(out, enc.prefix, enc.indent)
    return Result.Ok[string, error](string_concat(wrapped, "\n"))
}
