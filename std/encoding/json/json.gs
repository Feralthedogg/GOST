module json

// Stable JSON AST subset.
// Composite values are stored as canonical JSON text to avoid nested shared-copy hazards.
copy enum Json {
    Null;
    Bool(bool);
    Number(string);
    String(string); // raw inner text (escape sequences preserved)
    Array(string);  // canonical JSON array text
    Object(string); // canonical JSON object text
}

// Go-like stream wrappers (string-backed in Gost std for now).
struct Decoder {
    src: string;
    idx: i64;
    use_number: bool;
    disallow_unknown_fields: bool;
}

struct Encoder {
    prefix: string;
    indent: string;
    escape_html: bool;
}

fn zero_i64() -> i64 { return string_len(""); }
fn one_i64() -> i64 { return string_len("a"); }

fn error_new(msg: string) -> error {
    return __gost_error_new(msg);
}

fn ok_json(v: Json) -> Result[Json, error] {
    return Result.Ok[Json, error](v);
}

fn err_json(msg: string) -> Result[Json, error] {
    return Result.Err[Json, error](error_new(msg));
}

fn ok_str(v: string) -> Result[string, error] {
    return Result.Ok[string, error](v);
}

fn err_str(msg: string) -> Result[string, error] {
    return Result.Err[string, error](error_new(msg));
}

fn byte(c: string) -> i32 {
    return string_get(c, zero_i64());
}

fn is_ws(b: i32) -> bool {
    return b == byte(" ") || b == byte("\n") || b == byte("\r") || b == byte("\t");
}

fn is_digit(b: i32) -> bool {
    return b >= byte("0") && b <= byte("9");
}

fn skip_ws(s: string, idx: mutref[i64]) {
    let n: i64 = string_len(s);
    while *idx < n {
        let b: i32 = string_get(s, *idx);
        if !is_ws(b) { break; }
        *idx = *idx + one_i64();
    }
}

fn match_word(s: string, i: i64, w: string) -> bool {
    let n: i64 = string_len(s);
    let m: i64 = string_len(w);
    if i + m > n { return false; }
    let k: i64 = zero_i64();
    while k < m {
        if string_get(s, i + k) != string_get(w, k) { return false; }
        k = k + one_i64();
    }
    return true;
}

// Parses a quoted JSON string and returns raw inner bytes (escape sequences preserved).
fn parse_string_raw(s: string, idx: mutref[i64]) -> Result[string, error] {
    let n: i64 = string_len(s);
    let j: i64 = *idx;
    if j >= n { return err_str("unexpected end of input"); }
    if string_get(s, j) != byte("\"") {
        return err_str("expected string");
    }

    let start: i64 = j + one_i64();
    let k: i64 = start;
    while k < n {
        let b: i32 = string_get(s, k);
        if b == byte("\\") {
            k = k + one_i64() + one_i64();
            continue;
        }
        if b == byte("\"") {
            let out: string = string_slice(s, start, k - start);
            *idx = k + one_i64();
            return ok_str(out);
        }
        k = k + one_i64();
    }

    *idx = k;
    return err_str("unterminated string");
}

fn parse_number_raw(s: string, idx: mutref[i64]) -> Result[string, error] {
    let n: i64 = string_len(s);
    let start: i64 = *idx;
    let j: i64 = *idx;

    if string_get(s, j) == byte("-") { j = j + one_i64(); }
    if j >= n { return err_str("invalid number"); }
    if !is_digit(string_get(s, j)) { return err_str("invalid number"); }

    if string_get(s, j) == byte("0") {
        j = j + one_i64();
    } else {
        while j < n && is_digit(string_get(s, j)) {
            j = j + one_i64();
        }
    }

    if j < n && string_get(s, j) == byte(".") {
        j = j + one_i64();
        if j >= n || !is_digit(string_get(s, j)) { return err_str("invalid number"); }
        while j < n && is_digit(string_get(s, j)) {
            j = j + one_i64();
        }
    }

    if j < n {
        let c: i32 = string_get(s, j);
        if c == byte("e") || c == byte("E") {
            j = j + one_i64();
            if j < n {
                let sgn: i32 = string_get(s, j);
                if sgn == byte("+") || sgn == byte("-") {
                    j = j + one_i64();
                }
            }
            if j >= n || !is_digit(string_get(s, j)) { return err_str("invalid number"); }
            while j < n && is_digit(string_get(s, j)) {
                j = j + one_i64();
            }
        }
    }

    *idx = j;
    return ok_str(string_slice(s, start, j - start));
}

fn parse_array(s: string, idx: mutref[i64]) -> Result[Json, error] {
    let n: i64 = string_len(s);
    if string_get(s, *idx) != byte("[") {
        return err_json("expected '['");
    }

    *idx = *idx + one_i64();
    skip_ws(s, idx);

    let out: string = "[";

    if *idx < n && string_get(s, *idx) == byte("]") {
        *idx = *idx + one_i64();
        out = string_concat(out, "]");
        return ok_json(Json.Array(out));
    }

    let first: bool = true;
    loop {
        let vr = parse_value(s, idx);
        let has_err: bool = false;
        let err_val: error = error_new("invalid array element");
        let text: string = "";
        match vr {
            Result.Ok(v) => {
                text = json_stringify_value(v);
            },
            Result.Err(e) => {
                has_err = true;
                err_val = e;
            },
            _ => { has_err = true; },
        }
        if has_err { return Result.Err[Json, error](err_val); }

        if !first { out = string_concat(out, ","); }
        first = false;
        out = string_concat(out, text);

        skip_ws(s, idx);
        if *idx >= n { return err_json("unterminated array"); }

        let b: i32 = string_get(s, *idx);
        if b == byte(",") {
            *idx = *idx + one_i64();
            skip_ws(s, idx);
            continue;
        }
        if b == byte("]") {
            *idx = *idx + one_i64();
            out = string_concat(out, "]");
            break;
        }
        return err_json("expected ',' or ']'" );
    }

    return ok_json(Json.Array(out));
}

fn parse_object(s: string, idx: mutref[i64]) -> Result[Json, error] {
    let n: i64 = string_len(s);
    if string_get(s, *idx) != byte("{") {
        return err_json("expected '{'");
    }

    *idx = *idx + one_i64();
    skip_ws(s, idx);

    let out: string = "{";

    if *idx < n && string_get(s, *idx) == byte("}") {
        *idx = *idx + one_i64();
        out = string_concat(out, "}");
        return ok_json(Json.Object(out));
    }

    let first: bool = true;
    loop {
        let kr = parse_string_raw(s, idx);
        let key: string = "";
        let key_err: bool = false;
        let key_err_val: error = error_new("expected object key");
        match kr {
            Result.Ok(k) => { key = k; },
            Result.Err(e) => { key_err = true; key_err_val = e; },
            _ => { key_err = true; },
        }
        if key_err { return Result.Err[Json, error](key_err_val); }

        skip_ws(s, idx);
        if *idx >= n || string_get(s, *idx) != byte(":") {
            return err_json("expected ':' after object key");
        }
        *idx = *idx + one_i64();

        let vr = parse_value(s, idx);
        let has_err2: bool = false;
        let err2: error = error_new("invalid object value");
        let vtxt: string = "";
        match vr {
            Result.Ok(v) => { vtxt = json_stringify_value(v); },
            Result.Err(e) => { has_err2 = true; err2 = e; },
            _ => { has_err2 = true; },
        }
        if has_err2 { return Result.Err[Json, error](err2); }

        if !first { out = string_concat(out, ","); }
        first = false;

        // key is already raw inner string; keep as-is to preserve escapes.
        let kq: string = string_concat("\"", string_concat(key, "\""));
        out = string_concat(out, kq);
        out = string_concat(out, ":");
        out = string_concat(out, vtxt);

        skip_ws(s, idx);
        if *idx >= n { return err_json("unterminated object"); }

        let b: i32 = string_get(s, *idx);
        if b == byte(",") {
            *idx = *idx + one_i64();
            skip_ws(s, idx);
            continue;
        }
        if b == byte("}") {
            *idx = *idx + one_i64();
            out = string_concat(out, "}");
            break;
        }
        return err_json("expected ',' or '}'");
    }

    return ok_json(Json.Object(out));
}

fn parse_value(s: string, idx: mutref[i64]) -> Result[Json, error] {
    let n: i64 = string_len(s);
    skip_ws(s, idx);
    if *idx >= n { return err_json("unexpected end of input"); }

    let b: i32 = string_get(s, *idx);
    if b == byte("\"") {
        let sr = parse_string_raw(s, idx);
        return match sr {
            Result.Ok(v) => ok_json(Json.String(v)),
            Result.Err(e) => Result.Err[Json, error](e),
            _ => err_json("invalid string"),
        };
    }
    if b == byte("[") { return parse_array(s, idx); }
    if b == byte("{") { return parse_object(s, idx); }

    if b == byte("t") {
        if match_word(s, *idx, "true") {
            *idx = *idx + string_len("true");
            return ok_json(Json.Bool(true));
        }
        return err_json("invalid token");
    }
    if b == byte("f") {
        if match_word(s, *idx, "false") {
            *idx = *idx + string_len("false");
            return ok_json(Json.Bool(false));
        }
        return err_json("invalid token");
    }
    if b == byte("n") {
        if match_word(s, *idx, "null") {
            *idx = *idx + string_len("null");
            return ok_json(Json.Null);
        }
        return err_json("invalid token");
    }

    if b == byte("-") || is_digit(b) {
        let nr = parse_number_raw(s, idx);
        return match nr {
            Result.Ok(raw) => ok_json(Json.Number(raw)),
            Result.Err(e) => Result.Err[Json, error](e),
            _ => err_json("invalid number"),
        };
    }

    return err_json("unexpected token");
}

fn parse_internal(s: string) -> Result[Json, error] {
    let i: i64 = zero_i64();
    let r = parse_value(s, &mut i);
    let out: Result[Json, error] = err_json("invalid value");
    match r {
        Result.Ok(v) => {
            skip_ws(s, &mut i);
            if i != string_len(s) {
                out = err_json("trailing characters after JSON value");
            } else {
                out = ok_json(v);
            }
        },
        Result.Err(e) => { out = Result.Err[Json, error](e); },
        _ => { out = err_json("invalid value"); },
    };
    return out;
}

fn json_quote_raw(s: string) -> string {
    let t: string = string_concat("\"", s);
    return string_concat(t, "\"");
}

fn json_bool_text(b: bool) -> string {
    if b { return "true"; }
    return "false";
}

fn json_stringify_value(v: Json) -> string {
    return match v {
        Json.Null => "null",
        Json.Bool(b) => json_bool_text(b),
        Json.Number(s) => s,
        Json.String(s) => json_quote_raw(s),
        Json.Array(raw) => raw,
        Json.Object(raw) => raw,
        _ => "null",
    };
}

// Public API: parse JSON string
fn parse(s: string) -> Result[Json, error] {
    return parse_internal(s);
}

// Public API: stringify JSON AST
fn stringify(v: Json) -> string {
    return json_stringify_value(v);
}

// Go-like aliases
fn marshal(v: Json) -> Result[string, error] {
    return Result.Ok[string, error](stringify(v));
}

fn unmarshal(s: string) -> Result[Json, error] {
    return parse(s);
}

fn valid(s: string) -> bool {
    let r = parse(s);
    match r {
        Result.Ok(_) => { return true; },
        Result.Err(_) => { return false; },
        _ => { return false; },
    };
    return false;
}

fn compact(s: string) -> Result[string, error] {
    let r = parse(s);
    match r {
        Result.Ok(v) => { return Result.Ok[string, error](stringify(v)); },
        Result.Err(e) => { return Result.Err[string, error](e); },
        _ => { return err_str("invalid JSON"); },
    };
    return err_str("invalid JSON");
}

fn repeat_str(s: string, n: i64) -> string {
    let out: string = "";
    let i: i64 = zero_i64();
    while i < n {
        out = string_concat(out, s);
        i = i + one_i64();
    }
    return out;
}

fn next_non_ws_index(s: string, start: i64) -> i64 {
    let n: i64 = string_len(s);
    let i: i64 = start;
    while i < n {
        if !is_ws(string_get(s, i)) { return i; }
        i = i + one_i64();
    }
    return n;
}

fn pretty_json(raw: string, prefix: string, ind: string) -> string {
    let out: string = prefix;
    let n: i64 = string_len(raw);
    let i: i64 = zero_i64();
    let level: i64 = zero_i64();
    let in_str: bool = false;
    let esc: bool = false;
    let last_sig: i32 = 0;

    while i < n {
        let b: i32 = string_get(raw, i);
        if in_str {
            out = string_concat(out, string_slice(raw, i, one_i64()));
            if esc {
                esc = false;
            } else {
                if b == byte("\\") {
                    esc = true;
                } else {
                    if b == byte("\"") { in_str = false; }
                }
            }
            i = i + one_i64();
            continue;
        }

        if is_ws(b) {
            i = i + one_i64();
            continue;
        }

        if b == byte("\"") {
            in_str = true;
            out = string_concat(out, "\"");
            last_sig = b;
            i = i + one_i64();
            continue;
        }

        if b == byte("{") || b == byte("[") {
            out = string_concat(out, string_slice(raw, i, one_i64()));
            level = level + one_i64();
            let nx: i64 = next_non_ws_index(raw, i + one_i64());
            if nx < n {
                let nb: i32 = string_get(raw, nx);
                if nb != byte("}") && nb != byte("]") {
                    out = string_concat(out, "\n");
                    out = string_concat(out, prefix);
                    out = string_concat(out, repeat_str(ind, level));
                }
            }
            last_sig = b;
            i = i + one_i64();
            continue;
        }

        if b == byte("}") || b == byte("]") {
            if level > zero_i64() { level = level - one_i64(); }
            if last_sig != byte("{") && last_sig != byte("[") {
                out = string_concat(out, "\n");
                out = string_concat(out, prefix);
                out = string_concat(out, repeat_str(ind, level));
            }
            out = string_concat(out, string_slice(raw, i, one_i64()));
            last_sig = b;
            i = i + one_i64();
            continue;
        }

        if b == byte(",") {
            out = string_concat(out, ",");
            out = string_concat(out, "\n");
            out = string_concat(out, prefix);
            out = string_concat(out, repeat_str(ind, level));
            last_sig = b;
            i = i + one_i64();
            continue;
        }

        if b == byte(":") {
            out = string_concat(out, ": ");
            last_sig = b;
            i = i + one_i64();
            continue;
        }

        out = string_concat(out, string_slice(raw, i, one_i64()));
        last_sig = b;
        i = i + one_i64();
    }

    return out;
}

fn indent(s: string, prefix: string, ind: string) -> Result[string, error] {
    let r = compact(s);
    match r {
        Result.Ok(raw) => { return Result.Ok[string, error](pretty_json(raw, prefix, ind)); },
        Result.Err(e) => { return Result.Err[string, error](e); },
        _ => { return err_str("invalid JSON"); },
    };
    return err_str("invalid JSON");
}

fn marshal_indent(v: Json, prefix: string, ind: string) -> Result[string, error] {
    let raw: string = stringify(v);
    return Result.Ok[string, error](pretty_json(raw, prefix, ind));
}

fn html_escape(s: string) -> string {
    let out: string = "";
    let n: i64 = string_len(s);
    let i: i64 = zero_i64();
    while i < n {
        let b: i32 = string_get(s, i);
        if b == byte("<") {
            out = string_concat(out, "\\u003c");
        } else {
            if b == byte(">") {
                out = string_concat(out, "\\u003e");
            } else {
                if b == byte("&") {
                    out = string_concat(out, "\\u0026");
                } else {
                    out = string_concat(out, string_slice(s, i, one_i64()));
                }
            }
        }
        i = i + one_i64();
    }
    return out;
}

fn new_decoder(src: string) -> Decoder {
    return Decoder {
        src = src,
        idx = zero_i64(),
        use_number = false,
        disallow_unknown_fields = false,
    };
}

fn decoder_use_number(dec: Decoder) -> Decoder {
    return Decoder {
        src = dec.src,
        idx = dec.idx,
        use_number = true,
        disallow_unknown_fields = dec.disallow_unknown_fields,
    };
}

fn decoder_disallow_unknown_fields(dec: Decoder) -> Decoder {
    return Decoder {
        src = dec.src,
        idx = dec.idx,
        use_number = dec.use_number,
        disallow_unknown_fields = true,
    };
}

fn decode(dec: Decoder) -> Result[Json, error] {
    let idx: i64 = dec.idx;
    skip_ws(dec.src, &mut idx);
    let n: i64 = string_len(dec.src);
    if idx >= n { return err_json("unexpected end of input"); }
    let r = parse_value(dec.src, &mut idx);
    match r {
        Result.Ok(v) => {
            skip_ws(dec.src, &mut idx);
            return ok_json(v);
        },
        Result.Err(e) => { return Result.Err[Json, error](e); },
        _ => { return err_json("invalid value"); },
    };
    return err_json("invalid value");
}

fn new_encoder() -> Encoder {
    return Encoder {
        prefix = "",
        indent = "",
        escape_html = true,
    };
}

fn encoder_set_indent(enc: Encoder, prefix: string, ind: string) -> Encoder {
    return Encoder {
        prefix = prefix,
        indent = ind,
        escape_html = enc.escape_html,
    };
}

fn encoder_set_escape_html(enc: Encoder, on: bool) -> Encoder {
    return Encoder {
        prefix = enc.prefix,
        indent = enc.indent,
        escape_html = on,
    };
}

fn encode(enc: Encoder, v: Json) -> Result[string, error] {
    let formatted: Result[string, error] = err_str("encode failed");
    if string_len(enc.indent) > zero_i64() || string_len(enc.prefix) > zero_i64() {
        formatted = marshal_indent(v, enc.prefix, enc.indent);
    } else {
        formatted = marshal(v);
    }

    match formatted {
        Result.Ok(text) => {
            let out: string = text;
            if enc.escape_html {
                out = html_escape(out);
            }
            return Result.Ok[string, error](string_concat(out, "\n"));
        },
        Result.Err(e) => { return Result.Err[string, error](e); },
        _ => { return err_str("encode failed"); },
    };
    return err_str("encode failed");
}
