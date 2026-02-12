module base64

fn zero_i64() -> i64 { return string_len(""); }
fn one_i64() -> i64 { return string_len("a"); }
fn two_i64() -> i64 { return one_i64() + one_i64(); }
fn three_i64() -> i64 { return two_i64() + one_i64(); }
fn four_i64() -> i64 { return two_i64() + two_i64(); }

fn error_new(msg: string) -> error {
    return __gost_error_new(msg);
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

fn std_alpha() -> string {
    return "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
}

fn url_alpha() -> string {
    return "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
}

fn is_ws(b: i32) -> bool {
    return b == byte(" ") || b == byte("\n") || b == byte("\r") || b == byte("\t");
}

fn strip_ws(s: string) -> string {
    let out: string = "";
    let n: i64 = string_len(s);
    let i: i64 = zero_i64();
    while i < n {
        let b: i32 = string_get(s, i);
        if !is_ws(b) {
            out = string_concat(out, string_slice(s, i, one_i64()));
        }
        i = i + one_i64();
    }
    return out;
}

fn alpha_char(alpha: string, idx: i32) -> string {
    return string_slice(alpha, idx, one_i64());
}

fn alpha_index(alpha: string, b: i32) -> i32 {
    let i: i32 = 0;
    while i < 64 {
        if string_get(alpha, i) == b { return i; }
        i = i + 1;
    }
    return -1;
}

fn byte_str(b: i32) -> string {
    return string_from_byte(b);
}

fn encode_inner(src: string, alpha: string, padded: bool) -> string {
    let n: i64 = string_len(src);
    let i: i64 = zero_i64();
    let out: string = "";

    while i < n {
        let b0: i32 = string_get(src, i);
        i = i + one_i64();

        let b1: i32 = 0;
        let b2: i32 = 0;
        let has1: bool = false;
        let has2: bool = false;

        if i < n {
            b1 = string_get(src, i);
            i = i + one_i64();
            has1 = true;
        }
        if i < n {
            b2 = string_get(src, i);
            i = i + one_i64();
            has2 = true;
        }

        let i0: i32 = b0 / 4;
        let i1: i32 = (b0 % 4) * 16;
        if has1 { i1 = i1 + (b1 / 16); }

        let i2: i32 = (b1 % 16) * 4;
        if has2 { i2 = i2 + (b2 / 64); }

        let i3: i32 = b2 % 64;

        out = string_concat(out, alpha_char(alpha, i0));
        out = string_concat(out, alpha_char(alpha, i1));

        if has1 {
            out = string_concat(out, alpha_char(alpha, i2));
        } else {
            if padded { out = string_concat(out, "="); }
        }

        if has2 {
            out = string_concat(out, alpha_char(alpha, i3));
        } else {
            if padded { out = string_concat(out, "="); }
        }
    }

    return out;
}

fn decode_inner(src: string, alpha: string, padded: bool) -> Result[string, error] {
    let work: string = strip_ws(src);
    let n0: i64 = string_len(work);
    if n0 == zero_i64() {
        return ok_str("");
    }

    if padded {
        if n0 % four_i64() != zero_i64() {
            return err_str("invalid base64 length");
        }
    } else {
        let rem: i64 = n0 % four_i64();
        if rem == one_i64() {
            return err_str("invalid raw base64 length");
        }
        if rem == two_i64() {
            work = string_concat(work, "==");
        } else {
            if rem == three_i64() {
                work = string_concat(work, "=");
            }
        }
    }

    let n: i64 = string_len(work);
    if n % four_i64() != zero_i64() {
        return err_str("invalid base64 length");
    }

    let i: i64 = zero_i64();
    let out: string = "";
    while i < n {
        let c0: i32 = string_get(work, i);
        let c1: i32 = string_get(work, i + one_i64());
        let c2: i32 = string_get(work, i + two_i64());
        let c3: i32 = string_get(work, i + three_i64());

        if c0 == byte("=") || c1 == byte("=") {
            return err_str("invalid base64 padding");
        }

        let v0: i32 = alpha_index(alpha, c0);
        let v1: i32 = alpha_index(alpha, c1);
        if v0 < 0 || v1 < 0 {
            return err_str("invalid base64 character");
        }

        let pad2: bool = c2 == byte("=");
        let pad3: bool = c3 == byte("=");

        if pad2 && !pad3 {
            return err_str("invalid base64 padding");
        }
        if (pad2 || pad3) && (i + four_i64() < n) {
            return err_str("invalid base64 padding");
        }

        let v2: i32 = 0;
        let v3: i32 = 0;
        if !pad2 {
            v2 = alpha_index(alpha, c2);
            if v2 < 0 { return err_str("invalid base64 character"); }
        }
        if !pad3 {
            v3 = alpha_index(alpha, c3);
            if v3 < 0 { return err_str("invalid base64 character"); }
        }

        let chunk: i32 = v0 * 262144 + v1 * 4096 + v2 * 64 + v3;
        let b0: i32 = chunk / 65536;
        out = string_concat(out, byte_str(b0));

        if !pad2 {
            let b1: i32 = (chunk / 256) % 256;
            out = string_concat(out, byte_str(b1));
        }
        if !pad2 && !pad3 {
            let b2: i32 = chunk % 256;
            out = string_concat(out, byte_str(b2));
        }

        i = i + four_i64();
    }

    return ok_str(out);
}

fn encode_to_string(src: string) -> string {
    return encode_inner(src, std_alpha(), true);
}

fn decode_string(src: string) -> Result[string, error] {
    return decode_inner(src, std_alpha(), true);
}

fn raw_std_encode_to_string(src: string) -> string {
    return encode_inner(src, std_alpha(), false);
}

fn raw_std_decode_string(src: string) -> Result[string, error] {
    return decode_inner(src, std_alpha(), false);
}

fn url_encode_to_string(src: string) -> string {
    return encode_inner(src, url_alpha(), true);
}

fn url_decode_string(src: string) -> Result[string, error] {
    return decode_inner(src, url_alpha(), true);
}

fn raw_url_encode_to_string(src: string) -> string {
    return encode_inner(src, url_alpha(), false);
}

fn raw_url_decode_string(src: string) -> Result[string, error] {
    return decode_inner(src, url_alpha(), false);
}
