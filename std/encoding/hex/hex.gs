module hex

fn zero_i64() -> i64 { return string_len(""); }
fn one_i64() -> i64 { return string_len("a"); }
fn two_i64() -> i64 { return one_i64() + one_i64(); }

fn error_new(msg: string) -> error {
    return __gost_error_new(msg);
}

fn ok_str(v: string) -> Result[string, error] {
    return Result.Ok[string, error](v);
}

fn err_str(msg: string) -> Result[string, error] {
    return Result.Err[string, error](error_new(msg));
}

fn digits() -> string {
    return "0123456789abcdef";
}

fn byte_str(b: i32) -> string {
    return string_from_byte(b);
}

fn nibble_char(n: i32) -> string {
    return string_slice(digits(), n, one_i64());
}

fn hex_val(b: i32) -> i32 {
    if b >= string_get("0", zero_i64()) && b <= string_get("9", zero_i64()) {
        return b - string_get("0", zero_i64());
    }
    if b >= string_get("a", zero_i64()) && b <= string_get("f", zero_i64()) {
        return 10 + (b - string_get("a", zero_i64()));
    }
    if b >= string_get("A", zero_i64()) && b <= string_get("F", zero_i64()) {
        return 10 + (b - string_get("A", zero_i64()));
    }
    return -1;
}

fn encoded_len(n: i64) -> i64 {
    return n + n;
}

fn decoded_len(n: i64) -> i64 {
    return n / two_i64();
}

fn encode_to_string(src: string) -> string {
    let n: i64 = string_len(src);
    let i: i64 = zero_i64();
    let out: string = "";
    while i < n {
        let b: i32 = string_get(src, i);
        let hi: i32 = b / 16;
        let lo: i32 = b % 16;
        out = string_concat(out, nibble_char(hi));
        out = string_concat(out, nibble_char(lo));
        i = i + one_i64();
    }
    return out;
}

fn decode_string(src: string) -> Result[string, error] {
    let n: i64 = string_len(src);
    if n % two_i64() != zero_i64() {
        return err_str("invalid hex length");
    }

    let i: i64 = zero_i64();
    let out: string = "";
    while i < n {
        let c0: i32 = string_get(src, i);
        let c1: i32 = string_get(src, i + one_i64());
        let v0: i32 = hex_val(c0);
        let v1: i32 = hex_val(c1);
        if v0 < 0 || v1 < 0 {
            return err_str("invalid hex character");
        }
        out = string_concat(out, byte_str(v0 * 16 + v1));
        i = i + two_i64();
    }
    return ok_str(out);
}

