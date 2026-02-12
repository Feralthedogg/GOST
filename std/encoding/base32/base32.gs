module base32

fn zero_i64() -> i64 { return string_len(""); }
fn one_i64() -> i64 { return string_len("a"); }
fn two_i64() -> i64 { return one_i64() + one_i64(); }
fn three_i64() -> i64 { return two_i64() + one_i64(); }
fn four_i64() -> i64 { return two_i64() + two_i64(); }
fn five_i64() -> i64 { return four_i64() + one_i64(); }
fn six_i64() -> i64 { return five_i64() + one_i64(); }
fn seven_i64() -> i64 { return six_i64() + one_i64(); }
fn eight_i64() -> i64 { return four_i64() + four_i64(); }

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
    return "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
}

fn hex_alpha() -> string {
    return "0123456789ABCDEFGHIJKLMNOPQRSTUV";
}

fn is_ws(b: i32) -> bool {
    return b == byte(" ") || b == byte("\n") || b == byte("\r") || b == byte("\t");
}

fn is_eq(b: i32) -> bool {
    return b == byte("=");
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
    while i < 32 {
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
        let b3: i32 = 0;
        let b4: i32 = 0;
        let has1: bool = false;
        let has2: bool = false;
        let has3: bool = false;
        let has4: bool = false;

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
        if i < n {
            b3 = string_get(src, i);
            i = i + one_i64();
            has3 = true;
        }
        if i < n {
            b4 = string_get(src, i);
            i = i + one_i64();
            has4 = true;
        }

        let i0: i32 = b0 / 8;
        let i1: i32 = (b0 % 8) * 4;
        let i2: i32 = 0;
        let i3: i32 = 0;
        let i4: i32 = 0;
        let i5: i32 = 0;
        let i6: i32 = 0;
        let i7: i32 = 0;

        if has1 {
            i1 = i1 + (b1 / 64);
            i2 = (b1 / 2) % 32;
            i3 = (b1 % 2) * 16;
        }
        if has2 {
            i3 = i3 + (b2 / 16);
            i4 = (b2 % 16) * 2;
        }
        if has3 {
            i4 = i4 + (b3 / 128);
            i5 = (b3 / 4) % 32;
            i6 = (b3 % 4) * 8;
        }
        if has4 {
            i6 = i6 + (b4 / 32);
            i7 = b4 % 32;
        }

        out = string_concat(out, alpha_char(alpha, i0));
        out = string_concat(out, alpha_char(alpha, i1));

        if has1 {
            out = string_concat(out, alpha_char(alpha, i2));
            out = string_concat(out, alpha_char(alpha, i3));
        } else {
            if padded { out = string_concat(out, "======"); }
            continue;
        }

        if has2 {
            out = string_concat(out, alpha_char(alpha, i4));
        } else {
            if padded { out = string_concat(out, "===="); }
            continue;
        }

        if has3 {
            out = string_concat(out, alpha_char(alpha, i5));
            out = string_concat(out, alpha_char(alpha, i6));
        } else {
            if padded { out = string_concat(out, "==="); }
            continue;
        }

        if has4 {
            out = string_concat(out, alpha_char(alpha, i7));
        } else {
            if padded { out = string_concat(out, "="); }
            continue;
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
        if n0 % eight_i64() != zero_i64() {
            return err_str("invalid base32 length");
        }
    } else {
        let rem: i64 = n0 % eight_i64();
        if rem == one_i64() || rem == three_i64() || rem == six_i64() {
            return err_str("invalid raw base32 length");
        }
        if rem == two_i64() {
            work = string_concat(work, "======");
        } else {
            if rem == four_i64() {
                work = string_concat(work, "====");
            } else {
                if rem == five_i64() {
                    work = string_concat(work, "===");
                } else {
                    if rem == seven_i64() {
                        work = string_concat(work, "=");
                    }
                }
            }
        }
    }

    let n: i64 = string_len(work);
    if n % eight_i64() != zero_i64() {
        return err_str("invalid base32 length");
    }

    let i: i64 = zero_i64();
    let out: string = "";
    while i < n {
        let c0: i32 = string_get(work, i);
        let c1: i32 = string_get(work, i + one_i64());
        let c2: i32 = string_get(work, i + two_i64());
        let c3: i32 = string_get(work, i + three_i64());
        let c4: i32 = string_get(work, i + four_i64());
        let c5: i32 = string_get(work, i + five_i64());
        let c6: i32 = string_get(work, i + six_i64());
        let c7: i32 = string_get(work, i + seven_i64());

        let pad: i32 = 0;
        if is_eq(c7) {
            pad = 1;
            if is_eq(c6) {
                pad = 2;
                if is_eq(c5) {
                    pad = 3;
                    if is_eq(c4) {
                        pad = 4;
                        if is_eq(c3) {
                            pad = 5;
                            if is_eq(c2) {
                                pad = 6;
                                if is_eq(c1) {
                                    pad = 7;
                                    if is_eq(c0) {
                                        pad = 8;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        if pad > 0 && i + eight_i64() < n {
            return err_str("invalid base32 padding");
        }

        if pad == 0 {
            if is_eq(c0) || is_eq(c1) || is_eq(c2) || is_eq(c3) || is_eq(c4) || is_eq(c5) || is_eq(c6) || is_eq(c7) {
                return err_str("invalid base32 padding");
            }
        } else {
            if pad == 1 {
                if !is_eq(c7) || is_eq(c0) || is_eq(c1) || is_eq(c2) || is_eq(c3) || is_eq(c4) || is_eq(c5) || is_eq(c6) {
                    return err_str("invalid base32 padding");
                }
            } else {
                if pad == 3 {
                    if !is_eq(c5) || !is_eq(c6) || !is_eq(c7) || is_eq(c0) || is_eq(c1) || is_eq(c2) || is_eq(c3) || is_eq(c4) {
                        return err_str("invalid base32 padding");
                    }
                } else {
                    if pad == 4 {
                        if !is_eq(c4) || !is_eq(c5) || !is_eq(c6) || !is_eq(c7) || is_eq(c0) || is_eq(c1) || is_eq(c2) || is_eq(c3) {
                            return err_str("invalid base32 padding");
                        }
                    } else {
                        if pad == 6 {
                            if !is_eq(c2) || !is_eq(c3) || !is_eq(c4) || !is_eq(c5) || !is_eq(c6) || !is_eq(c7) || is_eq(c0) || is_eq(c1) {
                                return err_str("invalid base32 padding");
                            }
                        } else {
                            return err_str("invalid base32 padding");
                        }
                    }
                }
            }
        }

        let v0: i32 = alpha_index(alpha, c0);
        let v1: i32 = alpha_index(alpha, c1);
        let v2: i32 = 0;
        let v3: i32 = 0;
        let v4: i32 = 0;
        let v5: i32 = 0;
        let v6: i32 = 0;
        let v7: i32 = 0;

        if v0 < 0 || v1 < 0 {
            return err_str("invalid base32 character");
        }
        if !is_eq(c2) {
            v2 = alpha_index(alpha, c2);
            if v2 < 0 { return err_str("invalid base32 character"); }
        }
        if !is_eq(c3) {
            v3 = alpha_index(alpha, c3);
            if v3 < 0 { return err_str("invalid base32 character"); }
        }
        if !is_eq(c4) {
            v4 = alpha_index(alpha, c4);
            if v4 < 0 { return err_str("invalid base32 character"); }
        }
        if !is_eq(c5) {
            v5 = alpha_index(alpha, c5);
            if v5 < 0 { return err_str("invalid base32 character"); }
        }
        if !is_eq(c6) {
            v6 = alpha_index(alpha, c6);
            if v6 < 0 { return err_str("invalid base32 character"); }
        }
        if !is_eq(c7) {
            v7 = alpha_index(alpha, c7);
            if v7 < 0 { return err_str("invalid base32 character"); }
        }

        let b0: i32 = v0 * 8 + v1 / 4;
        let b1: i32 = (v1 % 4) * 64 + v2 * 2 + v3 / 16;
        let b2: i32 = (v3 % 16) * 16 + v4 / 2;
        let b3: i32 = (v4 % 2) * 128 + v5 * 4 + v6 / 8;
        let b4: i32 = (v6 % 8) * 32 + v7;

        if pad == 0 {
            out = string_concat(out, byte_str(b0));
            out = string_concat(out, byte_str(b1));
            out = string_concat(out, byte_str(b2));
            out = string_concat(out, byte_str(b3));
            out = string_concat(out, byte_str(b4));
        } else {
            if pad == 1 {
                out = string_concat(out, byte_str(b0));
                out = string_concat(out, byte_str(b1));
                out = string_concat(out, byte_str(b2));
                out = string_concat(out, byte_str(b3));
            } else {
                if pad == 3 {
                    out = string_concat(out, byte_str(b0));
                    out = string_concat(out, byte_str(b1));
                    out = string_concat(out, byte_str(b2));
                } else {
                    if pad == 4 {
                        out = string_concat(out, byte_str(b0));
                        out = string_concat(out, byte_str(b1));
                    } else {
                        // pad == 6
                        out = string_concat(out, byte_str(b0));
                    }
                }
            }
        }

        i = i + eight_i64();
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

fn hex_encode_to_string(src: string) -> string {
    return encode_inner(src, hex_alpha(), true);
}

fn hex_decode_string(src: string) -> Result[string, error] {
    return decode_inner(src, hex_alpha(), true);
}

fn raw_hex_encode_to_string(src: string) -> string {
    return encode_inner(src, hex_alpha(), false);
}

fn raw_hex_decode_string(src: string) -> Result[string, error] {
    return decode_inner(src, hex_alpha(), false);
}
