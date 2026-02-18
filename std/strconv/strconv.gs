module strconv

import "std/strings"

private fn strconv_zero_i64() -> i64 { return string_len("")
 }
private fn strconv_one_i64() -> i64 { return string_len("a")
 }
private fn strconv_byte(c: string) -> i32 { return string_get(c, strconv_zero_i64())
 }

private fn strconv_error_new(msg: string) -> error {
    return __gost_error_new(msg)
}

private fn strconv_is_space(b: i32) -> bool {
    if b == strconv_byte(" ") { return true
 }
    if b == strconv_byte("\t") { return true
 }
    if b == strconv_byte("\n") { return true
 }
    if b == strconv_byte("\r") { return true
 }
    return false
}

private fn strconv_trim_space(s: string) -> string {
    let n: i64 = string_len(s)
    if n == strconv_zero_i64() {
        return s
    }
    let left: i64 = strconv_zero_i64()
    while left < n {
        if !strconv_is_space(string_get(s, left)) {
            break
        }
        left = left + strconv_one_i64()
    }
    if left >= n {
        return ""
    }
    let right: i64 = n - strconv_one_i64()
    while right >= left {
        if !strconv_is_space(string_get(s, right)) {
            break
        }
        if right == strconv_zero_i64() {
            return ""
        }
        right = right - strconv_one_i64()
    }
    return string_slice(s, left, right - left + strconv_one_i64())
}

private fn strconv_digit_char(d: i32) -> string {
    return string_from_byte(strconv_byte("0") + d)
}

fn format_i64(v: i64) -> string {
    if v == strconv_zero_i64() {
        return "0"
    }
    let neg: bool = v < strconv_zero_i64()
    let x: i64 = v
    if neg {
        x = strconv_zero_i64() - x
    }
    let out: string = ""
    while x > strconv_zero_i64() {
        let d: i32 = (x % 10 as i64) as i32
        out = string_concat(strconv_digit_char(d), out)
        x = x / 10 as i64
    }
    if neg {
        out = string_concat("-", out)
    }
    return out
}

fn itoa(v: i32) -> string {
    return format_i64(v as i64)
}

fn format_bool(v: bool) -> string {
    if v {
        return "true"
    }
    return "false"
}

fn parse_i64(s: string) -> Result[i64, error] {
    let raw: string = strconv_trim_space(s)
    let n: i64 = string_len(raw)
    if n == strconv_zero_i64() {
        return Result.Err[i64, error](strconv_error_new("invalid integer: empty"))
    }

    let i: i64 = strconv_zero_i64()
    let neg: bool = false
    let first: i32 = string_get(raw, i)
    if first == strconv_byte("-") {
        neg = true
        i = i + strconv_one_i64()
    } else {
        if first == strconv_byte("+") {
            i = i + strconv_one_i64()
        }
    }
    if i >= n {
        return Result.Err[i64, error](strconv_error_new("invalid integer: sign only"))
    }

    let acc: i64 = strconv_zero_i64()
    while i < n {
        let b: i32 = string_get(raw, i)
        if b < strconv_byte("0") || b > strconv_byte("9") {
            return Result.Err[i64, error](strconv_error_new("invalid integer: non-digit"))
        }
        let d: i64 = (b - strconv_byte("0")) as i64
        acc = acc * 10 as i64 + d
        i = i + strconv_one_i64()
    }

    if neg {
        return Result.Ok[i64, error](strconv_zero_i64() - acc)
    }
    return Result.Ok[i64, error](acc)
}

fn atoi(s: string) -> Result[i32, error] {
    let r = parse_i64(s)
    let v: i64 = strconv_zero_i64()
    let err_v: error = strconv_error_new("invalid integer")
    let bad: bool = false
    match r {
        Result.Ok(x) => { v = x
 },
        Result.Err(e) => { err_v = e
 bad = true
 },
        _ => { bad = true
 },
    }
    if bad {
        return Result.Err[i32, error](err_v)
    }
    if v < -2147483648 as i64 || v > 2147483647 as i64 {
        return Result.Err[i32, error](strconv_error_new("invalid integer: out of i32 range"))
    }
    return Result.Ok[i32, error](v as i32)
}

fn parse_bool(s: string) -> Result[bool, error] {
    let raw: string = strconv_trim_space(s)
    if equal(raw, "true") { return Result.Ok[bool, error](true)
 }
    if equal(raw, "1") { return Result.Ok[bool, error](true)
 }
    if equal(raw, "t") { return Result.Ok[bool, error](true)
 }
    if equal(raw, "T") {
        return Result.Ok[bool, error](true)
    }
    if equal(raw, "false") { return Result.Ok[bool, error](false)
 }
    if equal(raw, "0") { return Result.Ok[bool, error](false)
 }
    if equal(raw, "f") { return Result.Ok[bool, error](false)
 }
    if equal(raw, "F") {
        return Result.Ok[bool, error](false)
    }
    return Result.Err[bool, error](strconv_error_new("invalid bool"))
}

fn parse_i64_loose(s: string, fallback: i64) -> i64 {
    let raw: string = strconv_trim_space(s)
    let n: i64 = string_len(raw)
    if n == strconv_zero_i64() { return fallback
 }

    let i: i64 = strconv_zero_i64()
    let neg: bool = false
    let first: i32 = string_get(raw, i)
    if first == strconv_byte("-") {
        neg = true
        i = i + strconv_one_i64()
    } else {
        if first == strconv_byte("+") {
            i = i + strconv_one_i64()
        }
    }
    if i >= n { return fallback
 }

    let acc: i64 = strconv_zero_i64()
    while i < n {
        let b: i32 = string_get(raw, i)
        if b < strconv_byte("0") { return fallback
 }
        if b > strconv_byte("9") { return fallback
 }
        let d: i64 = (b - strconv_byte("0")) as i64
        acc = acc * 10 as i64 + d
        i = i + strconv_one_i64()
    }
    if neg { return strconv_zero_i64() - acc
 }
    return acc
}

fn atoi_loose(s: string, fallback: i32) -> i32 {
    let v: i64 = parse_i64_loose(s, fallback as i64)
    let i32_min: i64 = -2147483647 as i64 - 1 as i64
    let i32_max: i64 = 2147483647 as i64
    if v < i32_min { return fallback
 }
    if v > i32_max { return fallback
 }
    return v as i32
}

fn parse_bool_loose(s: string, fallback: bool) -> bool {
    let raw: string = strconv_trim_space(s)
    if equal(raw, "true") { return true
 }
    if equal(raw, "1") { return true
 }
    if equal(raw, "t") { return true
 }
    if equal(raw, "T") { return true
 }
    if equal(raw, "false") { return false
 }
    if equal(raw, "0") { return false
 }
    if equal(raw, "f") { return false
 }
    if equal(raw, "F") { return false
 }
    return fallback
}

private fn strconv_pow10_i64(exp: i32) -> i64 {
    if exp <= 0 {
        return 1
    }
    let out: i64 = 1
    let i: i32 = 0
    while i < exp {
        out = out * 10
        i = i + 1
    }
    return out
}

private fn strconv_pad_left_zeros(s: string, width: i32) -> string {
    let out: string = s
    while string_len(out) < width as i64 {
        out = string_concat("0", out)
    }
    return out
}

fn format_f64(v: f64, precision: i32) -> string {
    if v != v {
        return "NaN"
    }
    let prec: i32 = precision
    if prec < 0 {
        prec = 0
    }
    if prec > 12 {
        prec = 12
    }

    let neg: bool = false
    let x: f64 = v
    if x < 0.0 {
        neg = true
        x = 0.0 - x
    }

    let whole: i64 = x as i64
    let frac: f64 = x - (whole as f64)
    let scale: i64 = strconv_pow10_i64(prec)
    let scaled: i64 = ((frac * (scale as f64)) + 0.5) as i64

    let carry: i64 = 0
    let frac_int: i64 = scaled
    if frac_int >= scale {
        carry = 1
        frac_int = frac_int - scale
    }

    let int_text: string = format_i64(whole + carry)
    let out: string = int_text
    if prec > 0 {
        let frac_text = strconv_pad_left_zeros(format_i64(frac_int), prec)
        out = string_concat(out, ".")
        out = string_concat(out, frac_text)
    }
    if neg {
        return string_concat("-", out)
    }
    return out
}

private fn strconv_parse_exp(raw: string, idx: mutref[i64]) -> Result[i32, error] {
    let n: i64 = string_len(raw)
    if *idx >= n {
        return Result.Err[i32, error](strconv_error_new("invalid float exponent"))
    }

    let exp_neg: bool = false
    let c0: i32 = string_get(raw, *idx)
    if c0 == strconv_byte("+") {
        *idx = *idx + 1
    } else {
        if c0 == strconv_byte("-") {
            exp_neg = true
            *idx = *idx + 1
        }
    }

    if *idx >= n {
        return Result.Err[i32, error](strconv_error_new("invalid float exponent"))
    }

    let exp_val: i32 = 0
    let has_digit: bool = false
    while *idx < n {
        let b: i32 = string_get(raw, *idx)
        if b < strconv_byte("0") || b > strconv_byte("9") {
            break
        }
        has_digit = true
        exp_val = exp_val * 10 + (b - strconv_byte("0"))
        *idx = *idx + 1
    }
    if !has_digit {
        return Result.Err[i32, error](strconv_error_new("invalid float exponent"))
    }
    if exp_neg {
        return Result.Ok[i32, error](0 - exp_val)
    }
    return Result.Ok[i32, error](exp_val)
}

fn parse_f64(s: string) -> Result[f64, error] {
    let raw: string = strconv_trim_space(s)
    let n: i64 = string_len(raw)
    if n == 0 {
        return Result.Err[f64, error](strconv_error_new("invalid float: empty"))
    }

    let i: i64 = 0
    let neg: bool = false
    let c0: i32 = string_get(raw, i)
    if c0 == strconv_byte("+") {
        i = i + 1
    } else {
        if c0 == strconv_byte("-") {
            neg = true
            i = i + 1
        }
    }
    if i >= n {
        return Result.Err[f64, error](strconv_error_new("invalid float: sign only"))
    }

    let int_part: f64 = 0.0
    let frac_part: f64 = 0.0
    let frac_scale: f64 = 1.0
    let saw_digit: bool = false

    while i < n {
        let b: i32 = string_get(raw, i)
        if b < strconv_byte("0") || b > strconv_byte("9") {
            break
        }
        saw_digit = true
        int_part = int_part * 10.0 + ((b - strconv_byte("0")) as f64)
        i = i + 1
    }

    if i < n && string_get(raw, i) == strconv_byte(".") {
        i = i + 1
        while i < n {
            let b: i32 = string_get(raw, i)
            if b < strconv_byte("0") || b > strconv_byte("9") {
                break
            }
            saw_digit = true
            frac_part = frac_part * 10.0 + ((b - strconv_byte("0")) as f64)
            frac_scale = frac_scale * 10.0
            i = i + 1
        }
    }

    if !saw_digit {
        return Result.Err[f64, error](strconv_error_new("invalid float: no digits"))
    }

    let exp10: i32 = 0
    if i < n {
        let e: i32 = string_get(raw, i)
        if e == strconv_byte("e") || e == strconv_byte("E") {
            i = i + 1
            match strconv_parse_exp(raw, &mut i) {
                Result.Ok(v) => { exp10 = v },
                Result.Err(err) => { return Result.Err[f64, error](err) },
                _ => { return Result.Err[f64, error](strconv_error_new("invalid float exponent")) },
            }
        }
    }

    if i != n {
        return Result.Err[f64, error](strconv_error_new("invalid float: trailing characters"))
    }

    let out: f64 = int_part + (frac_part / frac_scale)
    if exp10 > 0 {
        let k: i32 = 0
        while k < exp10 {
            out = out * 10.0
            k = k + 1
        }
    } else {
        if exp10 < 0 {
            let k: i32 = 0
            while k < (0 - exp10) {
                out = out / 10.0
                k = k + 1
            }
        }
    }

    if neg {
        out = 0.0 - out
    }
    return Result.Ok[f64, error](out)
}

fn atof(s: string) -> Result[f64, error] {
    return parse_f64(s)
}

