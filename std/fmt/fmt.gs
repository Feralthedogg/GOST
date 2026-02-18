module fmt

private fn join_with_sep(parts: []string, sep: string) -> string {
    let n: i64 = slice_len[string](&parts)
    if n == 0 {
        return ""
    }
    let out = ""
    let i: i64 = 0
    while i < n {
        if i > 0 {
            out = string_concat(out, sep)
        }
        out = string_concat(out, slice_get_copy[string](&parts, i))
        i = i + 1
    }
    return out
}

fn print(msg: string) {
    __gost_println(msg)
}

fn println(msg: string) {
    __gost_println(msg)
}

fn sprint(args: []string) -> string {
    return join_with_sep(args, " ")
}

fn printv(args: []string) {
    print(sprint(args))
}

fn printlnv(args: []string) {
    println(sprint(args))
}

fn sprintf(format: string, args: []string) -> string {
    let out = ""
    let n: i64 = string_len(format)
    let i: i64 = 0
    let ai: i64 = 0
    while i < n {
        let c = string_get(format, i)
        if c != 37 {
            out = string_concat(out, string_slice(format, i, 1))
            i = i + 1
            continue
        }
        if i + 1 >= n {
            out = string_concat(out, "%")
            break
        }
        let spec = string_get(format, i + 1)
        if spec == 37 {
            out = string_concat(out, "%")
            i = i + 2
            continue
        }
        if spec == 115 || spec == 100 || spec == 102 || spec == 118 {
            if ai < slice_len[string](&args) {
                out = string_concat(out, slice_get_copy[string](&args, ai))
                ai = ai + 1
            } else {
                out = string_concat(out, "%")
                out = string_concat(out, string_slice(format, i + 1, 1))
            }
            i = i + 2
            continue
        }
        out = string_concat(out, "%")
        out = string_concat(out, string_slice(format, i + 1, 1))
        i = i + 2
    }
    return out
}

fn printf(format: string, args: []string) {
    let out = sprintf(format, args)
    __gost_println(out)
}

private fn fmt_digit_char(d: i64) -> string { return string_from_byte(48 + d as i32)
 }

fn arg_i64(v: i64) -> string {
    if v == 0 {
        return "0"
    }
    let n = v
    let neg = false
    if n < 0 {
        neg = true
        n = 0 - n
    }
    let out = ""
    while n > 0 {
        let d = n % 10
        out = string_concat(fmt_digit_char(d), out)
        n = n / 10
    }
    if neg {
        out = string_concat("-", out)
    }
    return out
}

private fn fmt_pad_frac_6(v: i64) -> string {
    let out = arg_i64(v)
    while string_len(out) < 6 {
        out = string_concat("0", out)
    }
    return out
}

private fn fmt_trim_frac(frac: string) -> string {
    let n: i64 = string_len(frac)
    let end: i64 = n
    while end > 1 {
        if string_get(frac, end - 1) != 48 {
            break
        }
        end = end - 1
    }
    return string_slice(frac, 0, end)
}

fn arg_f64(v: f64) -> string {
    if v != v { return "NaN"
 }

    let neg = false
    let x = v
    if x < 0.0 {
        neg = true
        x = 0.0 - x
    }

    let whole: i64 = x as i64
    let frac: f64 = x - (whole as f64)
    let scale: i64 = 1000000
    let scaled: i64 = ((frac * (scale as f64)) + 0.5) as i64

    let carry: i64 = 0
    let frac_int: i64 = scaled
    if frac_int >= scale {
        carry = 1
        frac_int = frac_int - scale
    }

    let int_text = arg_i64(whole + carry)
    let frac_text = fmt_trim_frac(fmt_pad_frac_6(frac_int))
    let out = string_concat(int_text, ".")
    out = string_concat(out, frac_text)
    if neg {
        return string_concat("-", out)
    }
    return out
}

fn arg_i32(v: i32) -> string { return arg_i64(v as i64)
 }
fn arg_bool(v: bool) -> string {
    if v { return "true"
 }
    return "false"
}
fn arg_f32(v: f32) -> string { return arg_f64(v as f64)
 }
