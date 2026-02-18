module unicode

private fn byte(c: string) -> i32 {
    return string_get(c, 0)
}

fn is_upper(ch: i32) -> bool {
    return ch >= byte("A") && ch <= byte("Z")
}

fn is_lower(ch: i32) -> bool {
    return ch >= byte("a") && ch <= byte("z")
}

fn is_letter(ch: i32) -> bool {
    return is_upper(ch) || is_lower(ch)
}

fn is_digit(ch: i32) -> bool {
    return ch >= byte("0") && ch <= byte("9")
}

fn is_hex_digit(ch: i32) -> bool {
    if is_digit(ch) {
        return true
    }
    if ch >= byte("a") && ch <= byte("f") {
        return true
    }
    if ch >= byte("A") && ch <= byte("F") {
        return true
    }
    return false
}

fn is_space(ch: i32) -> bool {
    if ch == byte(" ") {
        return true
    }
    if ch == byte("\t") {
        return true
    }
    if ch == byte("\n") {
        return true
    }
    if ch == byte("\r") {
        return true
    }
    return false
}

fn is_alnum(ch: i32) -> bool {
    return is_letter(ch) || is_digit(ch)
}

fn to_upper(ch: i32) -> i32 {
    if is_lower(ch) {
        return ch - 32
    }
    return ch
}

fn to_lower(ch: i32) -> i32 {
    if is_upper(ch) {
        return ch + 32
    }
    return ch
}

