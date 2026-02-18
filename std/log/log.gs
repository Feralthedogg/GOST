module log

copy struct Logger {
    prefix: string
    min_level: i32
}

fn level_debug() -> i32 { return 10 }
fn level_info() -> i32 { return 20 }
fn level_warn() -> i32 { return 30 }
fn level_error() -> i32 { return 40 }

fn new(prefix: string) -> Logger {
    return Logger { prefix = prefix, min_level = level_info() }
}

fn with_level(prefix: string, min_level: i32) -> Logger {
    return Logger { prefix = prefix, min_level = min_level }
}

fn set_min_level(l: Logger, min_level: i32) -> Logger {
    let out = l
    out.min_level = min_level
    return out
}

private fn i64_to_string(v: i64) -> string {
    if v == 0 {
        return "0"
    }
    let x: i64 = v
    let neg: bool = x < 0
    if neg {
        x = -x
    }
    let out: string = ""
    while x > 0 {
        let d: i64 = x % 10
        let ch: i32 = 48 + (d as i32)
        out = string_concat(string_from_byte(ch), out)
        x = x / 10
    }
    if neg {
        out = string_concat("-", out)
    }
    return out
}

private fn join_args(args: []string) -> string {
    let n: i64 = slice_len[string](&args)
    if n == 0 {
        return ""
    }
    let i: i64 = 0
    let out: string = ""
    while i < n {
        if i > 0 {
            out = string_concat(out, " ")
        }
        out = string_concat(out, slice_get_copy[string](&args, i))
        i = i + 1
    }
    return out
}

private fn level_name(level: i32) -> string {
    if level >= level_error() { return "ERROR" }
    if level >= level_warn() { return "WARN" }
    if level >= level_info() { return "INFO" }
    return "DEBUG"
}

private fn emit(l: Logger, level: i32, msg: string) {
    if level < l.min_level {
        return
    }
    let line = "["
    line = string_concat(line, i64_to_string(__gost_now_ms()))
    line = string_concat(line, "] [")
    line = string_concat(line, level_name(level))
    line = string_concat(line, "]")
    if string_len(l.prefix) > 0 {
        line = string_concat(line, " ")
        line = string_concat(line, l.prefix)
        line = string_concat(line, ":")
    }
    line = string_concat(line, " ")
    line = string_concat(line, msg)
    __gost_println(line)
}

fn debug(l: Logger, msg: string) {
    emit(l, level_debug(), msg)
}

fn info(l: Logger, msg: string) {
    emit(l, level_info(), msg)
}

fn warn(l: Logger, msg: string) {
    emit(l, level_warn(), msg)
}

fn error(l: Logger, msg: string) {
    emit(l, level_error(), msg)
}

fn debugf(l: Logger, format: string, args: []string) {
    let msg = format
    let extra = join_args(args)
    if string_len(extra) > 0 {
        msg = string_concat(msg, " ")
        msg = string_concat(msg, extra)
    }
    debug(l, msg)
}

fn infof(l: Logger, format: string, args: []string) {
    let msg = format
    let extra = join_args(args)
    if string_len(extra) > 0 {
        msg = string_concat(msg, " ")
        msg = string_concat(msg, extra)
    }
    info(l, msg)
}

fn warnf(l: Logger, format: string, args: []string) {
    let msg = format
    let extra = join_args(args)
    if string_len(extra) > 0 {
        msg = string_concat(msg, " ")
        msg = string_concat(msg, extra)
    }
    warn(l, msg)
}

fn errorf(l: Logger, format: string, args: []string) {
    let msg = format
    let extra = join_args(args)
    if string_len(extra) > 0 {
        msg = string_concat(msg, " ")
        msg = string_concat(msg, extra)
    }
    error(l, msg)
}

fn print_debug(msg: string) {
    debug(with_level("", level_debug()), msg)
}

fn print_info(msg: string) {
    info(new(""), msg)
}

fn print_warn(msg: string) {
    warn(new(""), msg)
}

fn print_error(msg: string) {
    error(new(""), msg)
}
