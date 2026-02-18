module bufio

copy struct Reader {
    src: string
    pos: i64
}

copy struct Scanner {
    src: string
    pos: i64
    token: string
    done: bool
    last_err: error
}

private fn err_new(msg: string) -> error {
    return __gost_error_new(msg)
}

private fn byte(c: string) -> i32 {
    return string_get(c, 0)
}

fn new_reader(src: string) -> Reader {
    return Reader { src = src, pos = 0 }
}

fn read_file(path: string) -> Result[Reader, error] {
    let data = __gost_os_read_file(path)
    if __gost_os_last_status() != 0 {
        let msg = __gost_os_last_error()
        if string_len(msg) == 0 {
            return Result.Err[Reader, error](err_new("bufio read_file failed"))
        }
        return Result.Err[Reader, error](err_new(msg))
    }
    return Result.Ok[Reader, error](new_reader(data))
}

fn remaining(reader: Reader) -> i64 {
    let total_len: i64 = string_len(reader.src)
    if reader.pos >= total_len {
        return 0
    }
    return total_len - reader.pos
}

fn is_eof(reader: Reader) -> bool {
    return reader.pos >= string_len(reader.src)
}

fn read_bytes(reader: mutref[Reader], delim: i32) -> Result[string, error] {
    let src_copy = ""
    src_copy = reader.src
    let pos_copy: i64 = 0
    pos_copy = reader.pos
    if pos_copy >= string_len(src_copy) {
        return Result.Err[string, error](err_new("EOF"))
    }
    let start_pos: i64 = 0
    start_pos = pos_copy
    let idx: i64 = 0
    idx = pos_copy
    while idx < string_len(src_copy) {
        if string_get(src_copy, idx) == delim {
            reader.pos = idx + 1
            return Result.Ok[string, error](string_slice(src_copy, start_pos, idx - start_pos + 1))
        }
        idx = idx + 1
    }
    reader.pos = string_len(src_copy)
    return Result.Ok[string, error](string_slice(src_copy, start_pos, string_len(src_copy) - start_pos))
}

fn read_line(reader: mutref[Reader]) -> Result[string, error] {
    let src_copy = ""
    src_copy = reader.src
    let pos_copy: i64 = 0
    pos_copy = reader.pos
    if pos_copy < 0 {
        return Result.Err[string, error](err_new("invalid reader position"))
    }
    let n: i64 = string_len(src_copy)
    if pos_copy >= n {
        return Result.Err[string, error](err_new("EOF"))
    }
    let idx: i64 = 0
    idx = pos_copy
    while idx < n && string_get(src_copy, idx) != byte("\n") {
        idx = idx + 1
    }
    let raw = ""
    if idx < n {
        reader.pos = idx + 1
        raw = string_slice(src_copy, pos_copy, idx - pos_copy + 1)
    } else {
        reader.pos = n
        raw = string_slice(src_copy, pos_copy, n - pos_copy)
    }
    let m: i64 = string_len(raw)
    if m > 0 && string_get(raw, m - 1) == byte("\n") {
        if m > 1 && string_get(raw, m - 2) == byte("\r") {
            return Result.Ok[string, error](string_slice(raw, 0, m - 2))
        }
        return Result.Ok[string, error](string_slice(raw, 0, m - 1))
    }
    return Result.Ok[string, error](raw)
}

fn new_scanner(src: string) -> Scanner {
    return Scanner {
        src = src,
        pos = 0,
        token = "",
        done = false,
        last_err = err_new(""),
    }
}

fn scan(s: mutref[Scanner]) -> bool {
    let done_copy = false
    done_copy = s.done
    if done_copy {
        return false
    }
    let src_copy = ""
    src_copy = s.src
    let pos_copy: i64 = 0
    pos_copy = s.pos
    if pos_copy < 0 {
        s.last_err = err_new("invalid scanner position")
        s.done = true
        return false
    }
    if pos_copy >= string_len(src_copy) {
        s.done = true
        s.last_err = err_new("")
        return false
    }
    let idx: i64 = 0
    idx = pos_copy
    while idx < string_len(src_copy) && string_get(src_copy, idx) != byte("\n") {
        idx = idx + 1
    }
    let line = ""
    if idx < string_len(src_copy) {
        if idx > pos_copy && string_get(src_copy, idx - 1) == byte("\r") {
            line = string_slice(src_copy, pos_copy, idx - pos_copy - 1)
        } else {
            line = string_slice(src_copy, pos_copy, idx - pos_copy)
        }
        s.pos = idx + 1
    } else {
        line = string_slice(src_copy, pos_copy, string_len(src_copy) - pos_copy)
        s.pos = string_len(src_copy)
    }
    s.token = line
    let new_pos: i64 = 0
    new_pos = s.pos
    if new_pos >= string_len(src_copy) {
        s.done = true
        s.last_err = err_new("")
    }
    return true
}

fn text(s: Scanner) -> string {
    return s.token
}

fn err(s: Scanner) -> error {
    return s.last_err
}
