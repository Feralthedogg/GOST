module io

copy struct Reader {
    read_fn: fn(i32) -> Result[string, error]
}

copy struct Writer {
    write_fn: fn(string) -> Result[i64, error]
}

private fn io_error_new(msg: string) -> error {
    return __gost_error_new(msg)
}

fn new_reader(read_fn: fn(i32) -> Result[string, error]) -> Reader {
    return Reader { read_fn = read_fn }
}

fn new_writer(write_fn: fn(string) -> Result[i64, error]) -> Writer {
    return Writer { write_fn = write_fn }
}

fn eof() -> error {
    return io_error_new("EOF")
}

fn read_all(r: Reader) -> Result[string, error] {
    let out = ""
    let done = false
    while !done {
        let rf = r.read_fn
        match rf(4096) {
            Result.Ok(chunk) => {
                if string_len(chunk) == 0 {
                    done = true
                } else {
                    out = string_concat(out, chunk)
                }
            },
            Result.Err(e) => {
                return Result.Err[string, error](e)
            },
            _ => {
                return Result.Err[string, error](io_error_new("invalid read result"))
            },
        }
    }
    return Result.Ok[string, error](out)
}

fn copy_all(r: Reader, w: Writer) -> Result[i64, error] {
    let total: i64 = 0
    let done = false
    while !done {
        let rf = r.read_fn
        match rf(4096) {
            Result.Ok(chunk) => {
                if string_len(chunk) == 0 {
                    done = true
                } else {
                    let wf = w.write_fn
                    match wf(chunk) {
                        Result.Ok(n) => {
                            total = total + n
                        },
                        Result.Err(e) => {
                            return Result.Err[i64, error](e)
                        },
                        _ => {
                            return Result.Err[i64, error](io_error_new("invalid write result"))
                        },
                    }
                }
            },
            Result.Err(e) => {
                return Result.Err[i64, error](e)
            },
            _ => {
                return Result.Err[i64, error](io_error_new("invalid read result"))
            },
        }
    }
    return Result.Ok[i64, error](total)
}

fn copy_n(r: Reader, w: Writer, limit: i64) -> Result[i64, error] {
    if limit <= 0 {
        return Result.Ok[i64, error](0)
    }
    let total: i64 = 0
    let done = false
    while total < limit && !done {
        let remain: i64 = limit - total
        let chunk_size: i32 = 4096
        if remain < 4096 {
            chunk_size = remain as i32
        }
        let rf = r.read_fn
        match rf(chunk_size) {
            Result.Ok(chunk) => {
                if string_len(chunk) == 0 {
                    done = true
                }
                if !done {
                    let to_write = chunk
                    if string_len(chunk) > remain {
                        to_write = string_slice(chunk, 0, remain)
                    }
                    let wf = w.write_fn
                    match wf(to_write) {
                        Result.Ok(n) => {
                            total = total + n
                        },
                        Result.Err(e) => {
                            return Result.Err[i64, error](e)
                        },
                        _ => {
                            return Result.Err[i64, error](io_error_new("invalid write result"))
                        },
                    }
                }
            },
            Result.Err(e) => {
                return Result.Err[i64, error](e)
            },
            _ => {
                return Result.Err[i64, error](io_error_new("invalid read result"))
            },
        }
    }
    return Result.Ok[i64, error](total)
}
