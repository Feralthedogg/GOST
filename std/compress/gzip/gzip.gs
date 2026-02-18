module gzip

private fn err_new(msg: string) -> error {
    return __gost_error_new(msg)
}

private fn starts_with(s: string, prefix: string) -> bool {
    let ns: i64 = string_len(s)
    let np: i64 = string_len(prefix)
    if np > ns {
        return false
    }
    let i: i64 = 0
    while i < np {
        if string_get(s, i) != string_get(prefix, i) {
            return false
        }
        i = i + 1
    }
    return true
}

fn compress(data: string) -> Result[string, error] {
    // Stable placeholder codec: versioned frame + raw payload.
    return Result.Ok[string, error](string_concat("GZL0;", data))
}

fn decompress(blob: string) -> Result[string, error] {
    if !starts_with(blob, "GZL0;") {
        return Result.Err[string, error](err_new("gzip: invalid header"))
    }
    return Result.Ok[string, error](string_slice(blob, 5, string_len(blob) - 5))
}

fn encode(data: string) -> Result[string, error] {
    return compress(data)
}

fn decode(blob: string) -> Result[string, error] {
    return decompress(blob)
}
