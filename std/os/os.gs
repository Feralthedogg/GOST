module os

copy struct ExecResult {
    code: i32
    output: string
}

private fn os_error_new(msg: string) -> error {
    return __gost_error_new(msg)
}

private fn split_by_byte(s: string, sep: i32) -> []string {
    let out = make_slice[string](0, 0)
    let n: i64 = string_len(s)
    let start: i64 = 0
    let i: i64 = 0
    while i < n {
        if string_get(s, i) == sep {
            slice_push[string](&mut out, string_slice(s, start, i - start))
            start = i + 1
        }
        i = i + 1
    }
    slice_push[string](&mut out, string_slice(s, start, n - start))
    return out
}

private fn contains_byte(s: string, b: i32) -> bool {
    let i: i64 = 0
    let n: i64 = string_len(s)
    while i < n {
        if string_get(s, i) == b {
            return true
        }
        i = i + 1
    }
    return false
}

private fn split_items(raw: string) -> []string {
    // Backward compatible: old runtime used '\n', new runtime uses '\0'.
    if contains_byte(raw, 0) {
        return split_by_byte(raw, 0)
    }
    return split_by_byte(raw, 10)
}

private fn non_empty_error(fallback: string) -> error {
    let msg = __gost_os_last_error()
    if string_len(msg) == 0 {
        return os_error_new(fallback)
    }
    return os_error_new(msg)
}

fn last_status() -> i32 {
    return __gost_os_last_status()
}

fn last_error() -> string {
    return __gost_os_last_error()
}

fn last_output() -> string {
    return __gost_os_last_output()
}

fn exec(cmd: string) -> Result[ExecResult, error] {
    let code = __gost_os_exec(cmd)
    if __gost_os_last_status() != 0 {
        return Result.Err[ExecResult, error](non_empty_error("os exec failed"))
    }
    return Result.Ok[ExecResult, error](ExecResult {
        code = code,
        output = __gost_os_last_output(),
    })
}

fn pipe(cmd: string) -> Result[string, error] {
    let code = __gost_os_pipe(cmd)
    if __gost_os_last_status() != 0 {
        return Result.Err[string, error](non_empty_error("os pipe failed"))
    }
    let out = __gost_os_last_output()
    if code != 0 {
        if string_len(out) == 0 {
            return Result.Err[string, error](os_error_new("os pipe command failed"))
        }
        return Result.Err[string, error](os_error_new(out))
    }
    return Result.Ok[string, error](out)
}

fn read_file(path: string) -> Result[string, error] {
    let data = __gost_os_read_file(path)
    if __gost_os_last_status() == 0 {
        return Result.Ok[string, error](data)
    }
    return Result.Err[string, error](non_empty_error("os read_file failed"))
}

fn write_file(path: string, data: string) -> Result[i64, error] {
    let n = __gost_os_write_file(path, data)
    if n >= 0 && __gost_os_last_status() == 0 {
        return Result.Ok[i64, error](n)
    }
    return Result.Err[i64, error](non_empty_error("os write_file failed"))
}

fn remove(path: string) -> Result[i32, error] {
    let rc = __gost_os_remove(path)
    if rc == 0 && __gost_os_last_status() == 0 {
        return Result.Ok[i32, error](0)
    }
    return Result.Err[i32, error](non_empty_error("os remove failed"))
}

fn mkdir(path: string) -> Result[i32, error] {
    let rc = __gost_os_mkdir(path)
    if rc == 0 && __gost_os_last_status() == 0 {
        return Result.Ok[i32, error](0)
    }
    return Result.Err[i32, error](non_empty_error("os mkdir failed"))
}

fn readdir(path: string) -> Result[[]string, error] {
    let raw = __gost_os_readdir(path)
    if __gost_os_last_status() != 0 {
        return Result.Err[[]string, error](non_empty_error("os readdir failed"))
    }
    if string_len(raw) == 0 {
        let empty = make_slice[string](0, 0)
        return Result.Ok[[]string, error](empty)
    }
    return Result.Ok[[]string, error](split_items(raw))
}

fn stat_size(path: string) -> Result[i64, error] {
    let n = __gost_os_stat_size(path)
    if n >= 0 && __gost_os_last_status() == 0 {
        return Result.Ok[i64, error](n)
    }
    return Result.Err[i64, error](non_empty_error("os stat failed"))
}

fn getwd() -> Result[string, error] {
    let p = __gost_os_getwd()
    if __gost_os_last_status() == 0 {
        return Result.Ok[string, error](p)
    }
    return Result.Err[string, error](non_empty_error("os getwd failed"))
}

fn chdir(path: string) -> Result[i32, error] {
    let rc = __gost_os_chdir(path)
    if rc == 0 && __gost_os_last_status() == 0 {
        return Result.Ok[i32, error](0)
    }
    return Result.Err[i32, error](non_empty_error("os chdir failed"))
}

fn getenv(key: string) -> string {
    return __gost_os_getenv(key)
}

fn setenv(key: string, value: string) -> Result[i32, error] {
    let rc = __gost_os_setenv(key, value)
    if rc == 0 && __gost_os_last_status() == 0 {
        return Result.Ok[i32, error](0)
    }
    return Result.Err[i32, error](non_empty_error("os setenv failed"))
}

fn args() -> []string {
    let raw = __gost_os_args()
    if string_len(raw) == 0 {
        return make_slice[string](0, 0)
    }
    return split_items(raw)
}

fn exit(code: i32) -> i32 {
    return __gost_process_exit(code)
}
