module errors

import "std/strings"

private fn err_new(msg: string) -> error {
    return __gost_error_new(msg)
}

private fn err_msg(err: error) -> string {
    return __gost_error_message(err)
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

private fn index_of(s: string, sub: string) -> i64 {
    let ns: i64 = string_len(s)
    let nm: i64 = string_len(sub)
    if nm == 0 {
        return 0
    }
    if nm > ns {
        return -1 as i64
    }
    let i: i64 = 0
    while i <= (ns - nm) {
        let ok: bool = true
        let j: i64 = 0
        while j < nm {
            if string_get(s, i + j) != string_get(sub, j) {
                ok = false
                break
            }
            j = j + 1
        }
        if ok {
            return i
        }
        i = i + 1
    }
    return -1 as i64
}

fn new(msg: string) -> error {
    return err_new(msg)
}

fn wrap(msg: string, cause: error) -> error {
    let cm = err_msg(cause)
    if string_len(cm) == 0 {
        return err_new(msg)
    }
    return err_new(string_concat(string_concat(msg, ": "), cm))
}

fn message(err: error) -> string {
    return err_msg(err)
}

fn is(err: error, target: error) -> bool {
    let em = err_msg(err)
    let tm = err_msg(target)
    if string_len(tm) == 0 {
        return string_len(em) == 0
    }
    if equal(em, tm) {
        return true
    }
    let wrapped = string_concat(": ", tm)
    if index_of(em, wrapped) >= 0 {
        return true
    }
    return starts_with(em, tm)
}

fn unwrap(err: error) -> error {
    let em = err_msg(err)
    let i: i64 = index_of(em, ": ")
    if i < 0 {
        return err_new("")
    }
    let rest = string_slice(em, i + 2, string_len(em) - (i + 2))
    return err_new(rest)
}
