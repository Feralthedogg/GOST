module testing

import "std/strings"

struct T {
    name: string
    failed: bool
    logs: []string
}

fn new(name: string) -> T {
    return T {
        name = name,
        failed = false,
        logs = make_slice[string](0, 0),
    }
}

fn log(t: mutref[T], msg: string) {
    slice_push[string](&mut t.logs, msg)
}

fn fail(t: mutref[T]) {
    t.failed = true
}

fn error(t: mutref[T], msg: string) {
    t.failed = true
    slice_push[string](&mut t.logs, msg)
}

fn fatal(t: mutref[T], msg: string) {
    error(t, msg)
    panic(msg)
}

fn failed(t: T) -> bool {
    return t.failed
}

fn logs(t: T) -> []string {
    return t.logs
}

fn assert_true(t: mutref[T], cond: bool, msg: string) {
    if !cond {
        error(t, msg)
    }
}

fn assert_false(t: mutref[T], cond: bool, msg: string) {
    if cond {
        error(t, msg)
    }
}

fn assert_eq_i64(t: mutref[T], got: i64, want: i64, msg: string) {
    if got != want {
        error(t, msg)
    }
}

fn assert_eq_i32(t: mutref[T], got: i32, want: i32, msg: string) {
    if got != want {
        error(t, msg)
    }
}

fn assert_eq_string(t: mutref[T], got: string, want: string, msg: string) {
    if !equal(got, want) {
        error(t, msg)
    }
}

fn run(name: string, test_fn: fn(mutref[T]) -> unit) -> i32 {
    let t = new(name)
    test_fn(&mut t)
    if t.failed {
        let i: i64 = 0
        while i < slice_len[string](&t.logs) {
            __gost_println(string_concat(string_concat("FAIL ", name), string_concat(": ", slice_get_copy[string](&t.logs, i))))
            i = i + 1
        }
        return 1
    }
    __gost_println(string_concat("ok ", name))
    return 0
}
