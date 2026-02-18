module time

copy struct Instant {
    ms: i64
}

copy struct Duration {
    ns: i64
}

private fn ns_per_usec() -> i64 {
    return (1000 as i64)
}

private fn ns_per_msec() -> i64 {
    return (1000 as i64) * ns_per_usec()
}

private fn ns_per_sec() -> i64 {
    return (1000 as i64) * ns_per_msec()
}

private fn ns_per_min() -> i64 {
    return (60 as i64) * ns_per_sec()
}

private fn ns_per_hour() -> i64 {
    return (60 as i64) * ns_per_min()
}

private fn time_error_new(msg: string) -> error {
    return __gost_error_new(msg)
}

private fn str_eq(a: string, b: string) -> bool {
    let na: i64 = string_len(a)
    let nb: i64 = string_len(b)
    if na != nb {
        return false
    }
    let i: i64 = 0
    while i < na {
        if string_get(a, i) != string_get(b, i) {
            return false
        }
        i = i + 1
    }
    return true
}

private fn i64_to_string(v: i64) -> string {
    if v == 0 {
        return "0"
    }
    let neg: bool = false
    let n: i64 = v
    if n < 0 {
        neg = true
        n = 0 - n
    }
    let out: string = ""
    while n > 0 {
        let d: i32 = (n % 10) as i32
        out = string_concat(string_from_byte(string_get("0", 0) + d), out)
        n = n / 10
    }
    if neg {
        return string_concat("-", out)
    }
    return out
}

private fn is_space(b: i32) -> bool {
    if b == string_get(" ", 0) { return true }
    if b == string_get("\t", 0) { return true }
    if b == string_get("\n", 0) { return true }
    if b == string_get("\r", 0) { return true }
    return false
}

private fn trim_space(s: string) -> string {
    let n: i64 = string_len(s)
    if n == 0 {
        return s
    }
    let left: i64 = 0
    while left < n && is_space(string_get(s, left)) {
        left = left + 1
    }
    if left >= n {
        return ""
    }
    let right: i64 = n - 1
    while right > left && is_space(string_get(s, right)) {
        right = right - 1
    }
    return string_slice(s, left, right - left + 1)
}

fn now_ms() -> i64 {
    return __gost_now_ms()
}

fn now() -> Instant {
    return Instant { ms = __gost_now_ms() }
}

fn unix() -> i64 {
    return __gost_now_ms() / 1000
}

fn from_unix(sec: i64) -> Instant {
    return Instant { ms = sec * 1000 }
}

fn nanoseconds(n: i64) -> Duration {
    return Duration { ns = n }
}

fn microseconds(n: i64) -> Duration {
    return Duration { ns = n * ns_per_usec() }
}

fn milliseconds(n: i64) -> Duration {
    return Duration { ns = n * ns_per_msec() }
}

fn seconds(n: i64) -> Duration {
    return Duration { ns = n * ns_per_sec() }
}

fn minutes(n: i64) -> Duration {
    return Duration { ns = n * ns_per_min() }
}

fn hours(n: i64) -> Duration {
    return Duration { ns = n * ns_per_hour() }
}

fn add(a: Duration, b: Duration) -> Duration {
    return Duration { ns = a.ns + b.ns }
}

fn sub(a: Duration, b: Duration) -> Duration {
    return Duration { ns = a.ns - b.ns }
}

fn duration_ms(d: Duration) -> i64 {
    if d.ns <= 0 {
        return 0
    }
    return d.ns / (1000000 as i64)
}

fn since(start: Instant) -> Duration {
    let elapsed_ms: i64 = __gost_now_ms() - start.ms
    return milliseconds(elapsed_ms)
}

fn since_ms(start: Instant) -> i64 {
    return __gost_now_ms() - start.ms
}

fn until(when: Instant) -> Duration {
    let rem_ms: i64 = when.ms - __gost_now_ms()
    return milliseconds(rem_ms)
}

fn sleep_ms(ms: i32) {
    let ch = after(ms)
    recv(ch)
}

fn sleep(d: Duration) {
    let ms64: i64 = duration_ms(d)
    let ms32: i32 = ms64 as i32
    sleep_ms(ms32)
}

fn after_ms(ms: i32) -> chan[unit] {
    return after(ms)
}

fn after_duration(d: Duration) -> chan[unit] {
    let ms64: i64 = duration_ms(d)
    let ms32: i32 = ms64 as i32
    return after(ms32)
}

fn format_instant_ms(t: Instant) -> string {
    return i64_to_string(t.ms)
}

fn format_duration(d: Duration) -> string {
    if d.ns == 0 {
        return "0s"
    }

    let neg: bool = false
    let ns: i64 = d.ns
    if ns < 0 {
        neg = true
        ns = 0 - ns
    }

    let h: i64 = ns / ns_per_hour()
    let rem_h: i64 = ns % ns_per_hour()
    let m: i64 = rem_h / ns_per_min()
    let rem_m: i64 = rem_h % ns_per_min()
    let s: i64 = rem_m / ns_per_sec()
    let rem_s: i64 = rem_m % ns_per_sec()
    let ms: i64 = rem_s / (1000000 as i64)
    let rem_ms: i64 = rem_s % (1000000 as i64)
    let us: i64 = rem_ms / (1000 as i64)
    let nss: i64 = rem_ms % (1000 as i64)

    let out: string = ""
    if h > 0 {
        out = string_concat(out, i64_to_string(h))
        out = string_concat(out, "h")
    }
    if m > 0 {
        out = string_concat(out, i64_to_string(m))
        out = string_concat(out, "m")
    }
    if s > 0 {
        out = string_concat(out, i64_to_string(s))
        out = string_concat(out, "s")
    }
    if ms > 0 {
        out = string_concat(out, i64_to_string(ms))
        out = string_concat(out, "ms")
    }
    if us > 0 {
        out = string_concat(out, i64_to_string(us))
        out = string_concat(out, "us")
    }
    if nss > 0 {
        out = string_concat(out, i64_to_string(nss))
        out = string_concat(out, "ns")
    }

    if neg {
        return string_concat("-", out)
    }
    return out
}

private fn is_digit(b: i32) -> bool {
    return b >= string_get("0", 0) && b <= string_get("9", 0)
}

private fn parse_i64_ascii(s: string) -> Result[i64, error] {
    let n: i64 = string_len(s)
    if n == 0 {
        return Result.Err[i64, error](time_error_new("invalid duration number"))
    }
    let out: i64 = 0
    let i: i64 = 0
    while i < n {
        let b: i32 = string_get(s, i)
        if !is_digit(b) {
            return Result.Err[i64, error](time_error_new("invalid duration number"))
        }
        out = out * 10 + ((b - string_get("0", 0)) as i64)
        i = i + 1
    }
    return Result.Ok[i64, error](out)
}

private fn unit_ns(u: string) -> i64 {
    if str_eq(u, "ns") { return 1 }
    if str_eq(u, "us") { return ns_per_usec() }
    if str_eq(u, "ms") { return ns_per_msec() }
    if str_eq(u, "s") { return ns_per_sec() }
    if str_eq(u, "m") { return ns_per_min() }
    if str_eq(u, "h") { return ns_per_hour() }
    return 0
}

fn parse_duration(s: string) -> Result[Duration, error] {
    let raw = trim_space(s)
    let n: i64 = string_len(raw)
    if n == 0 {
        return Result.Err[Duration, error](time_error_new("empty duration"))
    }

    let total: i64 = 0
    let i: i64 = 0
    while i < n {
        let num_start: i64 = i
        while i < n && is_digit(string_get(raw, i)) {
            i = i + 1
        }
        if i == num_start {
            return Result.Err[Duration, error](time_error_new("invalid duration: expected number"))
        }
        let num_text = string_slice(raw, num_start, i - num_start)
        let amount: i64 = 0
        match parse_i64_ascii(num_text) {
            Result.Ok(v) => { amount = v },
            Result.Err(e) => { return Result.Err[Duration, error](e) },
            _ => { return Result.Err[Duration, error](time_error_new("invalid duration number")) },
        }

        if i >= n {
            return Result.Err[Duration, error](time_error_new("invalid duration: missing unit"))
        }

        let u: string = ""
        if i + 1 < n {
            let two = string_slice(raw, i, 2)
            if str_eq(two, "ns") || str_eq(two, "us") || str_eq(two, "ms") {
                u = two
                i = i + 2
            }
        }
        if string_len(u) == 0 {
            u = string_slice(raw, i, 1)
            i = i + 1
        }

        let mul = unit_ns(u)
        if mul == 0 {
            return Result.Err[Duration, error](time_error_new("invalid duration unit"))
        }
        total = total + amount * mul
    }

    return Result.Ok[Duration, error](Duration { ns = total })
}
