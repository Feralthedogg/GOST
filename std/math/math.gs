module math

fn min_i64(a: i64, b: i64) -> i64 {
    if a < b {
        return a
    }
    return b
}

fn max_i64(a: i64, b: i64) -> i64 {
    if a > b {
        return a
    }
    return b
}

fn abs_i64(v: i64) -> i64 {
    if v < 0 {
        return -v
    }
    return v
}

fn min_f64(a: f64, b: f64) -> f64 {
    if a < b {
        return a
    }
    return b
}

fn max_f64(a: f64, b: f64) -> f64 {
    if a > b {
        return a
    }
    return b
}

fn abs_f64(v: f64) -> f64 {
    if v < 0.0 {
        return -v
    }
    return v
}

fn floor(v: f64) -> f64 {
    let n: i64 = v as i64
    let f: f64 = n as f64
    if f > v {
        return (n - 1) as f64
    }
    return f
}

fn ceil(v: f64) -> f64 {
    let n: i64 = v as i64
    let f: f64 = n as f64
    if f < v {
        return (n + 1) as f64
    }
    return f
}

fn pow(base: f64, exp: i32) -> f64 {
    if exp == 0 {
        return 1.0
    }
    let e: i32 = exp
    let b: f64 = base
    let out: f64 = 1.0
    let neg: bool = e < 0
    if neg {
        e = -e
    }
    let i: i32 = 0
    while i < e {
        out = out * b
        i = i + 1
    }
    if neg {
        if out == 0.0 {
            return 0.0
        }
        return 1.0 / out
    }
    return out
}

fn log2(v: f64) -> f64 {
    if v <= 0.0 {
        return 0.0
    }
    let x: f64 = v
    let k: i32 = 0
    while x >= 2.0 {
        x = x / 2.0
        k = k + 1
    }
    while x < 1.0 {
        x = x * 2.0
        k = k - 1
    }
    let frac: f64 = 0.0
    let bit: f64 = 0.5
    let i: i32 = 0
    while i < 24 {
        x = x * x
        if x >= 2.0 {
            x = x / 2.0
            frac = frac + bit
        }
        bit = bit / 2.0
        i = i + 1
    }
    return (k as f64) + frac
}

