module rand

copy struct Rand {
    state: u64
}

private fn ensure_nonzero(seed: u64) -> u64 {
    if seed == 0 as u64 {
        return 88172645463393265 as u64
    }
    return seed
}

fn new(seed: u64) -> Rand {
    return Rand { state = ensure_nonzero(seed) }
}

fn seed(r: mutref[Rand], v: u64) {
    r.state = ensure_nonzero(v)
}

fn seed_now() -> Rand {
    return new(__gost_now_ms() as u64)
}

fn next_u64(r: mutref[Rand]) -> u64 {
    let x: u64 = 0 as u64
    x = r.state
    x = x ^ (x << 13)
    x = x ^ (x >> 7)
    x = x ^ (x << 17)
    r.state = x
    return x
}

fn next_i64(r: mutref[Rand]) -> i64 {
    return (next_u64(r) >> 1) as i64
}

fn intn(r: mutref[Rand], n: i64) -> i64 {
    if n <= 0 {
        return 0
    }
    return (next_u64(r) % (n as u64)) as i64
}

fn float64(r: mutref[Rand]) -> f64 {
    let x: u64 = next_u64(r) >> 11
    return (x as f64) / ((9007199254740992 as u64) as f64)
}

let global_state: u64 = 88172645463393265 as u64

fn global_seed(seed: u64) {
    global_state = ensure_nonzero(seed)
}

fn global_seed_now() {
    global_state = ensure_nonzero(__gost_now_ms() as u64)
}

fn global_u64() -> u64 {
    let r = Rand { state = global_state }
    let v = next_u64(&mut r)
    global_state = r.state
    return v
}

fn global_i64() -> i64 {
    return (global_u64() >> 1) as i64
}

fn global_intn(n: i64) -> i64 {
    if n <= 0 {
        return 0
    }
    return (global_u64() % (n as u64)) as i64
}

fn global_float64() -> f64 {
    let x: u64 = global_u64() >> 11
    return (x as f64) / ((9007199254740992 as u64) as f64)
}
