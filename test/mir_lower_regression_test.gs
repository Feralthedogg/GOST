module main

fn is_even_ref(v: ref[i32]) -> bool {
    return (*v % 2) == 0
}

fn load_ref(v: ref[i32]) -> i32 {
    return *v
}

fn pair_one() -> (i32, error) {
    return (1, nil)
}

fn idv(self: i32) -> i32 {
    return self
}

fn iter_index_regression() -> i32 {
    let xs = make_slice[i32](0 as i64, 0 as i64)
    slice_push[i32](&mut xs, 10)
    slice_push[i32](&mut xs, 11)
    slice_push[i32](&mut xs, 12)
    slice_push[i32](&mut xs, 13)

    let idx_sum: i64 = 0
    let val_sum: i32 = 0
    for i, v in map(filter(iter(&xs), is_even_ref), load_ref) {
        idx_sum += i
        val_sum += v
    }
    // Index tracks original iterator position (0 and 2 for even entries).
    if idx_sum != 2 as i64 {
        return 1
    }
    if val_sum != 22 {
        return 2
    }
    return 0
}

fn match_wildcard_order_regression() -> (i32, error) {
    let v = match 1 {
        _ => pair_one()?,
        1 => 100,
    }
    return (v, nil)
}

fn receiver_autoderef_regression() -> i32 {
    let x = 9
    let r = &x
    return r.idv()
}

fn main() -> i32 {
    let iter_rc = iter_index_regression()
    if iter_rc != 0 {
        return 10 + iter_rc
    }
    let mr = match_wildcard_order_regression()
    let mv = match mr {
        Result.Ok(x) => x,
        Result.Err(_) => -1,
        _ => -2,
    }
    if mv != 1 {
        return 20
    }
    if receiver_autoderef_regression() != 9 {
        return 40
    }
    return 0
}
