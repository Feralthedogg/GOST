module sort

fn sort_by[T](xs: mutref[[]T], less: fn(T, T) -> bool) {
    let n: i64 = slice_len[T](xs)
    if n <= 1 {
        return
    }
    let i: i64 = 1
    while i < n {
        let j: i64 = i
        while j > 0 {
            let a: T = slice_get_copy[T](xs, j - 1)
            let b: T = slice_get_copy[T](xs, j)
            if !less(b, a) {
                break
            }
            slice_set[T](xs, j - 1, b)
            slice_set[T](xs, j, a)
            j = j - 1
        }
        i = i + 1
    }
}

fn is_sorted_by[T](xs: ref[[]T], less: fn(T, T) -> bool) -> bool {
    let n: i64 = slice_len[T](xs)
    if n <= 1 {
        return true
    }
    let i: i64 = 1
    while i < n {
        let prev: T = slice_get_copy[T](xs, i - 1)
        let cur: T = slice_get_copy[T](xs, i)
        if less(cur, prev) {
            return false
        }
        i = i + 1
    }
    return true
}

fn search_by[T](xs: ref[[]T], pred: fn(T) -> bool) -> i64 {
    let lo: i64 = 0
    let hi: i64 = slice_len[T](xs)
    while lo < hi {
        let mid: i64 = lo + ((hi - lo) / 2)
        let v: T = slice_get_copy[T](xs, mid)
        if pred(v) {
            hi = mid
        } else {
            lo = mid + 1
        }
    }
    return lo
}

private fn less_i32(a: i32, b: i32) -> bool {
    return a < b
}

private fn less_i64(a: i64, b: i64) -> bool {
    return a < b
}

fn sort_i32(xs: mutref[[]i32]) {
    let n: i64 = slice_len[i32](xs)
    if n <= 1 {
        return
    }
    let i: i64 = 1
    while i < n {
        let j: i64 = i
        while j > 0 {
            let a: i32 = slice_get_copy[i32](xs, j - 1)
            let b: i32 = slice_get_copy[i32](xs, j)
            if b >= a {
                break
            }
            slice_set[i32](xs, j - 1, b)
            slice_set[i32](xs, j, a)
            j = j - 1
        }
        i = i + 1
    }
}

fn is_sorted_i32(xs: ref[[]i32]) -> bool {
    let n: i64 = slice_len[i32](xs)
    if n <= 1 {
        return true
    }
    let i: i64 = 1
    while i < n {
        let prev: i32 = slice_get_copy[i32](xs, i - 1)
        let cur: i32 = slice_get_copy[i32](xs, i)
        if cur < prev {
            return false
        }
        i = i + 1
    }
    return true
}

fn sort_i64(xs: mutref[[]i64]) {
    let n: i64 = slice_len[i64](xs)
    if n <= 1 {
        return
    }
    let i: i64 = 1
    while i < n {
        let j: i64 = i
        while j > 0 {
            let a: i64 = slice_get_copy[i64](xs, j - 1)
            let b: i64 = slice_get_copy[i64](xs, j)
            if b >= a {
                break
            }
            slice_set[i64](xs, j - 1, b)
            slice_set[i64](xs, j, a)
            j = j - 1
        }
        i = i + 1
    }
}

fn is_sorted_i64(xs: ref[[]i64]) -> bool {
    let n: i64 = slice_len[i64](xs)
    if n <= 1 {
        return true
    }
    let i: i64 = 1
    while i < n {
        let prev: i64 = slice_get_copy[i64](xs, i - 1)
        let cur: i64 = slice_get_copy[i64](xs, i)
        if cur < prev {
            return false
        }
        i = i + 1
    }
    return true
}

fn search_i32(xs: ref[[]i32], target: i32) -> i64 {
    let lo: i64 = 0
    let hi: i64 = slice_len[i32](xs)
    while lo < hi {
        let mid: i64 = lo + ((hi - lo) / 2)
        let v: i32 = slice_get_copy[i32](xs, mid)
        if v < target {
            lo = mid + 1
        } else {
            hi = mid
        }
    }
    if lo < slice_len[i32](xs) && slice_get_copy[i32](xs, lo) == target {
        return lo
    }
    return -1 as i64
}

fn search_i64(xs: ref[[]i64], target: i64) -> i64 {
    let lo: i64 = 0
    let hi: i64 = slice_len[i64](xs)
    while lo < hi {
        let mid: i64 = lo + ((hi - lo) / 2)
        let v: i64 = slice_get_copy[i64](xs, mid)
        if v < target {
            lo = mid + 1
        } else {
            hi = mid
        }
    }
    if lo < slice_len[i64](xs) && slice_get_copy[i64](xs, lo) == target {
        return lo
    }
    return -1 as i64
}
