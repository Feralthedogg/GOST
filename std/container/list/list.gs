module list

struct List {
    values: []string
    alive: []bool
    prev: []i64
    next: []i64
    head: i64
    tail: i64
    size: i64
}

private fn err_new(msg: string) -> error {
    return __gost_error_new(msg)
}

private fn invalid_index() -> i64 {
    return -1 as i64
}

private fn node_count(l: ref[List]) -> i64 {
    return slice_len[string](&l.values)
}

private fn node_count_mut(l: mutref[List]) -> i64 {
    return slice_len[string](&l.values)
}

private fn in_bounds(l: ref[List], idx: i64) -> bool {
    if idx < 0 {
        return false
    }
    return idx < node_count(l)
}

private fn in_bounds_mut(l: mutref[List], idx: i64) -> bool {
    if idx < 0 {
        return false
    }
    return idx < node_count_mut(l)
}

fn new() -> List {
    return List {
        values = make_slice[string](0, 0),
        alive = make_slice[bool](0, 0),
        prev = make_slice[i64](0, 0),
        next = make_slice[i64](0, 0),
        head = invalid_index(),
        tail = invalid_index(),
        size = 0,
    }
}

fn len(l: ref[List]) -> i64 {
    return l.size
}

fn is_empty(l: ref[List]) -> bool {
    return l.size == 0
}

fn push_back(l: mutref[List], value: string) -> i64 {
    let idx: i64 = 0
    idx = node_count_mut(l)
    let old_tail: i64 = 0
    old_tail = l.tail

    slice_push[string](&mut l.values, value)
    slice_push[bool](&mut l.alive, true)
    slice_push[i64](&mut l.prev, old_tail)
    slice_push[i64](&mut l.next, invalid_index())

    if old_tail >= 0 {
        slice_set[i64](&mut l.next, old_tail, idx)
    } else {
        l.head = idx
    }
    l.tail = idx
    l.size = l.size + 1
    return idx
}

fn push_front(l: mutref[List], value: string) -> i64 {
    let idx: i64 = 0
    idx = node_count_mut(l)
    let old_head: i64 = 0
    old_head = l.head

    slice_push[string](&mut l.values, value)
    slice_push[bool](&mut l.alive, true)
    slice_push[i64](&mut l.prev, invalid_index())
    slice_push[i64](&mut l.next, old_head)

    if old_head >= 0 {
        slice_set[i64](&mut l.prev, old_head, idx)
    } else {
        l.tail = idx
    }
    l.head = idx
    l.size = l.size + 1
    return idx
}

fn remove(l: mutref[List], idx: i64) -> bool {
    if !in_bounds_mut(l, idx) {
        return false
    }
    let alive_now = false
    alive_now = slice_get_copy[bool](&l.alive, idx)
    if !alive_now {
        return false
    }

    let p: i64 = 0
    p = slice_get_copy[i64](&l.prev, idx)
    let n: i64 = 0
    n = slice_get_copy[i64](&l.next, idx)

    if p >= 0 {
        slice_set[i64](&mut l.next, p, n)
    } else {
        l.head = n
    }
    if n >= 0 {
        slice_set[i64](&mut l.prev, n, p)
    } else {
        l.tail = p
    }

    slice_set[bool](&mut l.alive, idx, false)
    slice_set[i64](&mut l.prev, idx, invalid_index())
    slice_set[i64](&mut l.next, idx, invalid_index())
    l.size = l.size - 1
    return true
}

fn front_index(l: ref[List]) -> i64 {
    return l.head
}

fn back_index(l: ref[List]) -> i64 {
    return l.tail
}

fn next_index(l: ref[List], idx: i64) -> i64 {
    if idx < 0 {
        return l.head
    }
    if !in_bounds(l, idx) {
        return invalid_index()
    }
    let alive_now = false
    alive_now = slice_get_copy[bool](&l.alive, idx)
    if !alive_now {
        return invalid_index()
    }
    return slice_get_copy[i64](&l.next, idx)
}

fn prev_index(l: ref[List], idx: i64) -> i64 {
    if idx < 0 {
        return l.tail
    }
    if !in_bounds(l, idx) {
        return invalid_index()
    }
    let alive_now = false
    alive_now = slice_get_copy[bool](&l.alive, idx)
    if !alive_now {
        return invalid_index()
    }
    return slice_get_copy[i64](&l.prev, idx)
}

fn value(l: ref[List], idx: i64) -> Result[string, error] {
    if !in_bounds(l, idx) {
        return Result.Err[string, error](err_new("list index out of bounds"))
    }
    let alive_now = false
    alive_now = slice_get_copy[bool](&l.alive, idx)
    if !alive_now {
        return Result.Err[string, error](err_new("list node removed"))
    }
    return Result.Ok[string, error](slice_get_copy[string](&l.values, idx))
}

fn clear(l: mutref[List]) {
    l.values = make_slice[string](0, 0)
    l.alive = make_slice[bool](0, 0)
    l.prev = make_slice[i64](0, 0)
    l.next = make_slice[i64](0, 0)
    l.head = invalid_index()
    l.tail = invalid_index()
    l.size = 0
}
