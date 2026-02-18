module sync

copy struct Mutex {
    h: i64
}

copy struct RWMutex {
    mu: Mutex
}

copy struct WaitGroup {
    h: i64
}

copy struct Once {
    h: i64
}

fn new_mutex() -> Mutex {
    return Mutex { h = __gost_sync_mutex_new() }
}

fn lock(m: Mutex) {
    __gost_sync_mutex_lock(m.h)
}

fn unlock(m: Mutex) {
    __gost_sync_mutex_unlock(m.h)
}

fn try_lock(m: Mutex) -> bool {
    return __gost_sync_mutex_try_lock(m.h) != 0
}

fn new_rw_mutex() -> RWMutex {
    return RWMutex { mu = new_mutex() }
}

fn r_lock(rw: RWMutex) {
    lock(rw.mu)
}

fn r_unlock(rw: RWMutex) {
    unlock(rw.mu)
}

fn w_lock(rw: RWMutex) {
    lock(rw.mu)
}

fn w_unlock(rw: RWMutex) {
    unlock(rw.mu)
}

fn new_wait_group() -> WaitGroup {
    return WaitGroup { h = __gost_sync_waitgroup_new() }
}

fn add(wg: WaitGroup, delta: i32) {
    __gost_sync_waitgroup_add(wg.h, delta)
}

fn done(wg: WaitGroup) {
    __gost_sync_waitgroup_add(wg.h, -1)
}

fn wait(wg: WaitGroup) {
    __gost_sync_waitgroup_wait(wg.h)
}

fn new_once() -> Once {
    return Once { h = __gost_sync_once_new() }
}

fn do_once(o: Once, f: fn() -> unit) {
    if __gost_sync_once_begin(o.h) != 0 {
        f()
    }
}
