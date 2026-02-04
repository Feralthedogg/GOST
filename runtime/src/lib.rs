#![allow(private_interfaces)]
#![allow(non_camel_case_types, non_snake_case, dead_code, unsafe_op_in_unsafe_fn, static_mut_refs)]
use std::cell::Cell;
#[cfg(feature = "stats")]
use std::fmt::Write;
use std::ffi::c_void;
use std::mem::{self, MaybeUninit};
use std::ptr;
use std::sync::atomic::{AtomicI32, AtomicI64, Ordering};
use std::sync::Once;

#[cfg(not(windows))]
use libc::{
    pthread_cond_broadcast, pthread_cond_init, pthread_cond_signal, pthread_cond_t,
    pthread_cond_timedwait, pthread_cond_wait, pthread_create, pthread_detach, pthread_join,
    pthread_mutex_destroy, pthread_mutex_init, pthread_mutex_lock, pthread_mutex_t,
    pthread_mutex_unlock, pthread_t, timespec,
};

#[cfg(not(windows))]
use libc::CLOCK_MONOTONIC;
#[cfg(not(windows))]
use libc::{
    mmap, mprotect, munmap, sysconf, MAP_ANON, MAP_FAILED, MAP_PRIVATE, PROT_NONE, PROT_READ,
    PROT_WRITE, _SC_PAGESIZE,
};

use libc::write;

#[cfg(windows)]
#[repr(C)]
struct FILETIME {
    dwLowDateTime: u32,
    dwHighDateTime: u32,
}

#[cfg(windows)]
#[repr(C)]
struct SYSTEM_INFO {
    wProcessorArchitecture: u16,
    wReserved: u16,
    dwPageSize: u32,
    lpMinimumApplicationAddress: *mut c_void,
    lpMaximumApplicationAddress: *mut c_void,
    dwActiveProcessorMask: usize,
    dwNumberOfProcessors: u32,
    dwProcessorType: u32,
    dwAllocationGranularity: u32,
    wProcessorLevel: u16,
    wProcessorRevision: u16,
}

#[cfg(windows)]
#[repr(C)]
struct CRITICAL_SECTION {
    debug_info: *mut c_void,
    lock_count: i32,
    recursion_count: i32,
    owning_thread: *mut c_void,
    lock_semaphore: *mut c_void,
    spin_count: usize,
}

#[cfg(windows)]
#[repr(C)]
struct CONDITION_VARIABLE {
    ptr: *mut c_void,
}

#[cfg(windows)]
type OsMutex = CRITICAL_SECTION;
#[cfg(windows)]
type OsCond = CONDITION_VARIABLE;
#[cfg(windows)]
type OsThread = *mut c_void;

#[cfg(not(windows))]
type OsMutex = pthread_mutex_t;
#[cfg(not(windows))]
type OsCond = pthread_cond_t;
#[cfg(not(windows))]
type OsThread = pthread_t;

#[cfg(windows)]
unsafe extern "system" {
    fn GetTickCount64() -> u64;
    fn GetSystemTimeAsFileTime(lpSystemTimeAsFileTime: *mut FILETIME);
    fn GetSystemInfo(lpSystemInfo: *mut SYSTEM_INFO);
    fn SwitchToThread() -> i32;
    fn InitializeCriticalSection(cs: *mut CRITICAL_SECTION);
    fn DeleteCriticalSection(cs: *mut CRITICAL_SECTION);
    fn EnterCriticalSection(cs: *mut CRITICAL_SECTION);
    fn LeaveCriticalSection(cs: *mut CRITICAL_SECTION);
    fn InitializeConditionVariable(cv: *mut CONDITION_VARIABLE);
    fn SleepConditionVariableCS(cv: *mut CONDITION_VARIABLE, cs: *mut CRITICAL_SECTION, ms: u32) -> i32;
    fn WakeConditionVariable(cv: *mut CONDITION_VARIABLE);
    fn WakeAllConditionVariable(cv: *mut CONDITION_VARIABLE);
    fn CreateThread(
        lpThreadAttributes: *mut c_void,
        dwStackSize: usize,
        lpStartAddress: unsafe extern "system" fn(*mut c_void) -> u32,
        lpParameter: *mut c_void,
        dwCreationFlags: u32,
        lpThreadId: *mut u32,
    ) -> *mut c_void;
    fn WaitForSingleObject(handle: *mut c_void, ms: u32) -> u32;
    fn CloseHandle(handle: *mut c_void) -> i32;
    fn VirtualAlloc(
        lpAddress: *mut c_void,
        dwSize: usize,
        flAllocationType: u32,
        flProtect: u32,
    ) -> *mut c_void;
    fn VirtualFree(lpAddress: *mut c_void, dwSize: usize, dwFreeType: u32) -> i32;
    fn VirtualProtect(
        lpAddress: *mut c_void,
        dwSize: usize,
        flNewProtect: u32,
        lpflOldProtect: *mut u32,
    ) -> i32;
}

#[cfg(windows)]
const MEM_RESERVE: u32 = 0x2000;
#[cfg(windows)]
const MEM_COMMIT: u32 = 0x1000;
#[cfg(windows)]
const MEM_RELEASE: u32 = 0x8000;
#[cfg(windows)]
const PAGE_READWRITE: u32 = 0x04;
#[cfg(windows)]
const PAGE_NOACCESS: u32 = 0x01;
#[cfg(windows)]
const INFINITE: u32 = 0xFFFF_FFFF;

type gost_thread_fn = extern "C" fn(*mut c_void);

type gost_main_fn = extern "C" fn() -> i32;

#[cfg(target_arch = "aarch64")]
#[repr(C)]
struct gost_ctx {
    sp: u64,
    pc: u64,
    x19: u64,
    x20: u64,
    x21: u64,
    x22: u64,
    x23: u64,
    x24: u64,
    x25: u64,
    x26: u64,
    x27: u64,
    x28: u64,
    fp: u64,
    lr: u64,
}

#[cfg(not(target_arch = "aarch64"))]
#[repr(C)]
struct gost_ctx {
    rsp: u64,
    rip: u64,
    rbx: u64,
    rbp: u64,
    #[cfg(windows)]
    rdi: u64,
    #[cfg(windows)]
    rsi: u64,
    r12: u64,
    r13: u64,
    r14: u64,
    r15: u64,
    #[cfg(windows)]
    xmm6: [u8; 16],
    #[cfg(windows)]
    xmm7: [u8; 16],
    #[cfg(windows)]
    xmm8: [u8; 16],
    #[cfg(windows)]
    xmm9: [u8; 16],
    #[cfg(windows)]
    xmm10: [u8; 16],
    #[cfg(windows)]
    xmm11: [u8; 16],
    #[cfg(windows)]
    xmm12: [u8; 16],
    #[cfg(windows)]
    xmm13: [u8; 16],
    #[cfg(windows)]
    xmm14: [u8; 16],
    #[cfg(windows)]
    xmm15: [u8; 16],
}

unsafe extern "C" {
    fn gost_ctx_swap(from: *mut gost_ctx, to: *mut gost_ctx);
    fn gost_ctx_start();
}

const G_IDLE: i32 = 0;
const G_RUNNABLE: i32 = 1;
const G_RUNNING: i32 = 2;
const G_WAITING: i32 = 3;
const G_PARKING: i32 = 4;
const G_DEAD: i32 = 5;

#[repr(C)]
struct gost_sudog {
    g: *mut gost_g,
    elem: *mut c_void,
    success: i32,
    next: *mut gost_sudog,
}

#[repr(C)]
struct gost_g {
    ctx: gost_ctx,
    stack_base: *mut c_void,
    stack_reserve: usize,
    stack_commit: usize,
    entry: gost_thread_fn,
    entry_ctx: *mut c_void,
    state: i32,
    park_ready: i32,
    is_main: i32,
    exit_code: i32,
    next: *mut gost_g,
    on_runq: i32,
}

#[repr(C)]
struct gost_p {
    id: i32,
    runq_head: *mut gost_g,
    runq_tail: *mut gost_g,
    runq_len: i32,
}

#[repr(C)]
struct gost_m {
    sched_ctx: gost_ctx,
    curg: *mut gost_g,
    id: i32,
    p: *mut gost_p,
    stats: gost_stats_local,
}

#[repr(C)]
struct gost_sched {
    mu: OsMutex,
    cv: OsCond,
    runq_head: *mut gost_g,
    runq_tail: *mut gost_g,
    gomaxprocs: i32,
    mcount: i32,
    shutting_down: i32,
    main_done: i32,
    main_exit: i32,
    rr_p: i32,
    timers_heap: *mut gost_timer_heap,
    ps: *mut gost_p,
    ms: *mut gost_m,
    mthreads: *mut OsThread,
}

#[repr(C)]
struct gost_timer {
    when_ms: i64,
    ch: *mut __gost_chan,
    next: *mut gost_timer,
}

struct gost_timer_heap {
    v: Vec<*mut gost_timer>,
}

impl gost_timer_heap {
    fn new() -> Self {
        Self { v: Vec::new() }
    }
}

#[repr(C)]
struct __gost_chan {
    mu: OsMutex,
    elem_size: usize,
    cap: i32,
    buf: *mut u8,
    len: i32,
    head: i32,
    tail: i32,
    closed: i32,
    refcount: AtomicI64,
    sendq_head: *mut gost_sudog,
    sendq_tail: *mut gost_sudog,
    recvq_head: *mut gost_sudog,
    recvq_tail: *mut gost_sudog,
    selq_head: *mut gost_selnode,
    selq_tail: *mut gost_selnode,
}

#[repr(C)]
struct gost_selwaiter {
    g: *mut gost_g,
    fired: AtomicI32,
    nodes: *mut gost_selnode,
}

#[repr(C)]
struct gost_selnode {
    w: *mut gost_selwaiter,
    ch: *mut __gost_chan,
    next_w: *mut gost_selnode,
    prev_ch: *mut gost_selnode,
    next_ch: *mut gost_selnode,
    detached: i32,
}

#[repr(C)]
struct __gost_slice {
    elem_size: usize,
    len: i64,
    cap: i64,
    elem_drop: Option<extern "C" fn(*mut c_void)>,
    data: *mut u8,
}

#[repr(C)]
struct __gost_shared {
    refcount: AtomicI64,
    drop_payload: Option<extern "C" fn(*mut c_void)>,
    payload: [u8; 0],
}

#[repr(C)]
#[derive(Copy, Clone)]
struct __gost_map_entry {
    used: i32,
    key: __gost_map_key,
    val: *mut u8,
}

#[repr(C)]
#[derive(Copy, Clone)]
union __gost_map_key {
    i64v: i64,
    u64v: u64,
    strv: __gost_map_key_str,
}

#[repr(C)]
#[derive(Copy, Clone)]
struct __gost_map_key_str {
    ptr: *const u8,
    len: i64,
}

#[repr(C)]
struct __gost_map {
    key_kind: i32,
    val_size: usize,
    len: i64,
    cap: i64,
    entries: *mut __gost_map_entry,
}

static mut G_SCHED: MaybeUninit<gost_sched> = MaybeUninit::uninit();
static G_SCHED_ONCE: Once = Once::new();
static G_STACK_ONCE: Once = Once::new();

static mut G_STACK_COMMIT: usize = 64 * 1024;
static mut G_STACK_RESERVE: usize = 256 * 1024;

thread_local! {
    static TLS_M: Cell<*mut gost_m> = Cell::new(ptr::null_mut());
    static TLS_G: Cell<*mut gost_g> = Cell::new(ptr::null_mut());
}

fn tls_m_get() -> *mut gost_m { TLS_M.with(|c| c.get()) }
fn tls_m_set(m: *mut gost_m) { TLS_M.with(|c| c.set(m)); }
fn tls_g_get() -> *mut gost_g { TLS_G.with(|c| c.get()) }
fn tls_g_set(g: *mut gost_g) { TLS_G.with(|c| c.set(g)); }

unsafe fn fd_write_bytes(fd: i32, bytes: &[u8]) {
    if bytes.is_empty() { return; }
    #[cfg(windows)]
    {
        let _ = write(fd, bytes.as_ptr() as *const c_void, bytes.len() as u32);
    }
    #[cfg(not(windows))]
    {
        let _ = write(fd, bytes.as_ptr() as *const c_void, bytes.len());
    }
}

#[cfg(windows)]
unsafe fn os_mutex_init(m: *mut OsMutex) { InitializeCriticalSection(m); }
#[cfg(windows)]
unsafe fn os_mutex_destroy(m: *mut OsMutex) { DeleteCriticalSection(m); }
#[cfg(windows)]
unsafe fn os_mutex_lock(m: *mut OsMutex) { EnterCriticalSection(m); }
#[cfg(windows)]
unsafe fn os_mutex_unlock(m: *mut OsMutex) { LeaveCriticalSection(m); }

#[cfg(windows)]
unsafe fn os_cond_init(c: *mut OsCond) { InitializeConditionVariable(c); }
#[cfg(windows)]
unsafe fn os_cond_signal(c: *mut OsCond) { WakeConditionVariable(c); }
#[cfg(windows)]
unsafe fn os_cond_broadcast(c: *mut OsCond) { WakeAllConditionVariable(c); }
#[cfg(windows)]
unsafe fn os_cond_wait(c: *mut OsCond, m: *mut OsMutex) {
    let _ = SleepConditionVariableCS(c, m, INFINITE);
}
#[cfg(windows)]
unsafe fn os_cond_timedwait(c: *mut OsCond, m: *mut OsMutex, wait_ms: i64) {
    let mut ms = wait_ms;
    if ms < 0 { ms = 0; }
    if ms > u32::MAX as i64 { ms = u32::MAX as i64; }
    let _ = SleepConditionVariableCS(c, m, ms as u32);
}

#[cfg(not(windows))]
unsafe fn os_mutex_init(m: *mut OsMutex) { pthread_mutex_init(m, ptr::null()); }
#[cfg(not(windows))]
unsafe fn os_mutex_destroy(m: *mut OsMutex) { pthread_mutex_destroy(m); }
#[cfg(not(windows))]
unsafe fn os_mutex_lock(m: *mut OsMutex) { pthread_mutex_lock(m); }
#[cfg(not(windows))]
unsafe fn os_mutex_unlock(m: *mut OsMutex) { pthread_mutex_unlock(m); }

#[cfg(not(windows))]
unsafe fn os_cond_init(c: *mut OsCond) {
    let mut attr: libc::pthread_condattr_t = mem::zeroed();
    if libc::pthread_condattr_init(&mut attr) == 0 {
        let _ = libc::pthread_condattr_setclock(&mut attr, CLOCK_MONOTONIC);
        pthread_cond_init(c, &attr);
        let _ = libc::pthread_condattr_destroy(&mut attr);
    } else {
        pthread_cond_init(c, ptr::null());
    }
}
#[cfg(not(windows))]
unsafe fn os_cond_signal(c: *mut OsCond) { pthread_cond_signal(c); }
#[cfg(not(windows))]
unsafe fn os_cond_broadcast(c: *mut OsCond) { pthread_cond_broadcast(c); }
#[cfg(not(windows))]
unsafe fn os_cond_wait(c: *mut OsCond, m: *mut OsMutex) { pthread_cond_wait(c, m); }
#[cfg(not(windows))]
unsafe fn os_cond_timedwait(c: *mut OsCond, m: *mut OsMutex, wait_ms: i64) {
    let mut ts: timespec = mem::zeroed();
    timespec_now(&mut ts);
    timespec_add_ms(&mut ts, wait_ms);
    pthread_cond_timedwait(c, m, &ts);
}

#[cfg(windows)]
unsafe extern "system" fn win_worker_entry(arg: *mut c_void) -> u32 {
    m_worker_entry(arg);
    0
}

#[cfg(windows)]
unsafe extern "system" fn win_thread_entry(arg: *mut c_void) -> u32 {
    thread_entry(arg);
    0
}

#[cfg(windows)]
unsafe fn os_thread_create_worker(out: *mut OsThread, arg: *mut c_void) -> i32 {
    let h = CreateThread(ptr::null_mut(), 0, win_worker_entry, arg, 0, ptr::null_mut());
    if h.is_null() { return -1; }
    *out = h;
    0
}

#[cfg(windows)]
unsafe fn os_thread_create_detached(arg: *mut c_void) -> i32 {
    let h = CreateThread(ptr::null_mut(), 0, win_thread_entry, arg, 0, ptr::null_mut());
    if h.is_null() { return -1; }
    CloseHandle(h);
    0
}

#[cfg(windows)]
unsafe fn os_thread_join(t: OsThread) {
    if !t.is_null() {
        let _ = WaitForSingleObject(t, INFINITE);
        let _ = CloseHandle(t);
    }
}

#[cfg(not(windows))]
unsafe fn os_thread_create_worker(out: *mut OsThread, arg: *mut c_void) -> i32 {
    pthread_create(out, ptr::null(), Some(m_worker_entry), arg)
}

#[cfg(not(windows))]
unsafe fn os_thread_create_detached(arg: *mut c_void) -> i32 {
    let mut t: OsThread = mem::zeroed();
    if pthread_create(&mut t, ptr::null(), Some(thread_entry), arg) == 0 {
        pthread_detach(t);
        0
    } else {
        -1
    }
}

#[cfg(not(windows))]
unsafe fn os_thread_join(t: OsThread) {
    pthread_join(t, ptr::null_mut());
}

#[derive(Copy, Clone)]
struct StatSlot(usize);

const ST_G_CREATED: StatSlot = StatSlot(0);
const ST_G_FREED: StatSlot = StatSlot(1);
const ST_GOREADY_CALLS: StatSlot = StatSlot(2);
const ST_GOPARK_CALLS: StatSlot = StatSlot(3);
const ST_RUNQ_PUSH: StatSlot = StatSlot(4);
const ST_RUNQ_POP: StatSlot = StatSlot(5);
const ST_RUNQ_DUPE_BLOCKED: StatSlot = StatSlot(6);
const ST_SCHED_SWITCH: StatSlot = StatSlot(7);
const ST_SUDOG_ALLOC: StatSlot = StatSlot(8);
const ST_SUDOG_FREE: StatSlot = StatSlot(9);
const ST_CHAN_SEND_FAST: StatSlot = StatSlot(10);
const ST_CHAN_SEND_BUF: StatSlot = StatSlot(11);
const ST_CHAN_SEND_BLOCK: StatSlot = StatSlot(12);
const ST_CHAN_RECV_FAST: StatSlot = StatSlot(13);
const ST_CHAN_RECV_BUF: StatSlot = StatSlot(14);
const ST_CHAN_RECV_BLOCK: StatSlot = StatSlot(15);
const ST_CHAN_CLOSE: StatSlot = StatSlot(16);
const ST_TIMER_ADD: StatSlot = StatSlot(17);
const ST_TIMER_FIRED: StatSlot = StatSlot(18);
const ST_SELECT_WAITER_ALLOC: StatSlot = StatSlot(19);
const ST_SELECT_WAITER_FREE: StatSlot = StatSlot(20);
const ST_SELECT_NODE_ALLOC: StatSlot = StatSlot(21);
const ST_SELECT_NODE_FREE: StatSlot = StatSlot(22);
const ST_SELECT_NOTIFY_CALLS: StatSlot = StatSlot(23);
const ST_SELECT_WAKE: StatSlot = StatSlot(24);
const ST_STEAL_CALLS: StatSlot = StatSlot(25);
const ST_STEAL_FAIL: StatSlot = StatSlot(26);
const ST_STEAL_TAKE: StatSlot = StatSlot(27);
const ST_TH_PUSH: StatSlot = StatSlot(28);
const ST_TH_POP: StatSlot = StatSlot(29);
const ST_TH_SIFT_UP: StatSlot = StatSlot(30);
const ST_TH_SIFT_DOWN: StatSlot = StatSlot(31);
const ST_TH_MAXLEN: StatSlot = StatSlot(32);

const STAT_COUNT: usize = 33;

#[repr(C)]
#[derive(Copy, Clone)]
struct gost_stats_local {
    data: [i64; STAT_COUNT],
}

impl gost_stats_local {
    const ZERO: gost_stats_local = gost_stats_local { data: [0; STAT_COUNT] };
    #[inline(always)]
    fn inc(&mut self, idx: usize, v: i64) {
        self.data[idx] += v;
    }
    #[inline(always)]
    fn max(&mut self, idx: usize, v: i64) {
        if v > self.data[idx] {
            self.data[idx] = v;
        }
    }
}

static mut BOOT_STATS: gost_stats_local = gost_stats_local::ZERO;

static G_LIVE: AtomicI64 = AtomicI64::new(0);

const TIMER_DUE_BATCH: usize = 4096;
const TIMER_WAKE_BATCH: usize = 4096;

#[cfg(feature = "stats")]
fn stat_inc(slot: &StatSlot) {
    unsafe {
        let m = tls_m_get();
        if !m.is_null() {
            (*m).stats.inc(slot.0, 1);
        } else {
            BOOT_STATS.inc(slot.0, 1);
        }
    }
}
#[cfg(feature = "stats")]
fn stat_add(slot: &StatSlot, v: i64) {
    unsafe {
        let m = tls_m_get();
        if !m.is_null() {
            (*m).stats.inc(slot.0, v);
        } else {
            BOOT_STATS.inc(slot.0, v);
        }
    }
}
#[cfg(feature = "stats")]
fn stat_max(slot: &StatSlot, v: i64) {
    unsafe {
        let m = tls_m_get();
        if !m.is_null() {
            (*m).stats.max(slot.0, v);
        } else {
            BOOT_STATS.max(slot.0, v);
        }
    }
}
#[cfg(not(feature = "stats"))]
fn stat_inc(_slot: &StatSlot) {}
#[cfg(not(feature = "stats"))]
fn stat_add(_slot: &StatSlot, _v: i64) {}
#[cfg(not(feature = "stats"))]
fn stat_max(_slot: &StatSlot, _v: i64) {}

#[cfg(feature = "stats")]
unsafe fn stats_total(slot: &StatSlot) -> i64 {
    let mut total = BOOT_STATS.data[slot.0];
    let sched = g_sched_mut();
    if !sched.ms.is_null() {
        let count = if sched.mcount > 0 { sched.mcount as usize } else { 0 };
        for i in 0..count {
            total += (*sched.ms.add(i)).stats.data[slot.0];
        }
    }
    total
}

#[cfg(feature = "stats")]
unsafe fn stats_max_val(slot: &StatSlot) -> i64 {
    let mut maxv = BOOT_STATS.data[slot.0];
    let sched = g_sched_mut();
    if !sched.ms.is_null() {
        let count = if sched.mcount > 0 { sched.mcount as usize } else { 0 };
        for i in 0..count {
            let v = (*sched.ms.add(i)).stats.data[slot.0];
            if v > maxv {
                maxv = v;
            }
        }
    }
    maxv
}

static G_ERROR_NEXT: AtomicI32 = AtomicI32::new(1);

unsafe fn g_sched_mut() -> &'static mut gost_sched { &mut *G_SCHED.as_mut_ptr() }

static PARK_FUZZ_ONCE: Once = Once::new();
static mut PARK_FUZZ_MODE: i32 = 0;

fn env_i(name: &str, defv: i32) -> i32 {
    if let Ok(val) = std::env::var(name) {
        if let Ok(v) = val.parse::<i32>() {
            if v > 0 { return v.min(256); }
        }
    }
    defv
}

fn env_usize(name: &str, def_kb: usize) -> usize {
    if let Ok(val) = std::env::var(name) {
        if let Ok(v) = val.parse::<usize>() {
            if v > 0 { return v; }
        }
    }
    def_kb
}

#[inline(always)]
unsafe fn os_yield() {
    #[cfg(windows)]
    {
        let _ = SwitchToThread();
    }
    #[cfg(not(windows))]
    {
        libc::sched_yield();
    }
}

#[inline(always)]
unsafe fn park_fuzz_point() {
    PARK_FUZZ_ONCE.call_once(|| unsafe {
        PARK_FUZZ_MODE = env_i("GOST_PARK_FUZZ", 0);
    });
    let mode = PARK_FUZZ_MODE;
    if mode <= 0 { return; }
    for _ in 0..50 {
        std::hint::spin_loop();
    }
    os_yield();
}

unsafe extern "C" fn stack_init_once() {
    let c = env_usize("GOST_FIBER_COMMIT_KB", 64);
    let r = env_usize("GOST_FIBER_RESERVE_KB", 256);
    let commit = c * 1024;
    let mut reserve = r * 1024;
    if reserve < commit { reserve = commit; }
    G_STACK_COMMIT = commit;
    G_STACK_RESERVE = reserve;
}

unsafe fn page_size() -> usize {
    #[cfg(windows)]
    {
        let mut info: SYSTEM_INFO = mem::zeroed();
        GetSystemInfo(&mut info);
        return info.dwPageSize as usize;
    }
    #[cfg(not(windows))]
    {
        let ps = sysconf(_SC_PAGESIZE);
        if ps <= 0 { return 4096; }
        return ps as usize;
    }
}

unsafe fn stack_alloc(reserve: usize, commit: usize, out_commit: &mut usize) -> *mut c_void {
    if reserve == 0 {
        *out_commit = 0;
        return ptr::null_mut();
    }
    let page = page_size();
    let mut reserve = ((reserve + page - 1) / page) * page;
    let mut commit = ((commit + page - 1) / page) * page;
    if reserve < page * 2 { reserve = page * 2; }
    if commit < page { commit = reserve - page; }
    if commit > reserve - page { commit = reserve - page; }
    #[cfg(windows)]
    {
        let base = VirtualAlloc(ptr::null_mut(), reserve, MEM_RESERVE, PAGE_READWRITE);
        if base.is_null() {
            __gost_panic(b"VirtualAlloc reserve failed\0".as_ptr(), 27);
        }
        let guard = page;
        let commit_base = (base as *mut u8).add(reserve - commit) as *mut c_void;
        let ok = VirtualAlloc(commit_base, commit, MEM_COMMIT, PAGE_READWRITE);
        if ok.is_null() {
            VirtualFree(base, 0, MEM_RELEASE);
            __gost_panic(b"VirtualAlloc commit failed\0".as_ptr(), 26);
        }
        let mut old: u32 = 0;
        let _ = VirtualProtect(base, guard, PAGE_NOACCESS, &mut old);
        *out_commit = commit;
        return base;
    }
    #[cfg(not(windows))]
    {
        let base = mmap(ptr::null_mut(), reserve, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
        if base == MAP_FAILED {
            __gost_panic(b"mmap stack failed\0".as_ptr(), 17);
        }
        let _ = mprotect(base, page, PROT_NONE);
        *out_commit = reserve - page;
        return base as *mut c_void;
    }
}

unsafe fn stack_free(base: *mut c_void, _reserve: usize) {
    if base.is_null() { return; }
    #[cfg(windows)]
    {
        VirtualFree(base, 0, MEM_RELEASE);
    }
    #[cfg(not(windows))]
    {
        let _ = munmap(base, _reserve);
    }
}

unsafe fn ctx_init_g(g: *mut gost_g) {
    G_STACK_ONCE.call_once(|| unsafe { stack_init_once() });
    (*g).stack_reserve = G_STACK_RESERVE;
    let mut committed = 0usize;
    (*g).stack_base = stack_alloc((*g).stack_reserve, G_STACK_COMMIT, &mut committed);
    (*g).stack_commit = committed;

    let stack_top = (*g).stack_base.add((*g).stack_reserve) as usize;
    let top = stack_top & !0xFusize;
    #[cfg(target_arch = "aarch64")]
    {
        (*g).ctx.sp = top as u64;
        (*g).ctx.pc = gost_ctx_start as *const () as usize as u64;
        (*g).ctx.x19 = g as usize as u64;
        (*g).ctx.x20 = 0;
        (*g).ctx.x21 = 0;
        (*g).ctx.x22 = 0;
        (*g).ctx.x23 = 0;
        (*g).ctx.x24 = 0;
        (*g).ctx.x25 = 0;
        (*g).ctx.x26 = 0;
        (*g).ctx.x27 = 0;
        (*g).ctx.x28 = 0;
        (*g).ctx.fp = 0;
        (*g).ctx.lr = 0;
    }
    #[cfg(all(not(target_arch = "aarch64"), not(windows)))]
    {
        (*g).ctx.rsp = top as u64;
        (*g).ctx.rip = gost_ctx_start as *const () as usize as u64;
        (*g).ctx.r12 = g as usize as u64;
        (*g).ctx.rbx = 0;
        (*g).ctx.rbp = 0;
        (*g).ctx.r13 = 0;
        (*g).ctx.r14 = 0;
        (*g).ctx.r15 = 0;
    }
    #[cfg(all(not(target_arch = "aarch64"), windows))]
    {
        let rsp = top - 8;
        *(rsp as *mut u64) = 0;
        (*g).ctx.rsp = rsp as u64;
        (*g).ctx.rip = gost_ctx_start as *const () as usize as u64;
        (*g).ctx.r12 = g as usize as u64;
        (*g).ctx.rbx = 0;
        (*g).ctx.rbp = 0;
        (*g).ctx.r13 = 0;
        (*g).ctx.r14 = 0;
        (*g).ctx.r15 = 0;
        (*g).ctx.rdi = 0;
        (*g).ctx.rsi = 0;
        (*g).ctx.xmm6 = [0u8; 16];
        (*g).ctx.xmm7 = [0u8; 16];
        (*g).ctx.xmm8 = [0u8; 16];
        (*g).ctx.xmm9 = [0u8; 16];
        (*g).ctx.xmm10 = [0u8; 16];
        (*g).ctx.xmm11 = [0u8; 16];
        (*g).ctx.xmm12 = [0u8; 16];
        (*g).ctx.xmm13 = [0u8; 16];
        (*g).ctx.xmm14 = [0u8; 16];
        (*g).ctx.xmm15 = [0u8; 16];
    }
}

fn now_ms() -> i64 {
    #[cfg(windows)]
    unsafe {
        return GetTickCount64() as i64;
    }
    #[cfg(not(windows))]
    unsafe {
        let mut ts: timespec = mem::zeroed();
        libc::clock_gettime(CLOCK_MONOTONIC, &mut ts);
        return ts.tv_sec as i64 * 1000 + (ts.tv_nsec as i64 / 1_000_000);
    }
}

#[cfg(not(windows))]
unsafe fn timespec_now(ts: &mut timespec) {
    #[cfg(windows)]
    {
        let mut ft: FILETIME = mem::zeroed();
        GetSystemTimeAsFileTime(&mut ft);
        let t = ((ft.dwHighDateTime as u64) << 32) | ft.dwLowDateTime as u64;
        let t = t.wrapping_sub(116444736000000000u64);
        ts.tv_sec = (t / 10_000_000) as libc::time_t;
        ts.tv_nsec = ((t % 10_000_000) * 100) as libc::c_long;
    }
    #[cfg(not(windows))]
    {
        libc::clock_gettime(CLOCK_MONOTONIC, ts);
    }
}

#[cfg(not(windows))]
unsafe fn timespec_add_ms(ts: &mut timespec, ms: i64) {
    ts.tv_sec += (ms / 1000) as libc::time_t;
    ts.tv_nsec += ((ms % 1000) * 1_000_000) as libc::c_long;
    if ts.tv_nsec >= 1_000_000_000 {
        ts.tv_sec += 1;
        ts.tv_nsec -= 1_000_000_000;
    }
}

#[inline(always)]
unsafe fn th_less(a: *mut gost_timer, b: *mut gost_timer) -> bool {
    (*a).when_ms < (*b).when_ms
}

unsafe fn th_sift_up(h: &mut Vec<*mut gost_timer>, mut i: usize) -> i64 {
    let mut steps: i64 = 0;
    while i > 0 {
        let p = (i - 1) / 2;
        if !th_less(h[i], h[p]) { break; }
        h.swap(i, p);
        i = p;
        steps += 1;
    }
    steps
}

unsafe fn th_sift_down(h: &mut Vec<*mut gost_timer>, mut i: usize) -> i64 {
    let mut steps: i64 = 0;
    let n = h.len();
    loop {
        let l = i * 2 + 1;
        if l >= n { break; }
        let r = l + 1;
        let mut m = l;
        if r < n && th_less(h[r], h[l]) { m = r; }
        if !th_less(h[m], h[i]) { break; }
        h.swap(i, m);
        i = m;
        steps += 1;
    }
    steps
}

unsafe fn th_push(heap: *mut gost_timer_heap, t: *mut gost_timer) {
    let h = &mut (*heap).v;
    h.push(t);
    stat_inc(&ST_TH_PUSH);
    let steps = th_sift_up(h, h.len() - 1);
    stat_add(&ST_TH_SIFT_UP, steps);
    stat_max(&ST_TH_MAXLEN, h.len() as i64);
}

unsafe fn th_peek_when(heap: *mut gost_timer_heap) -> i64 {
    if heap.is_null() { return i64::MAX; }
    let h = &mut (*heap).v;
    if h.is_empty() { i64::MAX } else { (*h[0]).when_ms }
}

unsafe fn th_pop(heap: *mut gost_timer_heap) -> *mut gost_timer {
    if heap.is_null() { return ptr::null_mut(); }
    let h = &mut (*heap).v;
    if h.is_empty() { return ptr::null_mut(); }
    let top = h[0];
    let last = h.pop().unwrap();
    if !h.is_empty() {
        h[0] = last;
        let steps = th_sift_down(h, 0);
        stat_add(&ST_TH_SIFT_DOWN, steps);
    }
    stat_inc(&ST_TH_POP);
    (*top).next = ptr::null_mut();
    top
}

unsafe fn timer_add(ch: *mut __gost_chan, ms: i64) {
    if ch.is_null() { return; }
    let ms = if ms < 0 { 0 } else { ms };
    let t = libc::malloc(mem::size_of::<gost_timer>()) as *mut gost_timer;
    if t.is_null() {
        __gost_panic(b"out of memory\0".as_ptr(), 13);
    }
    ptr::write_bytes(t, 0, 1);
    (*t).when_ms = now_ms() + ms;
    (*t).ch = ch;
    (*t).next = ptr::null_mut();
    stat_inc(&ST_TIMER_ADD);

    let sched = g_sched_mut();
    os_mutex_lock(&mut sched.mu);
    if sched.timers_heap.is_null() {
        let b = Box::new(gost_timer_heap::new());
        sched.timers_heap = Box::into_raw(b);
    }
    let old_earliest = th_peek_when(sched.timers_heap);
    th_push(sched.timers_heap, t);
    let new_earliest = th_peek_when(sched.timers_heap);
    if old_earliest == i64::MAX || new_earliest < old_earliest {
        os_cond_signal(&mut sched.cv);
    }
    os_mutex_unlock(&mut sched.mu);
}

unsafe fn timer_pop_due_batch(now: i64, limit: usize) -> *mut gost_timer {
    let sched = g_sched_mut();
    let mut list: *mut gost_timer = ptr::null_mut();
    let mut tail: *mut gost_timer = ptr::null_mut();
    if sched.timers_heap.is_null() { return ptr::null_mut(); }
    let mut n = 0usize;
    while n < limit && th_peek_when(sched.timers_heap) <= now {
        let t = th_pop(sched.timers_heap);
        if t.is_null() { break; }
        (*t).next = ptr::null_mut();
        if !tail.is_null() {
            (*tail).next = t;
        } else {
            list = t;
        }
        tail = t;
        n += 1;
    }
    list
}

unsafe fn sudog_queue_push(head: &mut *mut gost_sudog, tail: &mut *mut gost_sudog, s: *mut gost_sudog) {
    (*s).next = ptr::null_mut();
    if !(*tail).is_null() {
        (**tail).next = s;
    } else {
        *head = s;
    }
    *tail = s;
}

unsafe fn sudog_queue_pop(head: &mut *mut gost_sudog, tail: &mut *mut gost_sudog) -> *mut gost_sudog {
    let s = *head;
    if s.is_null() { return ptr::null_mut(); }
    *head = (*s).next;
    if (*head).is_null() { *tail = ptr::null_mut(); }
    (*s).next = ptr::null_mut();
    s
}

unsafe fn sudog_new() -> *mut gost_sudog {
    let sd = libc::malloc(mem::size_of::<gost_sudog>()) as *mut gost_sudog;
    if sd.is_null() { __gost_panic(b"out of memory\0".as_ptr(), 13); }
    ptr::write_bytes(sd, 0, 1);
    stat_inc(&ST_SUDOG_ALLOC);
    sd
}

unsafe fn sudog_del(sd: *mut gost_sudog) {
    if sd.is_null() { return; }
    stat_inc(&ST_SUDOG_FREE);
    libc::free(sd as *mut c_void);
}

unsafe fn q_push_nocheck(head: &mut *mut gost_g, tail: &mut *mut gost_g, g: *mut gost_g) {
    (*g).next = ptr::null_mut();
    if !(*tail).is_null() { (**tail).next = g; } else { *head = g; }
    *tail = g;
}

unsafe fn q_push_enq_ret(head: &mut *mut gost_g, tail: &mut *mut gost_g, g: *mut gost_g) -> bool {
    if (*g).on_runq != 0 {
        stat_inc(&ST_RUNQ_DUPE_BLOCKED);
        return false;
    }
    (*g).on_runq = 1;
    stat_inc(&ST_RUNQ_PUSH);
    q_push_nocheck(head, tail, g);
    true
}

unsafe fn q_push_enq(head: &mut *mut gost_g, tail: &mut *mut gost_g, g: *mut gost_g) {
    let _ = q_push_enq_ret(head, tail, g);
}

unsafe fn q_pop(head: &mut *mut gost_g, tail: &mut *mut gost_g, clear_on_runq: bool) -> *mut gost_g {
    let g = *head;
    if g.is_null() { return ptr::null_mut(); }
    *head = (*g).next;
    if (*head).is_null() { *tail = ptr::null_mut(); }
    (*g).next = ptr::null_mut();
    if clear_on_runq {
        (*g).on_runq = 0;
        stat_inc(&ST_RUNQ_POP);
    }
    g
}

unsafe fn p_fill_from_global_locked(p: *mut gost_p, max_batch: i32) {
    let sched = g_sched_mut();
    let mut n = 0;
    while n < max_batch && !sched.runq_head.is_null() {
        let g = q_pop(&mut sched.runq_head, &mut sched.runq_tail, false);
        if g.is_null() { break; }
        q_push_nocheck(&mut (*p).runq_head, &mut (*p).runq_tail, g);
        (*p).runq_len += 1;
        n += 1;
    }
}

unsafe fn p_pop_runnable_locked(p: *mut gost_p) -> *mut gost_g {
    let g = q_pop(&mut (*p).runq_head, &mut (*p).runq_tail, true);
    if !g.is_null() { (*p).runq_len -= 1; }
    g
}

unsafe fn p_steal_locked(m: *mut gost_m, max_take: i32) -> bool {
    if m.is_null() || (*m).p.is_null() { return false; }
    let sched = g_sched_mut();
    let gp = sched.gomaxprocs;
    if gp <= 1 { return false; }
    stat_inc(&ST_STEAL_CALLS);
    let start = ((*m).id + 1) % gp;
    for k in 0..(gp - 1) {
        let idx = (start + k) % gp;
        let v = sched.ps.add(idx as usize);
        if v == (*m).p { continue; }
        let avail = (*v).runq_len;
        if avail <= 0 { continue; }
        let mut take = avail / 2;
        if take < 1 { take = 1; }
        if take > max_take { take = max_take; }
        let mut got = 0;
        while take > 0 && !(*v).runq_head.is_null() {
            let g = q_pop(&mut (*v).runq_head, &mut (*v).runq_tail, false);
            if g.is_null() { break; }
            q_push_nocheck(&mut (*(*m).p).runq_head, &mut (*(*m).p).runq_tail, g);
            (*(*m).p).runq_len += 1;
            (*v).runq_len -= 1;
            got += 1;
            take -= 1;
        }
        if got > 0 {
            stat_add(&ST_STEAL_TAKE, got as i64);
            return true;
        }
    }
    stat_inc(&ST_STEAL_FAIL);
    false
}
#[unsafe(no_mangle)]
pub extern "C" fn gost_ctx_entry(arg: *mut c_void) -> ! {
    unsafe {
        let g = arg as *mut gost_g;
        tls_g_set(g);
        let entry = (*g).entry;
        let ctx = (*g).entry_ctx;
        entry(ctx);
        let sched = g_sched_mut();
        os_mutex_lock(&mut sched.mu);
        (*g).state = G_DEAD;
        os_mutex_unlock(&mut sched.mu);
        let m = tls_m_get();
        if m.is_null() {
            __gost_panic(b"no scheduler\0".as_ptr(), 13);
        }
        gost_ctx_swap(&mut (*g).ctx, &mut (*m).sched_ctx);
        loop { std::hint::spin_loop(); }
    }
}

extern "C" fn gost_main_tramp(ctx: *mut c_void) {
    unsafe {
        let main_fn: gost_main_fn = mem::transmute(ctx);
        let code = main_fn();
        let g = tls_g_get();
        if !g.is_null() {
            (*g).exit_code = code;
        }
    }
}

unsafe fn new_g(entry: gost_thread_fn, ctx: *mut c_void, is_main: bool) -> *mut gost_g {
    let g = libc::malloc(mem::size_of::<gost_g>()) as *mut gost_g;
    if g.is_null() {
        __gost_panic(b"out of memory\0".as_ptr(), 13);
    }
    ptr::write_bytes(g, 0, 1);
    (*g).entry = entry;
    (*g).entry_ctx = ctx;
    (*g).state = G_RUNNABLE;
    (*g).is_main = if is_main { 1 } else { 0 };
    (*g).on_runq = 0;
    G_LIVE.fetch_add(1, Ordering::Relaxed);
    stat_inc(&ST_G_CREATED);
    ctx_init_g(g);
    g
}

unsafe extern "C" fn sched_init_once() {
    let sched = g_sched_mut();
    os_mutex_init(&mut sched.mu);
    os_cond_init(&mut sched.cv);
    sched.runq_head = ptr::null_mut();
    sched.runq_tail = ptr::null_mut();
    sched.gomaxprocs = 1;
    sched.mcount = 1;
    sched.shutting_down = 0;
    sched.main_done = 0;
    sched.main_exit = 0;
    sched.rr_p = 0;
    sched.timers_heap = ptr::null_mut();
    sched.ps = ptr::null_mut();
    sched.ms = ptr::null_mut();
    sched.mthreads = ptr::null_mut();
}

unsafe fn goready_locked_one(sched: &mut gost_sched, g: *mut gost_g, enq: &mut bool) {
    if g.is_null() { return; }
    if (*g).state == G_DEAD || (*g).state == G_RUNNING {
        return;
    }
    if (*g).state == G_PARKING {
        (*g).park_ready = 1;
        return;
    }
    if (*g).state == G_WAITING {
        (*g).state = G_RUNNABLE;
    }
    q_push_enq(&mut sched.runq_head, &mut sched.runq_tail, g);
    *enq = true;
}

unsafe fn goready_to_p_locked(sched: &mut gost_sched, g: *mut gost_g, p: *mut gost_p, enq: &mut bool) {
    if g.is_null() { return; }
    if (*g).state == G_DEAD || (*g).state == G_RUNNING {
        return;
    }
    if (*g).state == G_PARKING {
        (*g).park_ready = 1;
        return;
    }
    if (*g).state == G_WAITING {
        (*g).state = G_RUNNABLE;
    }
    let pushed = if p.is_null() {
        q_push_enq_ret(&mut sched.runq_head, &mut sched.runq_tail, g)
    } else {
        let ok = q_push_enq_ret(&mut (*p).runq_head, &mut (*p).runq_tail, g);
        if ok { (*p).runq_len += 1; }
        ok
    };
    if pushed { *enq = true; }
}

unsafe fn goready_batch(gs: &mut Vec<*mut gost_g>) {
    if gs.is_empty() { return; }
    stat_add(&ST_GOREADY_CALLS, gs.len() as i64);
    let sched = g_sched_mut();
    os_mutex_lock(&mut sched.mu);
    let mut enq = false;
    for &g in gs.iter() {
        goready_locked_one(sched, g, &mut enq);
    }
    if enq {
        if sched.gomaxprocs > 1 {
            os_cond_broadcast(&mut sched.cv);
        } else {
            os_cond_signal(&mut sched.cv);
        }
    }
    os_mutex_unlock(&mut sched.mu);
    gs.clear();
}

unsafe fn goready_batch_rr(gs: &mut Vec<*mut gost_g>) {
    if gs.is_empty() { return; }
    stat_add(&ST_GOREADY_CALLS, gs.len() as i64);
    let sched = g_sched_mut();
    os_mutex_lock(&mut sched.mu);
    let mut enq = false;
    let gp = sched.gomaxprocs;
    for &g in gs.iter() {
        if gp > 1 && !sched.ps.is_null() {
            let idx = sched.rr_p % gp;
            let p = sched.ps.add(idx as usize);
            sched.rr_p = (sched.rr_p + 1) % gp;
            goready_to_p_locked(sched, g, p, &mut enq);
        } else {
            goready_to_p_locked(sched, g, ptr::null_mut(), &mut enq);
        }
    }
    if enq {
        if sched.gomaxprocs > 1 {
            os_cond_broadcast(&mut sched.cv);
        } else {
            os_cond_signal(&mut sched.cv);
        }
    }
    os_mutex_unlock(&mut sched.mu);
    gs.clear();
}

unsafe fn goready(g: *mut gost_g) {
    if g.is_null() { return; }
    stat_inc(&ST_GOREADY_CALLS);
    let sched = g_sched_mut();
    os_mutex_lock(&mut sched.mu);
    let mut enq = false;
    goready_locked_one(sched, g, &mut enq);
    if enq {
        os_cond_signal(&mut sched.cv);
    }
    os_mutex_unlock(&mut sched.mu);
}

unsafe fn gopark() {
    let g = tls_g_get();
    let m = tls_m_get();
    if g.is_null() || m.is_null() { return; }
    stat_inc(&ST_GOPARK_CALLS);
    let sched = g_sched_mut();
    os_mutex_lock(&mut sched.mu);
    (*g).park_ready = 0;
    (*g).state = G_PARKING;
    os_mutex_unlock(&mut sched.mu);
    park_fuzz_point();
    gost_ctx_swap(&mut (*g).ctx, &mut (*m).sched_ctx);
}

unsafe extern "C" fn m_worker_entry(arg: *mut c_void) -> *mut c_void {
    let m = arg as *mut gost_m;
    tls_m_set(m);
    sched_loop(m);
    ptr::null_mut()
}

unsafe fn sched_loop(m: *mut gost_m) {
    let batch = 32;
    loop {
        let sched = g_sched_mut();
        os_mutex_lock(&mut sched.mu);
        loop {
            if sched.main_done != 0 && G_LIVE.load(Ordering::Relaxed) == 0 {
                sched.shutting_down = 1;
                os_cond_broadcast(&mut sched.cv);
            }
            if sched.shutting_down != 0 { break; }
            if !(*m).p.is_null() && !(*(*m).p).runq_head.is_null() { break; }
            if !(*m).p.is_null() && !sched.runq_head.is_null() {
                p_fill_from_global_locked((*m).p, batch);
                if !(*(*m).p).runq_head.is_null() { break; }
            }
            if !(*m).p.is_null() && (*(*m).p).runq_head.is_null() && sched.runq_head.is_null() {
                if p_steal_locked(m, batch) { break; }
            }
            let now = now_ms();
            let next_when = th_peek_when(sched.timers_heap);
            if next_when <= now { break; }
            if next_when != i64::MAX {
                let mut wait_ms = next_when - now;
                if wait_ms < 0 { wait_ms = 0; }
                #[cfg(windows)]
                {
                    if wait_ms > 50 { wait_ms = 50; }
                }
                os_cond_timedwait(&mut sched.cv, &mut sched.mu, wait_ms);
            } else {
                os_cond_wait(&mut sched.cv, &mut sched.mu);
            }
        }
        if sched.shutting_down != 0 {
            os_mutex_unlock(&mut sched.mu);
            break;
        }
        let mut due: *mut gost_timer = ptr::null_mut();
        let now = now_ms();
        if th_peek_when(sched.timers_heap) <= now {
            due = timer_pop_due_batch(now, TIMER_DUE_BATCH);
        }
        if !due.is_null() {
            os_mutex_unlock(&mut sched.mu);
            timer_fire_list(due);
            continue;
        }
        let mut g: *mut gost_g = ptr::null_mut();
        if !(*m).p.is_null() {
            g = p_pop_runnable_locked((*m).p);
        }
        if g.is_null() {
            g = q_pop(&mut sched.runq_head, &mut sched.runq_tail, true);
        }
        if !g.is_null() {
            (*m).curg = g;
            (*g).state = G_RUNNING;
        }
        os_mutex_unlock(&mut sched.mu);
        if g.is_null() { continue; }
        tls_g_set(g);
        stat_inc(&ST_SCHED_SWITCH);
        gost_ctx_swap(&mut (*m).sched_ctx, &mut (*g).ctx);
        (*m).curg = ptr::null_mut();
        if (*g).state == G_PARKING {
            let sched = g_sched_mut();
            os_mutex_lock(&mut sched.mu);
            if (*g).park_ready != 0 {
                (*g).park_ready = 0;
                (*g).state = G_RUNNABLE;
                q_push_enq(&mut sched.runq_head, &mut sched.runq_tail, g);
                os_cond_signal(&mut sched.cv);
            } else {
                (*g).state = G_WAITING;
            }
            os_mutex_unlock(&mut sched.mu);
        }
        if (*g).state == G_DEAD {
            if (*g).is_main != 0 {
                let sched = g_sched_mut();
                os_mutex_lock(&mut sched.mu);
                sched.main_exit = (*g).exit_code;
                sched.main_done = 1;
                os_mutex_unlock(&mut sched.mu);
            }
            stat_inc(&ST_G_FREED);
            G_LIVE.fetch_sub(1, Ordering::Relaxed);
            stack_free((*g).stack_base, (*g).stack_reserve);
            libc::free(g as *mut c_void);
        }
    }
}

unsafe fn selq_append_locked(ch: *mut __gost_chan, n: *mut gost_selnode) {
    (*n).prev_ch = (*ch).selq_tail;
    (*n).next_ch = ptr::null_mut();
    if !(*ch).selq_tail.is_null() {
        (*(*ch).selq_tail).next_ch = n;
    } else {
        (*ch).selq_head = n;
    }
    (*ch).selq_tail = n;
}

unsafe fn selq_unlink_locked(ch: *mut __gost_chan, n: *mut gost_selnode) {
    if !(*n).prev_ch.is_null() {
        (*(*n).prev_ch).next_ch = (*n).next_ch;
    } else if (*ch).selq_head == n {
        (*ch).selq_head = (*n).next_ch;
    }
    if !(*n).next_ch.is_null() {
        (*(*n).next_ch).prev_ch = (*n).prev_ch;
    } else if (*ch).selq_tail == n {
        (*ch).selq_tail = (*n).prev_ch;
    }
    (*n).prev_ch = ptr::null_mut();
    (*n).next_ch = ptr::null_mut();
}

unsafe fn __gost_chan_retain(ch: *mut __gost_chan) {
    if ch.is_null() { return; }
    (*ch).refcount.fetch_add(1, Ordering::Relaxed);
}

unsafe fn __gost_chan_release(ch: *mut __gost_chan) {
    if ch.is_null() { return; }
    if (*ch).refcount.fetch_sub(1, Ordering::AcqRel) != 1 { return; }
    os_mutex_destroy(&mut (*ch).mu);
    if !(*ch).buf.is_null() { __gost_free((*ch).buf as *mut c_void, 0, 1); }
    libc::free(ch as *mut c_void);
}

unsafe fn chan_send_nowait_collect(ch: *mut __gost_chan, wake: &mut Vec<*mut gost_g>) {
    if ch.is_null() { return; }
    os_mutex_lock(&mut (*ch).mu);
    if (*ch).closed != 0 {
        os_mutex_unlock(&mut (*ch).mu);
        return;
    }
    let recv_waiter = sudog_queue_pop(&mut (*ch).recvq_head, &mut (*ch).recvq_tail);
    if !recv_waiter.is_null() {
        if (*ch).elem_size > 0 && !(*recv_waiter).elem.is_null() {
            ptr::write_bytes((*recv_waiter).elem, 0, (*ch).elem_size);
        }
        (*recv_waiter).success = 1;
        let do_sel = !(*ch).selq_head.is_null();
        os_mutex_unlock(&mut (*ch).mu);
        wake.push((*recv_waiter).g);
        if do_sel { select_notify_chan(ch); }
        return;
    }
    if (*ch).cap > 0 && (*ch).len < (*ch).cap {
        if (*ch).elem_size > 0 {
            let slot = (*ch).buf.add((*ch).tail as usize * (*ch).elem_size);
            ptr::write_bytes(slot, 0, (*ch).elem_size);
        }
        (*ch).tail = ((*ch).tail + 1) % (*ch).cap;
        (*ch).len += 1;
        let do_sel = !(*ch).selq_head.is_null();
        os_mutex_unlock(&mut (*ch).mu);
        if do_sel { select_notify_chan(ch); }
        return;
    }
    os_mutex_unlock(&mut (*ch).mu);
}

unsafe fn timer_fire_list(mut list: *mut gost_timer) {
    let mut wake: Vec<*mut gost_g> = Vec::with_capacity(TIMER_WAKE_BATCH);
    while !list.is_null() {
        let next = (*list).next;
        chan_send_nowait_collect((*list).ch, &mut wake);
        stat_inc(&ST_TIMER_FIRED);
        __gost_chan_release((*list).ch);
        libc::free(list as *mut c_void);
        list = next;
        if wake.len() >= TIMER_WAKE_BATCH {
            goready_batch(&mut wake);
        }
    }
    if !wake.is_empty() {
        goready_batch(&mut wake);
    }
}

unsafe fn select_notify_chan(ch: *mut __gost_chan) {
    if ch.is_null() { return; }
    stat_inc(&ST_SELECT_NOTIFY_CALLS);
    let list = {
        os_mutex_lock(&mut (*ch).mu);
        let list = (*ch).selq_head;
        (*ch).selq_head = ptr::null_mut();
        (*ch).selq_tail = ptr::null_mut();
        let mut n = list;
        while !n.is_null() {
            (*n).detached = 1;
            n = (*n).next_ch;
        }
        os_mutex_unlock(&mut (*ch).mu);
        list
    };

    let sched = g_sched_mut();
    os_mutex_lock(&mut sched.mu);
    let mut enq = false;
    let mut n2 = list;
    while !n2.is_null() {
        let next = (*n2).next_ch;
        let w = (*n2).w;
        if !w.is_null() {
            if (*w).fired.swap(1, Ordering::Relaxed) == 0 {
                stat_inc(&ST_SELECT_WAKE);
                goready_locked_one(sched, (*w).g, &mut enq);
            }
        }
        n2 = next;
    }
    if enq {
        if sched.gomaxprocs > 1 {
            os_cond_broadcast(&mut sched.cv);
        } else {
            os_cond_signal(&mut sched.cv);
        }
    }
    os_mutex_unlock(&mut sched.mu);
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_rt_init() {
    G_SCHED_ONCE.call_once(|| unsafe { sched_init_once() });
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_rt_start(main_fn: Option<gost_main_fn>) -> i32 {
    unsafe {
        __gost_rt_init();
        let main_fn = match main_fn { Some(f) => f, None => return 0 };
        let sched = g_sched_mut();
        os_mutex_lock(&mut sched.mu);
        sched.shutting_down = 0;
        sched.main_done = 0;
        sched.main_exit = 0;
        if sched.timers_heap.is_null() {
            let b = Box::new(gost_timer_heap::new());
            sched.timers_heap = Box::into_raw(b);
        } else {
            (*sched.timers_heap).v.clear();
        }
        os_mutex_unlock(&mut sched.mu);

        let gp = env_i("GOST_GOMAXPROCS", 1);
        sched.gomaxprocs = gp;
        sched.mcount = gp;

        let ps = libc::calloc(gp as usize, mem::size_of::<gost_p>()) as *mut gost_p;
        let ms = libc::calloc(gp as usize, mem::size_of::<gost_m>()) as *mut gost_m;
        let mt = libc::calloc(if gp > 1 { (gp - 1) as usize } else { 1 }, mem::size_of::<OsThread>()) as *mut OsThread;
        if ps.is_null() || ms.is_null() || mt.is_null() {
            __gost_panic(b"out of memory\0".as_ptr(), 13);
        }
        sched.ps = ps;
        sched.ms = ms;
        sched.mthreads = mt;

        for i in 0..gp {
            (*ps.add(i as usize)).id = i;
            (*ms.add(i as usize)).id = i;
            (*ms.add(i as usize)).p = ps.add(i as usize);
        }

        for i in 1..gp {
            let mptr = ms.add(i as usize) as *mut c_void;
            let rc = os_thread_create_worker(mt.add((i - 1) as usize), mptr);
            if rc != 0 { __gost_panic(b"failed to spawn worker\0".as_ptr(), 22); }
        }

        tls_m_set(ms);

        let ctx = main_fn as *mut c_void;
        let main_g = new_g(gost_main_tramp, ctx, true);
        goready(main_g);

        sched_loop(ms);

        for i in 1..gp {
            let t = *mt.add((i - 1) as usize);
            os_thread_join(t);
        }

        __gost_rt_dump_stats();

        libc::free(mt as *mut c_void);
        libc::free(ms as *mut c_void);
        libc::free(ps as *mut c_void);
        sched.mthreads = ptr::null_mut();
        sched.ms = ptr::null_mut();
        sched.ps = ptr::null_mut();

        sched.main_exit
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_panic(msg: *const u8, len: usize) {
    unsafe {
        if !msg.is_null() && len > 0 {
            fd_write_bytes(2, std::slice::from_raw_parts(msg, len));
        }
        fd_write_bytes(2, b"\n");
        libc::abort();
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_println_str(ptr: *const u8, len: usize) {
    unsafe {
        if !ptr.is_null() && len > 0 {
            fd_write_bytes(1, std::slice::from_raw_parts(ptr, len));
        }
        fd_write_bytes(1, b"\n");
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_error_new(_msg: *const u8, _len: usize) -> i32 {
    let id = G_ERROR_NEXT.fetch_add(1, Ordering::Relaxed);
    if id == 0 { return 1; }
    id
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_alloc(size: usize, _align: usize) -> *mut c_void {
    unsafe {
        let size = if size == 0 { 1 } else { size };
        let p = libc::malloc(size);
        if p.is_null() {
            __gost_panic(b"out of memory\0".as_ptr(), 13);
        }
        p
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_free(p: *mut c_void, _size: usize, _align: usize) {
    unsafe {
        if !p.is_null() {
            libc::free(p);
        }
    }
}

#[repr(C)]
struct thread_start { f: gost_thread_fn, ctx: *mut c_void }

unsafe extern "C" fn thread_entry(arg: *mut c_void) -> *mut c_void {
    let start = arg as *mut thread_start;
    let f = (*start).f;
    let ctx = (*start).ctx;
    libc::free(start as *mut c_void);
    f(ctx);
    ptr::null_mut()
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_spawn_thread(f: gost_thread_fn, ctx: *mut c_void) {
    unsafe {
        let start = libc::malloc(mem::size_of::<thread_start>()) as *mut thread_start;
        if start.is_null() { __gost_panic(b"out of memory\0".as_ptr(), 13); }
        (*start).f = f;
        (*start).ctx = ctx;
        if os_thread_create_detached(start as *mut c_void) != 0 {
            libc::free(start as *mut c_void);
            __gost_panic(b"failed to spawn thread\0".as_ptr(), 22);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_go_spawn(f: gost_thread_fn, ctx: *mut c_void) {
    unsafe {
        __gost_rt_init();
        if tls_m_get().is_null() {
            __gost_spawn_thread(f, ctx);
            return;
        }
        let g = new_g(f, ctx, false);
        goready(g);
    }
}
#[unsafe(no_mangle)]
pub extern "C" fn __gost_chan_new(elem_size: usize, cap: i32) -> *mut __gost_chan {
    unsafe {
        let ch = libc::malloc(mem::size_of::<__gost_chan>()) as *mut __gost_chan;
        if ch.is_null() { __gost_panic(b"out of memory\0".as_ptr(), 13); }
        ptr::write_bytes(ch, 0, 1);
        os_mutex_init(&mut (*ch).mu);
        (*ch).elem_size = elem_size;
        (*ch).cap = cap;
        (*ch).len = 0;
        (*ch).head = 0;
        (*ch).tail = 0;
        (*ch).closed = 0;
        (*ch).refcount.store(1, Ordering::Relaxed);
        (*ch).sendq_head = ptr::null_mut();
        (*ch).sendq_tail = ptr::null_mut();
        (*ch).recvq_head = ptr::null_mut();
        (*ch).recvq_tail = ptr::null_mut();
        (*ch).selq_head = ptr::null_mut();
        (*ch).selq_tail = ptr::null_mut();
        (*ch).buf = ptr::null_mut();
        if elem_size > 0 && cap > 0 {
            let total = elem_size * cap as usize;
            (*ch).buf = __gost_alloc(total, 1) as *mut u8;
        }
        ch
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_chan_send(ch: *mut __gost_chan, elem: *const c_void) -> i32 {
    unsafe {
        if ch.is_null() { return 1; }
        os_mutex_lock(&mut (*ch).mu);
        if (*ch).closed != 0 {
            os_mutex_unlock(&mut (*ch).mu);
            return 1;
        }
        let recv_waiter = sudog_queue_pop(&mut (*ch).recvq_head, &mut (*ch).recvq_tail);
        if !recv_waiter.is_null() {
            stat_inc(&ST_CHAN_SEND_FAST);
            if (*ch).elem_size > 0 && !elem.is_null() && !(*recv_waiter).elem.is_null() {
                ptr::copy_nonoverlapping(elem as *const u8, (*recv_waiter).elem as *mut u8, (*ch).elem_size);
            }
            (*recv_waiter).success = 1;
            let do_sel = !(*ch).selq_head.is_null();
            os_mutex_unlock(&mut (*ch).mu);
            goready((*recv_waiter).g);
            if do_sel { select_notify_chan(ch); }
            return 0;
        }
        if (*ch).cap > 0 && (*ch).len < (*ch).cap {
            stat_inc(&ST_CHAN_SEND_BUF);
            if (*ch).elem_size > 0 {
                let slot = (*ch).buf.add((*ch).tail as usize * (*ch).elem_size);
                ptr::copy_nonoverlapping(elem as *const u8, slot, (*ch).elem_size);
            }
            (*ch).tail = ((*ch).tail + 1) % (*ch).cap;
            (*ch).len += 1;
            let do_sel = !(*ch).selq_head.is_null();
            os_mutex_unlock(&mut (*ch).mu);
            if do_sel { select_notify_chan(ch); }
            return 0;
        }
        stat_inc(&ST_CHAN_SEND_BLOCK);
        let sd = sudog_new();
        (*sd).g = tls_g_get();
        (*sd).elem = elem as *mut c_void;
        (*sd).success = 0;
        (*sd).next = ptr::null_mut();
        sudog_queue_push(&mut (*ch).sendq_head, &mut (*ch).sendq_tail, sd);
        os_mutex_unlock(&mut (*ch).mu);
        gopark();
        let ok = (*sd).success;
        sudog_del(sd);
        if ok != 0 { 0 } else { 1 }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_chan_recv(ch: *mut __gost_chan, out_elem: *mut c_void) -> i32 {
    unsafe {
        if ch.is_null() { return 1; }
        os_mutex_lock(&mut (*ch).mu);
        if (*ch).cap > 0 && (*ch).len > 0 {
            stat_inc(&ST_CHAN_RECV_BUF);
            if (*ch).elem_size > 0 && !out_elem.is_null() {
                let slot = (*ch).buf.add((*ch).head as usize * (*ch).elem_size);
                ptr::copy_nonoverlapping(slot, out_elem as *mut u8, (*ch).elem_size);
            }
            (*ch).head = ((*ch).head + 1) % (*ch).cap;
            (*ch).len -= 1;
            let send_waiter = sudog_queue_pop(&mut (*ch).sendq_head, &mut (*ch).sendq_tail);
            if !send_waiter.is_null() {
                stat_inc(&ST_CHAN_RECV_FAST);
                if (*ch).elem_size > 0 && !(*send_waiter).elem.is_null() {
                    let slot = (*ch).buf.add((*ch).tail as usize * (*ch).elem_size);
                    ptr::copy_nonoverlapping((*send_waiter).elem as *const u8, slot, (*ch).elem_size);
                }
                (*ch).tail = ((*ch).tail + 1) % (*ch).cap;
                (*ch).len += 1;
                (*send_waiter).success = 1;
                let do_sel = !(*ch).selq_head.is_null();
                os_mutex_unlock(&mut (*ch).mu);
                goready((*send_waiter).g);
                if do_sel { select_notify_chan(ch); }
                return 0;
            }
            let do_sel = !(*ch).selq_head.is_null();
            os_mutex_unlock(&mut (*ch).mu);
            if do_sel { select_notify_chan(ch); }
            return 0;
        }
        let send_waiter = sudog_queue_pop(&mut (*ch).sendq_head, &mut (*ch).sendq_tail);
        if !send_waiter.is_null() {
            stat_inc(&ST_CHAN_RECV_FAST);
            if (*ch).elem_size > 0 && !out_elem.is_null() && !(*send_waiter).elem.is_null() {
                ptr::copy_nonoverlapping((*send_waiter).elem as *const u8, out_elem as *mut u8, (*ch).elem_size);
            }
            (*send_waiter).success = 1;
            let do_sel = !(*ch).selq_head.is_null();
            os_mutex_unlock(&mut (*ch).mu);
            goready((*send_waiter).g);
            if do_sel { select_notify_chan(ch); }
            return 0;
        }
        if (*ch).closed != 0 {
            os_mutex_unlock(&mut (*ch).mu);
            return 1;
        }
        stat_inc(&ST_CHAN_RECV_BLOCK);
        let sd = sudog_new();
        (*sd).g = tls_g_get();
        (*sd).elem = out_elem;
        (*sd).success = 0;
        (*sd).next = ptr::null_mut();
        sudog_queue_push(&mut (*ch).recvq_head, &mut (*ch).recvq_tail, sd);
        os_mutex_unlock(&mut (*ch).mu);
        gopark();
        let ok = (*sd).success;
        sudog_del(sd);
        if ok != 0 { 0 } else { 1 }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_chan_close(ch: *mut __gost_chan) -> i32 {
    unsafe {
        if ch.is_null() { return 1; }
        os_mutex_lock(&mut (*ch).mu);
        if (*ch).closed != 0 {
            os_mutex_unlock(&mut (*ch).mu);
            return 1;
        }
        stat_inc(&ST_CHAN_CLOSE);
        (*ch).closed = 1;
        let send_list = (*ch).sendq_head;
        let recv_list = (*ch).recvq_head;
        let do_sel = !(*ch).selq_head.is_null();
        (*ch).sendq_head = ptr::null_mut();
        (*ch).sendq_tail = ptr::null_mut();
        (*ch).recvq_head = ptr::null_mut();
        (*ch).recvq_tail = ptr::null_mut();
        os_mutex_unlock(&mut (*ch).mu);
        let mut s = send_list;
        while !s.is_null() {
            let next = (*s).next;
            (*s).success = 0;
            (*s).next = ptr::null_mut();
            goready((*s).g);
            s = next;
        }
        let mut r = recv_list;
        while !r.is_null() {
            let next = (*r).next;
            (*r).success = 0;
            (*r).next = ptr::null_mut();
            goready((*r).g);
            r = next;
        }
        if do_sel { select_notify_chan(ch); }
        0
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_chan_drop(ch: *mut __gost_chan) {
    unsafe {
        if ch.is_null() { return; }
        __gost_chan_close(ch);
        __gost_chan_release(ch);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_chan_can_send(ch: *mut __gost_chan) -> i32 {
    unsafe {
        if ch.is_null() { return 0; }
        os_mutex_lock(&mut (*ch).mu);
        let mut ready = 0;
        if (*ch).closed == 0 {
            if !(*ch).recvq_head.is_null() {
                ready = 1;
            } else if (*ch).cap == 0 {
                ready = 0;
            } else {
                ready = if (*ch).len < (*ch).cap { 1 } else { 0 };
            }
        }
        os_mutex_unlock(&mut (*ch).mu);
        ready
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_chan_can_recv(ch: *mut __gost_chan) -> i32 {
    unsafe {
        if ch.is_null() { return 0; }
        os_mutex_lock(&mut (*ch).mu);
        let mut ready = 0;
        if (*ch).cap == 0 {
            ready = if !(*ch).sendq_head.is_null() || (*ch).closed != 0 { 1 } else { 0 };
        } else {
            if (*ch).len > 0 { ready = 1; }
            else if !(*ch).sendq_head.is_null() { ready = 1; }
            else if (*ch).closed != 0 { ready = 1; }
        }
        os_mutex_unlock(&mut (*ch).mu);
        ready
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_select_wait(chans: *mut *mut __gost_chan, n: u32) -> i32 {
    unsafe {
        if chans.is_null() || n == 0 || tls_g_get().is_null() {
            return 0;
        }
        let w = libc::malloc(mem::size_of::<gost_selwaiter>()) as *mut gost_selwaiter;
        if w.is_null() { __gost_panic(b"out of memory\0".as_ptr(), 13); }
        ptr::write_bytes(w, 0, 1);
        (*w).g = tls_g_get();
        (*w).fired.store(0, Ordering::Relaxed);
        (*w).nodes = ptr::null_mut();
        stat_inc(&ST_SELECT_WAITER_ALLOC);

        let mut node_count: u32 = 0;
        for i in 0..n {
            let ch = *chans.add(i as usize);
            if ch.is_null() { continue; }
            let mut dup = false;
            for j in 0..i {
                if *chans.add(j as usize) == ch { dup = true; break; }
            }
            if dup { continue; }
            let node = libc::malloc(mem::size_of::<gost_selnode>()) as *mut gost_selnode;
            if node.is_null() { __gost_panic(b"out of memory\0".as_ptr(), 13); }
            ptr::write_bytes(node, 0, 1);
            (*node).w = w;
            (*node).ch = ch;
            (*node).detached = 0;
            (*node).next_w = (*w).nodes;
            (*w).nodes = node;
            stat_inc(&ST_SELECT_NODE_ALLOC);
            os_mutex_lock(&mut (*ch).mu);
            selq_append_locked(ch, node);
            os_mutex_unlock(&mut (*ch).mu);
            node_count += 1;
        }
        if node_count == 0 {
            libc::free(w as *mut c_void);
            stat_inc(&ST_SELECT_WAITER_FREE);
            return 0;
        }

        gopark();

        let mut node = (*w).nodes;
        while !node.is_null() {
            let next = (*node).next_w;
            if !(*node).ch.is_null() {
                os_mutex_lock(&mut (*(*node).ch).mu);
                if (*node).detached == 0 {
                    selq_unlink_locked((*node).ch, node);
                }
                os_mutex_unlock(&mut (*(*node).ch).mu);
            }
            libc::free(node as *mut c_void);
            stat_inc(&ST_SELECT_NODE_FREE);
            node = next;
        }
        libc::free(w as *mut c_void);
        stat_inc(&ST_SELECT_WAITER_FREE);
        0
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_slice_new(elem_size: usize, len: i64, cap: i64, elem_drop: Option<extern "C" fn(*mut c_void)>) -> *mut __gost_slice {
    unsafe {
        if cap < len {
            __gost_panic(b"slice cap < len\0".as_ptr(), 15);
        }
        let s = __gost_alloc(mem::size_of::<__gost_slice>(), 1) as *mut __gost_slice;
        (*s).elem_size = elem_size;
        (*s).len = len;
        (*s).cap = cap;
        (*s).elem_drop = elem_drop;
        (*s).data = ptr::null_mut();
        if elem_size > 0 && cap > 0 {
            let total = elem_size * cap as usize;
            (*s).data = __gost_alloc(total, 1) as *mut u8;
            ptr::write_bytes((*s).data, 0, total);
        }
        s
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_slice_drop(s: *mut __gost_slice) {
    unsafe {
        if s.is_null() { return; }
        if let Some(drop_fn) = (*s).elem_drop {
            if (*s).elem_size > 0 && !(*s).data.is_null() {
                for i in 0..(*s).len {
                    drop_fn((*s).data.add((i as usize) * (*s).elem_size) as *mut c_void);
                }
            }
        }
        if !(*s).data.is_null() { __gost_free((*s).data as *mut c_void, 0, 1); }
        __gost_free(s as *mut c_void, 0, 1);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_slice_len(s: *mut __gost_slice) -> i64 {
    unsafe { if s.is_null() { 0 } else { (*s).len } }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_slice_data(s: *mut __gost_slice) -> *mut c_void {
    unsafe { if s.is_null() { ptr::null_mut() } else { (*s).data as *mut c_void } }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_slice_bounds_check(s: *mut __gost_slice, i: i64) {
    unsafe {
        if s.is_null() || i < 0 || i >= (*s).len {
            __gost_panic(b"slice index out of bounds\0".as_ptr(), 25);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_slice_push(s: *mut __gost_slice, elem_bytes: *const c_void) {
    unsafe {
        if s.is_null() { __gost_panic(b"slice is null\0".as_ptr(), 13); }
        if (*s).len == (*s).cap {
            let new_cap = if (*s).cap > 0 { (*s).cap * 2 } else { 1 };
            let total = (*s).elem_size * new_cap as usize;
            let new_data = __gost_alloc(total, 1) as *mut u8;
            if !(*s).data.is_null() && (*s).elem_size > 0 && (*s).len > 0 {
                ptr::copy_nonoverlapping((*s).data, new_data, (*s).elem_size * (*s).len as usize);
            }
            if !(*s).data.is_null() { __gost_free((*s).data as *mut c_void, 0, 1); }
            (*s).data = new_data;
            (*s).cap = new_cap;
        }
        if (*s).elem_size > 0 {
            ptr::copy_nonoverlapping(elem_bytes as *const u8, (*s).data.add((*s).len as usize * (*s).elem_size), (*s).elem_size);
        }
        (*s).len += 1;
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_slice_pop(s: *mut __gost_slice, out_elem_bytes: *mut c_void) -> i32 {
    unsafe {
        if s.is_null() || (*s).len == 0 { return 1; }
        (*s).len -= 1;
        if (*s).elem_size > 0 && !(*s).data.is_null() && !out_elem_bytes.is_null() {
            ptr::copy_nonoverlapping((*s).data.add((*s).len as usize * (*s).elem_size), out_elem_bytes as *mut u8, (*s).elem_size);
        }
        0
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_shared_new(payload_size: usize, drop_payload: Option<extern "C" fn(*mut c_void)>, payload_bytes: *const c_void) -> *mut __gost_shared {
    unsafe {
        let total = mem::size_of::<__gost_shared>() + if payload_size > 0 { payload_size } else { 1 };
        let s = __gost_alloc(total, 1) as *mut __gost_shared;
        (*s).refcount.store(1, Ordering::Relaxed);
        (*s).drop_payload = drop_payload;
        if payload_size > 0 && !payload_bytes.is_null() {
            ptr::copy_nonoverlapping(payload_bytes as *const u8, (*s).payload.as_mut_ptr(), payload_size);
        }
        s
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_shared_inc(s: *mut __gost_shared) {
    unsafe { if !s.is_null() { (*s).refcount.fetch_add(1, Ordering::Relaxed); } }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_shared_dec(s: *mut __gost_shared) {
    unsafe {
        if s.is_null() { return; }
        if (*s).refcount.fetch_sub(1, Ordering::AcqRel) == 1 {
            if let Some(drop_fn) = (*s).drop_payload { drop_fn((*s).payload.as_mut_ptr() as *mut c_void); }
            __gost_free(s as *mut c_void, 0, 1);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_shared_get_ptr(s: *mut __gost_shared) -> *mut c_void {
    unsafe { if s.is_null() { ptr::null_mut() } else { (*s).payload.as_mut_ptr() as *mut c_void } }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_shared_is_unique(s: *mut __gost_shared) -> i32 {
    unsafe { if s.is_null() { 0 } else { if (*s).refcount.load(Ordering::Relaxed) == 1 { 1 } else { 0 } } }
}

unsafe fn map_key_equal(m: *mut __gost_map, entry: *mut __gost_map_entry, key_bytes: *const c_void) -> bool {
    if (*entry).used == 0 { return false; }
    match (*m).key_kind {
        1 => {
            let key = *(key_bytes as *const i64);
            (*entry).key.i64v == key
        }
        2 => {
            let key = *(key_bytes as *const u64);
            (*entry).key.u64v == key
        }
        3 => {
            let ptr = *(key_bytes as *const *const u8);
            let len_ptr = (key_bytes as *const u8).add(mem::size_of::<*const u8>()) as *const i64;
            let len = *len_ptr;
            let stored = (*entry).key.strv;
            if stored.len != len { return false; }
            if len == 0 { return true; }
            libc::memcmp(stored.ptr as *const c_void, ptr as *const c_void, len as usize) == 0
        }
        _ => false,
    }
}

unsafe fn map_store_key(m: *mut __gost_map, entry: *mut __gost_map_entry, key_bytes: *const c_void) {
    (*entry).used = 1;
    match (*m).key_kind {
        1 => { (*entry).key.i64v = *(key_bytes as *const i64); }
        2 => { (*entry).key.u64v = *(key_bytes as *const u64); }
        3 => {
            let ptr = *(key_bytes as *const *const u8);
            let len_ptr = (key_bytes as *const u8).add(mem::size_of::<*const u8>()) as *const i64;
            (*entry).key.strv = __gost_map_key_str { ptr, len: *len_ptr };
        }
        _ => {}
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_map_new(key_kind: i32, val_size: usize, cap: i64) -> *mut __gost_map {
    unsafe {
        let mut cap = cap;
        if cap < 0 { cap = 0; }
        let m = __gost_alloc(mem::size_of::<__gost_map>(), 1) as *mut __gost_map;
        (*m).key_kind = key_kind;
        (*m).val_size = val_size;
        (*m).len = 0;
        (*m).cap = if cap > 0 { cap } else { 8 };
        let entries = __gost_alloc(mem::size_of::<__gost_map_entry>() * (*m).cap as usize, 1) as *mut __gost_map_entry;
        ptr::write_bytes(entries, 0, (*m).cap as usize);
        (*m).entries = entries;
        m
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_map_get(m: *mut __gost_map, key_bytes: *const c_void, out_val_bytes: *mut c_void) -> i32 {
    unsafe {
        if m.is_null() { return 0; }
        for i in 0..(*m).len {
            let entry = (*m).entries.add(i as usize);
            if map_key_equal(m, entry, key_bytes) {
                if !out_val_bytes.is_null() && (*m).val_size > 0 {
                    ptr::copy_nonoverlapping((*entry).val, out_val_bytes as *mut u8, (*m).val_size);
                }
                return 1;
            }
        }
        0
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_map_set(m: *mut __gost_map, key_bytes: *const c_void, val_bytes: *const c_void) {
    unsafe {
        if m.is_null() { return; }
        for i in 0..(*m).len {
            let entry = (*m).entries.add(i as usize);
            if map_key_equal(m, entry, key_bytes) {
                if (*m).val_size > 0 && !val_bytes.is_null() {
                    ptr::copy_nonoverlapping(val_bytes as *const u8, (*entry).val, (*m).val_size);
                }
                return;
            }
        }
        if (*m).len == (*m).cap {
            let new_cap = if (*m).cap > 0 { (*m).cap * 2 } else { 8 };
            let new_entries = __gost_alloc(mem::size_of::<__gost_map_entry>() * new_cap as usize, 1) as *mut __gost_map_entry;
            ptr::write_bytes(new_entries, 0, new_cap as usize);
            for i in 0..(*m).len {
                *new_entries.add(i as usize) = *(*m).entries.add(i as usize);
            }
            __gost_free((*m).entries as *mut c_void, 0, 1);
            (*m).entries = new_entries;
            (*m).cap = new_cap;
        }
        let entry = (*m).entries.add((*m).len as usize);
        map_store_key(m, entry, key_bytes);
        if (*m).val_size > 0 {
            (*entry).val = __gost_alloc((*m).val_size, 1) as *mut u8;
            if !val_bytes.is_null() {
                ptr::copy_nonoverlapping(val_bytes as *const u8, (*entry).val, (*m).val_size);
            } else {
                ptr::write_bytes((*entry).val, 0, (*m).val_size);
            }
        } else {
            (*entry).val = ptr::null_mut();
        }
        (*m).len += 1;
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_map_del(m: *mut __gost_map, key_bytes: *const c_void) -> i32 {
    unsafe {
        if m.is_null() { return 0; }
        for i in 0..(*m).len {
            let entry = (*m).entries.add(i as usize);
            if map_key_equal(m, entry, key_bytes) {
                if !(*entry).val.is_null() {
                    __gost_free((*entry).val as *mut c_void, 0, 1);
                }
                if i != (*m).len - 1 {
                    *entry = *(*m).entries.add(((*m).len - 1) as usize);
                }
                (*m).len -= 1;
                return 1;
            }
        }
        0
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_map_len(m: *mut __gost_map) -> i64 {
    unsafe { if m.is_null() { 0 } else { (*m).len } }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_map_drop(m: *mut __gost_map) {
    unsafe {
        if m.is_null() { return; }
        for i in 0..(*m).len {
            let entry = (*m).entries.add(i as usize);
            if !(*entry).val.is_null() {
                __gost_free((*entry).val as *mut c_void, 0, 1);
            }
        }
        if !(*m).entries.is_null() {
            __gost_free((*m).entries as *mut c_void, 0, 1);
        }
        __gost_free(m as *mut c_void, 0, 1);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_after_ms(ms: i64) -> *mut __gost_chan {
    unsafe {
        let ch = __gost_chan_new(0, 1);
        __gost_chan_retain(ch);
        timer_add(ch, ms);
        ch
    }
}

#[cfg(feature = "stats")]
unsafe fn __gost_rt_dump_stats() {
    let mut out = String::new();
    let _ = writeln!(&mut out, "---- GOST STATS ----");
    let g_created = stats_total(&ST_G_CREATED);
    let g_freed = stats_total(&ST_G_FREED);
    let _ = writeln!(
        &mut out,
        "g: created={} freed={} live={} glive={}",
        g_created,
        g_freed,
        g_created - g_freed,
        G_LIVE.load(Ordering::Relaxed)
    );
    let _ = writeln!(
        &mut out,
        "sched: goready={} gopark={} switch={}",
        stats_total(&ST_GOREADY_CALLS),
        stats_total(&ST_GOPARK_CALLS),
        stats_total(&ST_SCHED_SWITCH)
    );
    let _ = writeln!(
        &mut out,
        "runq: push={} pop={} dupe_blocked={}",
        stats_total(&ST_RUNQ_PUSH),
        stats_total(&ST_RUNQ_POP),
        stats_total(&ST_RUNQ_DUPE_BLOCKED)
    );
    let _ = writeln!(
        &mut out,
        "sudog: alloc={} free={} live={}",
        stats_total(&ST_SUDOG_ALLOC),
        stats_total(&ST_SUDOG_FREE),
        stats_total(&ST_SUDOG_ALLOC) - stats_total(&ST_SUDOG_FREE)
    );
    let _ = writeln!(
        &mut out,
        "chan: send(fast={} buf={} block={}) recv(fast={} buf={} block={}) close={}",
        stats_total(&ST_CHAN_SEND_FAST),
        stats_total(&ST_CHAN_SEND_BUF),
        stats_total(&ST_CHAN_SEND_BLOCK),
        stats_total(&ST_CHAN_RECV_FAST),
        stats_total(&ST_CHAN_RECV_BUF),
        stats_total(&ST_CHAN_RECV_BLOCK),
        stats_total(&ST_CHAN_CLOSE)
    );
    let _ = writeln!(
        &mut out,
        "timer: add={} fired={}",
        stats_total(&ST_TIMER_ADD),
        stats_total(&ST_TIMER_FIRED)
    );
    let _ = writeln!(
        &mut out,
        "select: waiter alloc={} free={} node alloc={} free={} notify={} wake={}",
        stats_total(&ST_SELECT_WAITER_ALLOC),
        stats_total(&ST_SELECT_WAITER_FREE),
        stats_total(&ST_SELECT_NODE_ALLOC),
        stats_total(&ST_SELECT_NODE_FREE),
        stats_total(&ST_SELECT_NOTIFY_CALLS),
        stats_total(&ST_SELECT_WAKE)
    );
    let _ = writeln!(
        &mut out,
        "steal: calls={} fail={} take={}",
        stats_total(&ST_STEAL_CALLS),
        stats_total(&ST_STEAL_FAIL),
        stats_total(&ST_STEAL_TAKE)
    );
    let _ = writeln!(
        &mut out,
        "timerheap: push={} pop={} up={} down={} max={}",
        stats_total(&ST_TH_PUSH),
        stats_total(&ST_TH_POP),
        stats_total(&ST_TH_SIFT_UP),
        stats_total(&ST_TH_SIFT_DOWN),
        stats_max_val(&ST_TH_MAXLEN)
    );
    let _ = writeln!(&mut out, "--------------------");
    fd_write_bytes(2, out.as_bytes());
}

#[cfg(not(feature = "stats"))]
unsafe fn __gost_rt_dump_stats() {}
