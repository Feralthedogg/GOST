// #![allow(private_interfaces)]
#![allow(non_camel_case_types, non_snake_case, dead_code, unsafe_op_in_unsafe_fn, static_mut_refs)]
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
#[cfg(feature = "stats")]
use std::fmt::Write;
use std::ffi::c_void;
use std::io::{ErrorKind, Read, Write as IoWrite};
use std::mem::{self, MaybeUninit};
use std::net::{TcpListener, TcpStream, ToSocketAddrs, UdpSocket};
#[cfg(unix)]
use std::os::fd::AsRawFd;
#[cfg(windows)]
use std::os::windows::io::{AsRawSocket, IntoRawSocket};
use std::ptr;
use std::sync::atomic::{AtomicI32, AtomicI64, AtomicPtr, AtomicU32, Ordering};
use std::sync::{Arc, Mutex, Once, OnceLock};
use reqwest::blocking::Client;
use reqwest::header::{HeaderName, HeaderValue};
use reqwest::Method;
use socket2::{Domain, Protocol, SockAddr, Socket, Type};
use tungstenite::stream::MaybeTlsStream;
use tungstenite::{connect as ws_connect, Error as WsError, Message as WsMessage};

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

#[cfg(target_os = "linux")]
use libc::{
    close, epoll_create1, epoll_ctl, epoll_event, epoll_wait, eventfd, fcntl, read, EFD_CLOEXEC,
    EFD_NONBLOCK, EPOLLERR, EPOLLHUP, EPOLLIN, EPOLLOUT, EPOLLRDHUP, EPOLLONESHOT, EPOLL_CLOEXEC,
    EPOLL_CTL_ADD, EPOLL_CTL_DEL, EPOLL_CTL_MOD, F_GETFL, F_SETFL, O_NONBLOCK,
};

#[cfg(target_os = "macos")]
use libc::{
    close, fcntl, pipe, poll, pollfd, read, F_GETFL, F_SETFL, O_NONBLOCK, POLLERR, POLLHUP,
    POLLIN, POLLNVAL, POLLOUT,
};

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
#[repr(C)]
struct WSAPOLLFD {
    fd: usize,
    events: i16,
    revents: i16,
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
    fn GetLastError() -> u32;
    fn CreateMutexW(
        lpMutexAttributes: *mut c_void,
        bInitialOwner: i32,
        lpName: *const u16,
    ) -> *mut c_void;
    fn SetConsoleOutputCP(wCodePageID: u32) -> i32;
    fn SetConsoleCP(wCodePageID: u32) -> i32;
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
    fn ioctlsocket(s: usize, cmd: i32, argp: *mut u32) -> i32;
    fn WSAPoll(fdArray: *mut WSAPOLLFD, fds: u32, timeout: i32) -> i32;
    fn send(s: usize, buf: *const u8, len: i32, flags: i32) -> i32;
    fn recv(s: usize, buf: *mut u8, len: i32, flags: i32) -> i32;
    fn closesocket(s: usize) -> i32;
    fn WSAGetLastError() -> i32;
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
#[cfg(windows)]
const FIONBIO: i32 = 0x8004_667E_u32 as i32;
#[cfg(windows)]
const WS_POLLIN: i16 = 0x0300;
#[cfg(windows)]
const WS_POLLOUT: i16 = 0x0010;
#[cfg(windows)]
const WS_POLLERR: i16 = 0x0001;
#[cfg(windows)]
const WS_POLLHUP: i16 = 0x0002;
#[cfg(windows)]
const WS_POLLNVAL: i16 = 0x0004;
#[cfg(windows)]
const WS_SOCKET_ERROR: i32 = -1;
#[cfg(windows)]
const WSA_EWOULDBLOCK: i32 = 10035;
#[cfg(windows)]
const WSA_EINPROGRESS: i32 = 10036;
#[cfg(windows)]
const WSA_EALREADY: i32 = 10037;

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
    timer_due_batch: usize,
    timer_wake_batch: usize,
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

const PD_WANT_READ: u32 = 1 << 0;
const PD_WANT_WRITE: u32 = 1 << 1;
const PD_ARMED_READ: u32 = 1 << 2;
const PD_ARMED_WRITE: u32 = 1 << 3;

#[repr(C)]
pub struct gost_poll_desc {
    pub fd: i32,
    handle: usize,
    r_wait: AtomicPtr<gost_g>,
    w_wait: AtomicPtr<gost_g>,
    state: AtomicU32,
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
    strong: AtomicI64,
    weak: AtomicI64,
    drop_payload: Option<extern "C" fn(*mut c_void)>,
    payload: [u8; 0],
}

#[repr(C)]
struct __gost_weak {
    inner: *mut __gost_shared,
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
#[cfg(windows)]
static mut G_SINGLETON_MUTEX: *mut c_void = ptr::null_mut();

thread_local! {
    static TLS_M: Cell<*mut gost_m> = Cell::new(ptr::null_mut());
    static TLS_G: Cell<*mut gost_g> = Cell::new(ptr::null_mut());
    static NET_LAST_STATUS: Cell<i32> = Cell::new(0);
    static NET_LAST_HTTP_STATUS: Cell<i32> = Cell::new(0);
    static NET_LAST_ERR: RefCell<String> = RefCell::new(String::new());
    static NET_LAST_PEER: RefCell<String> = RefCell::new(String::new());
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
const ST_TIMER_CANCELED_EXIT: StatSlot = StatSlot(19);
const ST_SELECT_WAITER_ALLOC: StatSlot = StatSlot(20);
const ST_SELECT_WAITER_FREE: StatSlot = StatSlot(21);
const ST_SELECT_NODE_ALLOC: StatSlot = StatSlot(22);
const ST_SELECT_NODE_FREE: StatSlot = StatSlot(23);
const ST_SELECT_NOTIFY_CALLS: StatSlot = StatSlot(24);
const ST_SELECT_WAKE: StatSlot = StatSlot(25);
const ST_STEAL_CALLS: StatSlot = StatSlot(26);
const ST_STEAL_FAIL: StatSlot = StatSlot(27);
const ST_STEAL_TAKE: StatSlot = StatSlot(28);
const ST_TH_PUSH: StatSlot = StatSlot(29);
const ST_TH_POP: StatSlot = StatSlot(30);
const ST_TH_SIFT_UP: StatSlot = StatSlot(31);
const ST_TH_SIFT_DOWN: StatSlot = StatSlot(32);
const ST_TH_MAXLEN: StatSlot = StatSlot(33);

const STAT_COUNT: usize = 34;

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
static NETPOLL_WAITER: AtomicI32 = AtomicI32::new(0);
static NET_HANDLE_NEXT: AtomicI64 = AtomicI64::new(1);
static NET_HANDLE_TABLE: OnceLock<Mutex<HashMap<i64, NetHandle>>> = OnceLock::new();
static NET_HTTP_CLIENT: OnceLock<Result<Client, String>> = OnceLock::new();
static CHAN_REGISTRY: OnceLock<Mutex<HashSet<usize>>> = OnceLock::new();

type GostWs = tungstenite::WebSocket<MaybeTlsStream<TcpStream>>;

enum NetHandle {
    TcpListener {
        sock: Arc<TcpListener>,
        pd: usize,
    },
    TcpStream {
        sock: Arc<TcpStream>,
        pd: usize,
    },
    UdpSocket {
        sock: Arc<UdpSocket>,
        pd: usize,
    },
    WsSocket {
        ws: Arc<Mutex<GostWs>>,
        pd: usize,
    },
}

#[cfg(any(target_os = "linux", target_os = "macos", windows))]
const NETPOLL_MAX_EVENTS: usize = 64;
#[cfg(any(target_os = "linux", target_os = "macos", windows))]
static NETPOLL_INIT_ONCE: Once = Once::new();
#[cfg(any(target_os = "linux", target_os = "macos", windows))]
static NETPOLL_ENABLED: AtomicI32 = AtomicI32::new(0);

#[cfg(target_os = "linux")]
const NETPOLL_BREAK_TAG: u64 = 1;
#[cfg(target_os = "linux")]
static mut NETPOLL_EPFD: i32 = -1;
#[cfg(target_os = "linux")]
static mut NETPOLL_BREAKFD: i32 = -1;

#[cfg(target_os = "macos")]
static mut NETPOLL_BREAK_RFD: i32 = -1;
#[cfg(target_os = "macos")]
static mut NETPOLL_BREAK_WFD: i32 = -1;

#[cfg(windows)]
static mut NETPOLL_BREAK_RSOCK: usize = 0;
#[cfg(windows)]
static mut NETPOLL_BREAK_WSOCK: usize = 0;

#[cfg(any(target_os = "macos", windows))]
static NETPOLL_REG: OnceLock<Mutex<HashSet<usize>>> = OnceLock::new();

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

fn env_usize_clamp(name: &str, defv: usize, lo: usize, hi: usize) -> usize {
    if let Ok(val) = std::env::var(name) {
        if let Ok(v) = val.parse::<usize>() {
            if v > 0 {
                return v.clamp(lo, hi);
            }
        }
    }
    defv
}

fn net_table() -> &'static Mutex<HashMap<i64, NetHandle>> {
    NET_HANDLE_TABLE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn net_table_lock() -> std::sync::MutexGuard<'static, HashMap<i64, NetHandle>> {
    match net_table().lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    }
}

#[cfg(windows)]
unsafe fn os_console_utf8_init() {
    const CP_UTF8: u32 = 65001;
    let _ = SetConsoleOutputCP(CP_UTF8);
    let _ = SetConsoleCP(CP_UTF8);
}

#[cfg(not(windows))]
unsafe fn os_console_utf8_init() {}

fn chan_registry() -> &'static Mutex<HashSet<usize>> {
    CHAN_REGISTRY.get_or_init(|| Mutex::new(HashSet::new()))
}

fn chan_register(ch: *mut __gost_chan) {
    if ch.is_null() {
        return;
    }
    let mut reg = match chan_registry().lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    reg.insert(ch as usize);
}

fn chan_unregister(ch: *mut __gost_chan) {
    if ch.is_null() {
        return;
    }
    let mut reg = match chan_registry().lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    reg.remove(&(ch as usize));
}

fn chan_is_registered(ch: *mut __gost_chan) -> bool {
    if ch.is_null() {
        return false;
    }
    let p = ch as usize;
    if p < 4096 {
        return false;
    }
    let align = mem::align_of::<__gost_chan>();
    if align > 1 && (p & (align - 1)) != 0 {
        return false;
    }
    let reg = match chan_registry().lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    reg.contains(&p)
}

fn net_set_ok() {
    NET_LAST_STATUS.with(|v| v.set(0));
    NET_LAST_ERR.with(|v| v.borrow_mut().clear());
}

fn net_set_eof() {
    NET_LAST_STATUS.with(|v| v.set(1));
    NET_LAST_ERR.with(|v| v.borrow_mut().clear());
}

fn net_set_would_block() {
    NET_LAST_STATUS.with(|v| v.set(2));
    NET_LAST_ERR.with(|v| v.borrow_mut().clear());
}

fn net_set_err(msg: String) {
    NET_LAST_STATUS.with(|v| v.set(-1));
    NET_LAST_ERR.with(|v| *v.borrow_mut() = msg);
}

fn net_set_http_status(code: i32) {
    NET_LAST_HTTP_STATUS.with(|v| v.set(code));
}

fn net_set_last_peer(peer: String) {
    NET_LAST_PEER.with(|v| *v.borrow_mut() = peer);
}

fn net_clear_last_peer() {
    NET_LAST_PEER.with(|v| v.borrow_mut().clear());
}

unsafe fn raw_bytes_from<'a>(ptr: *const u8, len: i64) -> Result<&'a [u8], String> {
    if len < 0 {
        return Err("negative length".to_string());
    }
    if ptr.is_null() && len != 0 {
        return Err("null pointer with non-zero length".to_string());
    }
    if len == 0 {
        return Ok(&[]);
    }
    Ok(std::slice::from_raw_parts(ptr, len as usize))
}

unsafe fn raw_utf8_from(ptr: *const u8, len: i64) -> Result<String, String> {
    let b = raw_bytes_from(ptr, len)?;
    match std::str::from_utf8(b) {
        Ok(s) => Ok(s.to_string()),
        Err(_) => Err("invalid utf-8 string".to_string()),
    }
}

unsafe fn gost_string_set_bytes(out: *mut gost_string, bytes: &[u8]) {
    if out.is_null() {
        return;
    }
    if bytes.is_empty() {
        (*out).ptr = ptr::null();
        (*out).len = 0;
        return;
    }
    let dst = __gost_alloc(bytes.len(), 1) as *mut u8;
    ptr::copy_nonoverlapping(bytes.as_ptr(), dst, bytes.len());
    (*out).ptr = dst as *const u8;
    (*out).len = bytes.len() as i64;
}

fn net_insert_handle(h: NetHandle) -> i64 {
    let id = NET_HANDLE_NEXT.fetch_add(1, Ordering::Relaxed);
    let mut table = net_table_lock();
    table.insert(id, h);
    id
}

unsafe fn net_handle_close_poll(h: &NetHandle) {
    match h {
        NetHandle::TcpListener { pd, .. } => {
            let p = *pd as *mut gost_poll_desc;
            if !p.is_null() {
                __gost_poll_close(p);
            }
        }
        NetHandle::TcpStream { pd, .. } => {
            let p = *pd as *mut gost_poll_desc;
            if !p.is_null() {
                __gost_poll_close(p);
            }
        }
        NetHandle::UdpSocket { pd, .. } => {
            let p = *pd as *mut gost_poll_desc;
            if !p.is_null() {
                __gost_poll_close(p);
            }
        }
        NetHandle::WsSocket { pd, .. } => {
            let p = *pd as *mut gost_poll_desc;
            if !p.is_null() {
                __gost_poll_close(p);
            }
        }
    }
}

fn net_remove_handle(id: i64) -> bool {
    let mut table = net_table_lock();
    let removed = table.remove(&id);
    drop(table);
    if let Some(h) = removed {
        unsafe { net_handle_close_poll(&h); }
        true
    } else {
        false
    }
}

#[cfg(any(target_os = "macos", windows))]
fn netpoll_reg_lock() -> std::sync::MutexGuard<'static, HashSet<usize>> {
    match NETPOLL_REG.get_or_init(|| Mutex::new(HashSet::new())).lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    }
}

#[cfg(any(target_os = "macos", windows))]
unsafe fn netpoll_reg_add(pd: *mut gost_poll_desc) {
    if pd.is_null() {
        return;
    }
    let key = pd as usize;
    let mut reg = netpoll_reg_lock();
    reg.insert(key);
}

#[cfg(any(target_os = "macos", windows))]
unsafe fn netpoll_reg_remove(pd: *mut gost_poll_desc) {
    if pd.is_null() {
        return;
    }
    let key = pd as usize;
    let mut reg = netpoll_reg_lock();
    reg.remove(&key);
}

#[cfg(any(target_os = "macos", windows))]
unsafe fn netpoll_reg_contains(pd: *mut gost_poll_desc) -> bool {
    if pd.is_null() {
        return false;
    }
    let key = pd as usize;
    let reg = netpoll_reg_lock();
    reg.contains(&key)
}

#[cfg(unix)]
fn net_open_pd_listener(sock: &TcpListener) -> *mut gost_poll_desc {
    unsafe { __gost_poll_open(sock.as_raw_fd() as i32) }
}

#[cfg(windows)]
fn net_open_pd_listener(sock: &TcpListener) -> *mut gost_poll_desc {
    __gost_poll_open_socket(sock.as_raw_socket() as u64)
}

#[cfg(not(any(unix, windows)))]
fn net_open_pd_listener(_sock: &TcpListener) -> *mut gost_poll_desc {
    ptr::null_mut()
}

#[cfg(unix)]
fn net_open_pd_stream(sock: &TcpStream) -> *mut gost_poll_desc {
    unsafe { __gost_poll_open(sock.as_raw_fd() as i32) }
}

#[cfg(windows)]
fn net_open_pd_stream(sock: &TcpStream) -> *mut gost_poll_desc {
    __gost_poll_open_socket(sock.as_raw_socket() as u64)
}

#[cfg(not(any(unix, windows)))]
fn net_open_pd_stream(_sock: &TcpStream) -> *mut gost_poll_desc {
    ptr::null_mut()
}

#[cfg(unix)]
fn net_open_pd_udp(sock: &UdpSocket) -> *mut gost_poll_desc {
    unsafe { __gost_poll_open(sock.as_raw_fd() as i32) }
}

#[cfg(windows)]
fn net_open_pd_udp(sock: &UdpSocket) -> *mut gost_poll_desc {
    __gost_poll_open_socket(sock.as_raw_socket() as u64)
}

#[cfg(not(any(unix, windows)))]
fn net_open_pd_udp(_sock: &UdpSocket) -> *mut gost_poll_desc {
    ptr::null_mut()
}

fn net_get_tcp_listener(id: i64) -> Result<(Arc<TcpListener>, *mut gost_poll_desc), String> {
    let table = net_table_lock();
    match table.get(&id) {
        Some(NetHandle::TcpListener { sock, pd }) => Ok((sock.clone(), *pd as *mut gost_poll_desc)),
        Some(_) => Err("handle is not tcp listener".to_string()),
        None => Err("invalid handle".to_string()),
    }
}

fn net_get_tcp_stream(id: i64) -> Result<(Arc<TcpStream>, *mut gost_poll_desc), String> {
    let table = net_table_lock();
    match table.get(&id) {
        Some(NetHandle::TcpStream { sock, pd }) => Ok((sock.clone(), *pd as *mut gost_poll_desc)),
        Some(_) => Err("handle is not tcp connection".to_string()),
        None => Err("invalid handle".to_string()),
    }
}

fn net_get_udp_socket(id: i64) -> Result<(Arc<UdpSocket>, *mut gost_poll_desc), String> {
    let table = net_table_lock();
    match table.get(&id) {
        Some(NetHandle::UdpSocket { sock, pd }) => Ok((sock.clone(), *pd as *mut gost_poll_desc)),
        Some(_) => Err("handle is not udp socket".to_string()),
        None => Err("invalid handle".to_string()),
    }
}

fn net_get_ws(id: i64) -> Result<(Arc<Mutex<GostWs>>, *mut gost_poll_desc), String> {
    let table = net_table_lock();
    match table.get(&id) {
        Some(NetHandle::WsSocket { ws, pd }) => Ok((ws.clone(), *pd as *mut gost_poll_desc)),
        Some(_) => Err("handle is not websocket".to_string()),
        None => Err("invalid handle".to_string()),
    }
}

fn ws_set_nonblocking(ws: &mut GostWs, on: bool) -> Result<(), String> {
    match ws.get_mut() {
        MaybeTlsStream::Plain(s) => s
            .set_nonblocking(on)
            .map_err(|e| format!("ws set nonblocking failed: {}", e)),
        #[cfg(windows)]
        MaybeTlsStream::NativeTls(s) => s
            .get_mut()
            .set_nonblocking(on)
            .map_err(|e| format!("ws set nonblocking failed: {}", e)),
        #[cfg(not(windows))]
        MaybeTlsStream::Rustls(s) => s
            .get_mut()
            .set_nonblocking(on)
            .map_err(|e| format!("ws set nonblocking failed: {}", e)),
        #[allow(unreachable_patterns)]
        _ => Err("unsupported websocket stream".to_string()),
    }
}

fn ws_open_pd(ws: &mut GostWs) -> *mut gost_poll_desc {
    match ws.get_mut() {
        MaybeTlsStream::Plain(s) => net_open_pd_stream(s),
        #[cfg(windows)]
        MaybeTlsStream::NativeTls(s) => net_open_pd_stream(s.get_mut()),
        #[cfg(not(windows))]
        MaybeTlsStream::Rustls(s) => net_open_pd_stream(s.get_mut()),
        #[allow(unreachable_patterns)]
        _ => ptr::null_mut(),
    }
}

fn net_http_client() -> Result<&'static Client, String> {
    match NET_HTTP_CLIENT.get_or_init(|| {
        Client::builder()
            .build()
            .map_err(|e| format!("http client build failed: {}", e))
    }) {
        Ok(c) => Ok(c),
        Err(e) => Err(e.clone()),
    }
}

#[inline(always)]
fn net_tune_tcp_stream(stream: &TcpStream) {
    let _ = stream.set_nodelay(true);
}

fn net_is_connect_in_progress(err: &std::io::Error) -> bool {
    if err.kind() == ErrorKind::WouldBlock {
        return true;
    }
    match err.raw_os_error() {
        #[cfg(unix)]
        Some(libc::EINPROGRESS) | Some(libc::EALREADY) | Some(libc::EWOULDBLOCK) => true,
        #[cfg(windows)]
        Some(WSA_EWOULDBLOCK) | Some(WSA_EINPROGRESS) | Some(WSA_EALREADY) => true,
        _ => false,
    }
}

fn net_tcp_connect_blocking(addr: &str) -> Result<(TcpStream, *mut gost_poll_desc), String> {
    let stream = TcpStream::connect(addr).map_err(|e| format!("tcp connect failed: {}", e))?;
    net_tune_tcp_stream(&stream);
    let pd = net_open_pd_stream(&stream);
    Ok((stream, pd))
}

fn net_tcp_connect_nonblocking(addr: &str) -> Result<(TcpStream, *mut gost_poll_desc), String> {
    let addrs: Vec<std::net::SocketAddr> = addr
        .to_socket_addrs()
        .map_err(|e| format!("tcp resolve failed: {}", e))?
        .collect();
    if addrs.is_empty() {
        return Err("tcp connect failed: no resolved address".to_string());
    }

    let mut last_err: Option<String> = None;
    for sa in addrs {
        let domain = Domain::for_address(sa);
        let socket = match Socket::new(domain, Type::STREAM, Some(Protocol::TCP)) {
            Ok(s) => s,
            Err(e) => {
                last_err = Some(format!("tcp socket create failed: {}", e));
                continue;
            }
        };
        if let Err(e) = socket.set_nonblocking(true) {
            last_err = Some(format!("tcp set nonblocking failed: {}", e));
            continue;
        }
        let sa = SockAddr::from(sa);
        match socket.connect(&sa) {
            Ok(()) => {
                let stream: TcpStream = socket.into();
                net_tune_tcp_stream(&stream);
                let pd = net_open_pd_stream(&stream);
                return Ok((stream, pd));
            }
            Err(e) if net_is_connect_in_progress(&e) => {
                let stream: TcpStream = socket.into();
                net_tune_tcp_stream(&stream);
                let pd = net_open_pd_stream(&stream);
                if pd.is_null() {
                    last_err = Some("tcp connect failed: netpoll unavailable".to_string());
                    continue;
                }
                loop {
                    match unsafe { net_wait_write(pd) } {
                        Ok(()) => {}
                        Err(msg) => {
                            __gost_poll_close(pd);
                            return Err(msg);
                        }
                    }
                    match stream.take_error() {
                        Ok(None) => return Ok((stream, pd)),
                        Ok(Some(se)) if net_is_connect_in_progress(&se) => continue,
                        Ok(Some(se)) => {
                            __gost_poll_close(pd);
                            last_err = Some(format!("tcp connect failed: {}", se));
                            break;
                        }
                        Err(se) => {
                            __gost_poll_close(pd);
                            last_err = Some(format!("tcp connect failed: {}", se));
                            break;
                        }
                    }
                }
            }
            Err(e) => {
                last_err = Some(format!("tcp connect failed: {}", e));
            }
        }
    }
    Err(last_err.unwrap_or_else(|| "tcp connect failed".to_string()))
}

fn net_http_request_impl(
    method: &str,
    url: &str,
    body: &[u8],
    content_type: &str,
    headers: &str,
) -> Result<(i32, Vec<u8>), String> {
    let m = if method.is_empty() { "GET" } else { method };
    let method = Method::from_bytes(m.as_bytes())
        .map_err(|e| format!("invalid http method '{}': {}", m, e))?;
    let client = net_http_client()?;
    let mut req = client.request(method.clone(), url);
    if !content_type.is_empty() {
        req = req.header("Content-Type", content_type);
    }
    if !headers.is_empty() {
        for raw in headers.split('\n') {
            let line = raw.trim();
            if line.is_empty() {
                continue;
            }
            let mut parts = line.splitn(2, ':');
            let key = parts.next().unwrap_or("").trim();
            let val = parts.next().unwrap_or("").trim();
            if key.is_empty() {
                return Err("http header name is empty".to_string());
            }
            if val.is_empty() {
                return Err(format!("http header '{}' has empty value", key));
            }
            let hn = HeaderName::from_bytes(key.as_bytes())
                .map_err(|e| format!("invalid http header name '{}': {}", key, e))?;
            let hv = HeaderValue::from_str(val)
                .map_err(|e| format!("invalid http header value for '{}': {}", key, e))?;
            req = req.header(hn, hv);
        }
    }

    let send_body = !body.is_empty() || !(method == Method::GET || method == Method::HEAD);
    let resp = if send_body {
        req.body(body.to_vec())
            .send()
            .map_err(|e| format!("http transport error: {}", e))?
    } else {
        req.send().map_err(|e| format!("http transport error: {}", e))?
    };

    let status = resp.status().as_u16() as i32;
    let payload = resp
        .bytes()
        .map_err(|e| format!("http read failed: {}", e))?
        .to_vec();
    Ok((status, payload))
}

#[cfg(any(target_os = "linux", target_os = "macos", windows))]
#[inline(always)]
fn netpoll_enabled() -> bool {
    NETPOLL_ENABLED.load(Ordering::Acquire) != 0
}

#[cfg(not(any(target_os = "linux", target_os = "macos", windows)))]
#[inline(always)]
fn netpoll_enabled() -> bool {
    false
}

#[cfg(unix)]
unsafe fn netpoll_set_nonblock(fd: i32) -> i32 {
    if fd < 0 {
        return -1;
    }
    let flags = fcntl(fd, F_GETFL);
    if flags < 0 {
        return -1;
    }
    if (flags & O_NONBLOCK) != 0 {
        return 0;
    }
    if fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0 {
        return -1;
    }
    0
}

#[cfg(windows)]
unsafe fn netpoll_set_nonblock_socket(sock: usize) -> i32 {
    if sock == 0 || sock == usize::MAX {
        return -1;
    }
    let mut on: u32 = 1;
    if ioctlsocket(sock, FIONBIO, &mut on as *mut u32) != 0 {
        return -1;
    }
    0
}

#[cfg(not(any(unix, windows)))]
unsafe fn netpoll_set_nonblock(_fd: i32) -> i32 {
    0
}

#[cfg(target_os = "linux")]
unsafe fn netpoll_init() {
    NETPOLL_INIT_ONCE.call_once(|| unsafe {
        let epfd = epoll_create1(EPOLL_CLOEXEC);
        if epfd < 0 {
            return;
        }

        let breakfd = eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK);
        if breakfd < 0 {
            let _ = close(epfd);
            return;
        }

        let mut ev: epoll_event = mem::zeroed();
        ev.events = (EPOLLIN | EPOLLERR | EPOLLHUP) as u32;
        ev.u64 = NETPOLL_BREAK_TAG;
        if epoll_ctl(epfd, EPOLL_CTL_ADD, breakfd, &mut ev as *mut epoll_event) != 0 {
            let _ = close(breakfd);
            let _ = close(epfd);
            return;
        }

        NETPOLL_EPFD = epfd;
        NETPOLL_BREAKFD = breakfd;
        NETPOLL_ENABLED.store(1, Ordering::Release);
    });
}

#[cfg(target_os = "macos")]
unsafe fn netpoll_init() {
    NETPOLL_INIT_ONCE.call_once(|| unsafe {
        let mut fds: [i32; 2] = [0; 2];
        if pipe(fds.as_mut_ptr()) != 0 {
            return;
        }
        if netpoll_set_nonblock(fds[0]) != 0 || netpoll_set_nonblock(fds[1]) != 0 {
            let _ = close(fds[0]);
            let _ = close(fds[1]);
            return;
        }
        NETPOLL_BREAK_RFD = fds[0];
        NETPOLL_BREAK_WFD = fds[1];
        {
            let mut reg = netpoll_reg_lock();
            reg.clear();
        }
        NETPOLL_ENABLED.store(1, Ordering::Release);
    });
}

#[cfg(windows)]
unsafe fn netpoll_make_break_pair() -> Option<(usize, usize)> {
    let listener = TcpListener::bind("127.0.0.1:0").ok()?;
    let addr = listener.local_addr().ok()?;
    let writer = TcpStream::connect(addr).ok()?;
    let (reader, _) = listener.accept().ok()?;
    let rsock = reader.into_raw_socket() as usize;
    let wsock = writer.into_raw_socket() as usize;
    Some((rsock, wsock))
}

#[cfg(windows)]
unsafe fn netpoll_init() {
    NETPOLL_INIT_ONCE.call_once(|| unsafe {
        let Some((rsock, wsock)) = netpoll_make_break_pair() else {
            return;
        };
        if netpoll_set_nonblock_socket(rsock) != 0 || netpoll_set_nonblock_socket(wsock) != 0 {
            let _ = closesocket(rsock);
            let _ = closesocket(wsock);
            return;
        }
        NETPOLL_BREAK_RSOCK = rsock;
        NETPOLL_BREAK_WSOCK = wsock;
        {
            let mut reg = netpoll_reg_lock();
            reg.clear();
        }
        NETPOLL_ENABLED.store(1, Ordering::Release);
    });
}

#[cfg(not(any(target_os = "linux", target_os = "macos", windows)))]
unsafe fn netpoll_init() {}

#[cfg(target_os = "linux")]
#[inline(always)]
unsafe fn netpoll_wake_readable(flags: i32) -> bool {
    (flags & (EPOLLIN | EPOLLERR | EPOLLHUP | EPOLLRDHUP)) != 0
}

#[cfg(target_os = "linux")]
#[inline(always)]
unsafe fn netpoll_wake_writable(flags: i32) -> bool {
    (flags & (EPOLLOUT | EPOLLERR | EPOLLHUP)) != 0
}

#[cfg(target_os = "linux")]
unsafe fn netpoll_drain_breakfd() {
    if NETPOLL_BREAKFD < 0 {
        return;
    }
    let mut val: u64 = 0;
    loop {
        let n = read(
            NETPOLL_BREAKFD,
            (&mut val as *mut u64).cast::<c_void>(),
            mem::size_of::<u64>(),
        );
        if n < mem::size_of::<u64>() as isize {
            break;
        }
    }
}

#[cfg(target_os = "macos")]
unsafe fn netpoll_drain_breakfd() {
    if NETPOLL_BREAK_RFD < 0 {
        return;
    }
    let mut buf = [0u8; 128];
    loop {
        let n = read(
            NETPOLL_BREAK_RFD,
            buf.as_mut_ptr().cast::<c_void>(),
            buf.len(),
        );
        if n <= 0 || (n as usize) < buf.len() {
            break;
        }
    }
}

#[cfg(windows)]
unsafe fn netpoll_drain_breakfd() {
    if NETPOLL_BREAK_RSOCK == 0 {
        return;
    }
    let mut buf = [0u8; 128];
    loop {
        let n = recv(NETPOLL_BREAK_RSOCK, buf.as_mut_ptr(), buf.len() as i32, 0);
        if n > 0 {
            if (n as usize) < buf.len() {
                break;
            }
            continue;
        }
        if n == 0 {
            break;
        }
        let err = WSAGetLastError();
        if err == WSA_EWOULDBLOCK {
            break;
        }
        break;
    }
}

#[cfg(target_os = "linux")]
unsafe fn netpoll_break() {
    if !netpoll_enabled() || NETPOLL_BREAKFD < 0 {
        return;
    }
    let one: u64 = 1;
    let _ = write(
        NETPOLL_BREAKFD,
        (&one as *const u64).cast::<c_void>(),
        mem::size_of::<u64>(),
    );
}

#[cfg(target_os = "macos")]
unsafe fn netpoll_break() {
    if !netpoll_enabled() || NETPOLL_BREAK_WFD < 0 {
        return;
    }
    let one: [u8; 1] = [1];
    let _ = write(
        NETPOLL_BREAK_WFD,
        one.as_ptr().cast::<c_void>(),
        one.len(),
    );
}

#[cfg(windows)]
unsafe fn netpoll_break() {
    if !netpoll_enabled() || NETPOLL_BREAK_WSOCK == 0 {
        return;
    }
    let one = [1u8; 1];
    let _ = send(NETPOLL_BREAK_WSOCK, one.as_ptr(), 1, 0);
}

#[cfg(not(any(target_os = "linux", target_os = "macos", windows)))]
unsafe fn netpoll_break() {}

#[cfg(target_os = "linux")]
#[inline(always)]
unsafe fn netpoll_epoll_events(want: u32) -> u32 {
    let mut events = (EPOLLERR | EPOLLHUP | EPOLLRDHUP | EPOLLONESHOT) as u32;
    if (want & PD_WANT_READ) != 0 {
        events |= EPOLLIN as u32;
    }
    if (want & PD_WANT_WRITE) != 0 {
        events |= EPOLLOUT as u32;
    }
    events
}

#[cfg(target_os = "linux")]
unsafe fn netpoll_arm(pd: *mut gost_poll_desc, want: u32) -> i32 {
    if pd.is_null() {
        return -1;
    }
    if !netpoll_enabled() {
        return -1;
    }
    let fd = (*pd).fd;
    if fd < 0 {
        return -1;
    }
    if want == 0 {
        (*pd).state.store(0, Ordering::Release);
        return 0;
    }

    let mut ev: epoll_event = mem::zeroed();
    ev.events = netpoll_epoll_events(want);
    ev.u64 = pd as u64;

    let epfd = NETPOLL_EPFD;
    if epfd < 0 {
        return -1;
    }

    let mut rc = epoll_ctl(epfd, EPOLL_CTL_MOD, fd, &mut ev as *mut epoll_event);
    if rc != 0 {
        rc = epoll_ctl(epfd, EPOLL_CTL_ADD, fd, &mut ev as *mut epoll_event);
        if rc != 0 {
            return -1;
        }
    }

    let mut state = want;
    if (want & PD_WANT_READ) != 0 {
        state |= PD_ARMED_READ;
    }
    if (want & PD_WANT_WRITE) != 0 {
        state |= PD_ARMED_WRITE;
    }
    (*pd).state.store(state, Ordering::Release);
    0
}

#[cfg(target_os = "macos")]
unsafe fn netpoll_arm(pd: *mut gost_poll_desc, want: u32) -> i32 {
    if pd.is_null() || !netpoll_enabled() {
        return -1;
    }
    let fd = (*pd).handle as i32;
    if fd < 0 {
        return -1;
    }
    if want == 0 {
        (*pd).state.store(0, Ordering::Release);
        netpoll_reg_remove(pd);
        return 0;
    }
    let mut state = want;
    if (want & PD_WANT_READ) != 0 {
        state |= PD_ARMED_READ;
    }
    if (want & PD_WANT_WRITE) != 0 {
        state |= PD_ARMED_WRITE;
    }
    (*pd).state.store(state, Ordering::Release);
    netpoll_reg_add(pd);
    0
}

#[cfg(windows)]
unsafe fn netpoll_arm(pd: *mut gost_poll_desc, want: u32) -> i32 {
    if pd.is_null() || !netpoll_enabled() {
        return -1;
    }
    let sock = (*pd).handle;
    if sock == 0 || sock == usize::MAX {
        return -1;
    }
    if want == 0 {
        (*pd).state.store(0, Ordering::Release);
        netpoll_reg_remove(pd);
        return 0;
    }
    let mut state = want;
    if (want & PD_WANT_READ) != 0 {
        state |= PD_ARMED_READ;
    }
    if (want & PD_WANT_WRITE) != 0 {
        state |= PD_ARMED_WRITE;
    }
    (*pd).state.store(state, Ordering::Release);
    netpoll_reg_add(pd);
    0
}

#[cfg(not(any(target_os = "linux", target_os = "macos", windows)))]
unsafe fn netpoll_arm(_pd: *mut gost_poll_desc, _want: u32) -> i32 {
    -1
}

unsafe fn netpoll_waiters_want(pd: *mut gost_poll_desc) -> u32 {
    if pd.is_null() {
        return 0;
    }
    let mut want = 0u32;
    if !(*pd).r_wait.load(Ordering::Acquire).is_null() {
        want |= PD_WANT_READ;
    }
    if !(*pd).w_wait.load(Ordering::Acquire).is_null() {
        want |= PD_WANT_WRITE;
    }
    want
}

unsafe fn netpoll_wait_slot(
    pd: *mut gost_poll_desc,
    slot: &AtomicPtr<gost_g>,
    want: u32,
) -> Result<(), String> {
    if pd.is_null() {
        return Err("poll descriptor is null".to_string());
    }
    let g = tls_g_get();
    if g.is_null() {
        return Err("netpoll wait requires runtime goroutine context".to_string());
    }
    if slot
        .compare_exchange(ptr::null_mut(), g, Ordering::AcqRel, Ordering::Acquire)
        .is_err()
    {
        return Err("concurrent waiters for same fd direction are not supported".to_string());
    }

    // Commit parking state before arming so an immediate wake cannot be lost.
    let sched = g_sched_mut();
    os_mutex_lock(&mut sched.mu);
    (*g).park_ready = 0;
    (*g).state = G_PARKING;
    os_mutex_unlock(&mut sched.mu);

    let arm_want = netpoll_waiters_want(pd) | want;
    if netpoll_arm(pd, arm_want) != 0 {
        slot.store(ptr::null_mut(), Ordering::Release);
        let sched = g_sched_mut();
        os_mutex_lock(&mut sched.mu);
        if (*g).state == G_PARKING && (*g).park_ready == 0 {
            (*g).state = G_RUNNING;
        }
        os_mutex_unlock(&mut sched.mu);
        return Err("netpoll arm failed".to_string());
    }

    gopark_committed();
    let _ = slot.compare_exchange(g, ptr::null_mut(), Ordering::AcqRel, Ordering::Acquire);
    Ok(())
}

unsafe fn netpoll_wait_read(pd: *mut gost_poll_desc) -> Result<(), String> {
    if pd.is_null() {
        return Err("netpoll unavailable".to_string());
    }
    netpoll_wait_slot(pd, &(*pd).r_wait, PD_WANT_READ)
}

unsafe fn netpoll_wait_write(pd: *mut gost_poll_desc) -> Result<(), String> {
    if pd.is_null() {
        return Err("netpoll unavailable".to_string());
    }
    netpoll_wait_slot(pd, &(*pd).w_wait, PD_WANT_WRITE)
}

#[cfg(any(target_os = "linux", target_os = "macos", windows))]
unsafe fn net_wait_read(pd: *mut gost_poll_desc) -> Result<(), String> {
    if pd.is_null() {
        std::thread::yield_now();
        return Ok(());
    }
    netpoll_wait_read(pd)
}

#[cfg(not(any(target_os = "linux", target_os = "macos", windows)))]
unsafe fn net_wait_read(_pd: *mut gost_poll_desc) -> Result<(), String> {
    std::thread::yield_now();
    Ok(())
}

#[cfg(any(target_os = "linux", target_os = "macos", windows))]
unsafe fn net_wait_write(pd: *mut gost_poll_desc) -> Result<(), String> {
    if pd.is_null() {
        std::thread::yield_now();
        return Ok(());
    }
    netpoll_wait_write(pd)
}

#[cfg(not(any(target_os = "linux", target_os = "macos", windows)))]
unsafe fn net_wait_write(_pd: *mut gost_poll_desc) -> Result<(), String> {
    std::thread::yield_now();
    Ok(())
}

#[cfg(target_os = "linux")]
unsafe fn netpoll_wait_collect(ready: &mut Vec<*mut gost_g>, wait_ms: i64) {
    if !netpoll_enabled() || NETPOLL_EPFD < 0 {
        return;
    }
    let timeout = if wait_ms < 0 {
        -1
    } else if wait_ms > i32::MAX as i64 {
        i32::MAX
    } else {
        wait_ms as i32
    };

    let mut events: [epoll_event; NETPOLL_MAX_EVENTS] = mem::zeroed();
    let n = epoll_wait(
        NETPOLL_EPFD,
        events.as_mut_ptr(),
        NETPOLL_MAX_EVENTS as i32,
        timeout,
    );
    if n <= 0 {
        return;
    }
    for i in 0..(n as usize) {
        let ev = events[i];
        if ev.u64 == NETPOLL_BREAK_TAG {
            netpoll_drain_breakfd();
            continue;
        }
        let pd = ev.u64 as *mut gost_poll_desc;
        if pd.is_null() {
            continue;
        }
        let flags = ev.events as i32;
        let mut clear_mask = 0u32;
        if netpoll_wake_readable(flags) {
            clear_mask |= PD_ARMED_READ;
            let g = (*pd).r_wait.swap(ptr::null_mut(), Ordering::AcqRel);
            if !g.is_null() {
                ready.push(g);
            }
        }
        if netpoll_wake_writable(flags) {
            clear_mask |= PD_ARMED_WRITE;
            let g = (*pd).w_wait.swap(ptr::null_mut(), Ordering::AcqRel);
            if !g.is_null() {
                ready.push(g);
            }
        }
        if clear_mask != 0 {
            (*pd).state.fetch_and(!clear_mask, Ordering::AcqRel);
        }
        let rearm_want = netpoll_waiters_want(pd);
        if rearm_want != 0 {
            let _ = netpoll_arm(pd, rearm_want);
        }
    }
}

#[cfg(target_os = "macos")]
unsafe fn netpoll_wait_collect(ready: &mut Vec<*mut gost_g>, wait_ms: i64) {
    if !netpoll_enabled() {
        return;
    }
    let timeout = if wait_ms < 0 {
        -1
    } else if wait_ms > i32::MAX as i64 {
        i32::MAX
    } else {
        wait_ms as i32
    };

    let regs = {
        let reg = netpoll_reg_lock();
        reg.clone()
    };
    let mut fds: Vec<pollfd> = Vec::with_capacity(regs.len() + 1);
    let mut pds: Vec<*mut gost_poll_desc> = Vec::with_capacity(regs.len() + 1);

    if NETPOLL_BREAK_RFD >= 0 {
        fds.push(pollfd {
            fd: NETPOLL_BREAK_RFD,
            events: POLLIN as i16,
            revents: 0,
        });
        pds.push(ptr::null_mut());
    }

    for key in regs {
        let pd = key as *mut gost_poll_desc;
        if pd.is_null() || !netpoll_reg_contains(pd) {
            continue;
        }
        let fd = (*pd).handle as i32;
        if fd < 0 {
            continue;
        }
        let want = netpoll_waiters_want(pd);
        if want == 0 {
            continue;
        }
        let mut events: i16 = 0;
        if (want & PD_WANT_READ) != 0 {
            events |= POLLIN as i16;
        }
        if (want & PD_WANT_WRITE) != 0 {
            events |= POLLOUT as i16;
        }
        if events == 0 {
            continue;
        }
        fds.push(pollfd { fd, events, revents: 0 });
        pds.push(pd);
    }
    if fds.is_empty() {
        return;
    }

    let n = poll(fds.as_mut_ptr(), fds.len() as libc::nfds_t, timeout);
    if n <= 0 {
        return;
    }
    for i in 0..fds.len() {
        let revents = fds[i].revents as i32;
        if revents == 0 {
            continue;
        }
        let pd = pds[i];
        if pd.is_null() {
            netpoll_drain_breakfd();
            continue;
        }
        if !netpoll_reg_contains(pd) {
            continue;
        }
        if (*pd).handle as i32 != fds[i].fd {
            continue;
        }
        let mut clear_mask = 0u32;
        if (revents & (POLLIN | POLLERR | POLLHUP | POLLNVAL)) != 0 {
            clear_mask |= PD_ARMED_READ;
            let g = (*pd).r_wait.swap(ptr::null_mut(), Ordering::AcqRel);
            if !g.is_null() {
                ready.push(g);
            }
        }
        if (revents & (POLLOUT | POLLERR | POLLHUP | POLLNVAL)) != 0 {
            clear_mask |= PD_ARMED_WRITE;
            let g = (*pd).w_wait.swap(ptr::null_mut(), Ordering::AcqRel);
            if !g.is_null() {
                ready.push(g);
            }
        }
        if clear_mask != 0 {
            (*pd).state.fetch_and(!clear_mask, Ordering::AcqRel);
        }
        let rearm_want = netpoll_waiters_want(pd);
        if rearm_want != 0 {
            let _ = netpoll_arm(pd, rearm_want);
        }
    }
}

#[cfg(windows)]
unsafe fn netpoll_wait_collect(ready: &mut Vec<*mut gost_g>, wait_ms: i64) {
    if !netpoll_enabled() {
        return;
    }
    let timeout = if wait_ms < 0 {
        -1
    } else if wait_ms > i32::MAX as i64 {
        i32::MAX
    } else {
        wait_ms as i32
    };

    let regs = {
        let reg = netpoll_reg_lock();
        reg.clone()
    };
    let mut fds: Vec<WSAPOLLFD> = Vec::with_capacity(regs.len() + 1);
    let mut pds: Vec<*mut gost_poll_desc> = Vec::with_capacity(regs.len() + 1);

    if NETPOLL_BREAK_RSOCK != 0 {
        fds.push(WSAPOLLFD {
            fd: NETPOLL_BREAK_RSOCK,
            events: WS_POLLIN,
            revents: 0,
        });
        pds.push(ptr::null_mut());
    }

    for key in regs {
        let pd = key as *mut gost_poll_desc;
        if pd.is_null() || !netpoll_reg_contains(pd) {
            continue;
        }
        let sock = (*pd).handle;
        if sock == 0 {
            continue;
        }
        let want = netpoll_waiters_want(pd);
        if want == 0 {
            continue;
        }
        let mut events: i16 = 0;
        if (want & PD_WANT_READ) != 0 {
            events |= WS_POLLIN;
        }
        if (want & PD_WANT_WRITE) != 0 {
            events |= WS_POLLOUT;
        }
        if events == 0 {
            continue;
        }
        fds.push(WSAPOLLFD {
            fd: sock,
            events,
            revents: 0,
        });
        pds.push(pd);
    }
    if fds.is_empty() {
        return;
    }

    let n = WSAPoll(fds.as_mut_ptr(), fds.len() as u32, timeout);
    if n <= 0 {
        return;
    }
    for i in 0..fds.len() {
        let revents = fds[i].revents;
        if revents == 0 {
            continue;
        }
        let pd = pds[i];
        if pd.is_null() {
            netpoll_drain_breakfd();
            continue;
        }
        if !netpoll_reg_contains(pd) {
            continue;
        }
        if (*pd).handle != fds[i].fd {
            continue;
        }
        let mut clear_mask = 0u32;
        if (revents & (WS_POLLIN | WS_POLLERR | WS_POLLHUP | WS_POLLNVAL)) != 0 {
            clear_mask |= PD_ARMED_READ;
            let g = (*pd).r_wait.swap(ptr::null_mut(), Ordering::AcqRel);
            if !g.is_null() {
                ready.push(g);
            }
        }
        if (revents & (WS_POLLOUT | WS_POLLERR | WS_POLLHUP | WS_POLLNVAL)) != 0 {
            clear_mask |= PD_ARMED_WRITE;
            let g = (*pd).w_wait.swap(ptr::null_mut(), Ordering::AcqRel);
            if !g.is_null() {
                ready.push(g);
            }
        }
        if clear_mask != 0 {
            (*pd).state.fetch_and(!clear_mask, Ordering::AcqRel);
        }
        let rearm_want = netpoll_waiters_want(pd);
        if rearm_want != 0 {
            let _ = netpoll_arm(pd, rearm_want);
        }
    }
}

#[cfg(any(target_os = "linux", target_os = "macos", windows))]
unsafe fn netpoll_wait_idle_locked(wait_ms: i64) -> bool {
    if !netpoll_enabled() {
        return false;
    }
    if NETPOLL_WAITER
        .compare_exchange(0, 1, Ordering::AcqRel, Ordering::Relaxed)
        .is_err()
    {
        return false;
    }

    let sched = g_sched_mut();
    os_mutex_unlock(&mut sched.mu);

    let mut ready: Vec<*mut gost_g> = Vec::with_capacity(32);
    netpoll_wait_collect(&mut ready, wait_ms);

    NETPOLL_WAITER.store(0, Ordering::Release);

    if !ready.is_empty() {
        goready_batch(&mut ready);
    }

    os_mutex_lock(&mut sched.mu);
    true
}

#[cfg(not(any(target_os = "linux", target_os = "macos", windows)))]
unsafe fn netpoll_wait_idle_locked(_wait_ms: i64) -> bool {
    false
}

#[cfg(target_os = "linux")]
unsafe fn netpoll_del(pd: *mut gost_poll_desc) {
    if pd.is_null() || !netpoll_enabled() || NETPOLL_EPFD < 0 {
        return;
    }
    let fd = (*pd).fd;
    if fd >= 0 {
        let _ = epoll_ctl(NETPOLL_EPFD, EPOLL_CTL_DEL, fd, ptr::null_mut());
    }
}

#[cfg(target_os = "macos")]
unsafe fn netpoll_del(pd: *mut gost_poll_desc) {
    if pd.is_null() {
        return;
    }
    netpoll_reg_remove(pd);
}

#[cfg(windows)]
unsafe fn netpoll_del(pd: *mut gost_poll_desc) {
    if pd.is_null() {
        return;
    }
    netpoll_reg_remove(pd);
}

#[cfg(not(any(target_os = "linux", target_os = "macos", windows)))]
unsafe fn netpoll_del(_pd: *mut gost_poll_desc) {}

unsafe fn poll_desc_new_with(fd: i32, handle: usize) -> *mut gost_poll_desc {
    let pd = libc::malloc(mem::size_of::<gost_poll_desc>()) as *mut gost_poll_desc;
    if pd.is_null() {
        return ptr::null_mut();
    }
    ptr::write(
        pd,
        gost_poll_desc {
            fd,
            handle,
            r_wait: AtomicPtr::new(ptr::null_mut()),
            w_wait: AtomicPtr::new(ptr::null_mut()),
            state: AtomicU32::new(0),
        },
    );
    pd
}

unsafe fn poll_desc_new(fd: i32) -> *mut gost_poll_desc {
    if fd < 0 {
        return ptr::null_mut();
    }
    poll_desc_new_with(fd, fd as usize)
}

#[cfg(windows)]
unsafe fn poll_desc_new_socket(sock: usize) -> *mut gost_poll_desc {
    poll_desc_new_with(-1, sock)
}

unsafe fn poll_desc_free(pd: *mut gost_poll_desc) {
    if pd.is_null() {
        return;
    }
    libc::free(pd.cast::<c_void>());
}

#[cfg(not(windows))]
#[unsafe(no_mangle)]
pub extern "C" fn __gost_poll_open(fd: i32) -> *mut gost_poll_desc {
    unsafe {
        if fd < 0 {
            return ptr::null_mut();
        }
        netpoll_init();
        if !netpoll_enabled() {
            return ptr::null_mut();
        }
        if netpoll_set_nonblock(fd) != 0 {
            return ptr::null_mut();
        }
        poll_desc_new(fd)
    }
}

#[cfg(windows)]
#[unsafe(no_mangle)]
pub extern "C" fn __gost_poll_open(fd: i32) -> *mut gost_poll_desc {
    let _ = fd;
    ptr::null_mut()
}

#[cfg(windows)]
#[unsafe(no_mangle)]
pub extern "C" fn __gost_poll_open_socket(sock: u64) -> *mut gost_poll_desc {
    unsafe {
        if sock == 0 || sock == u64::MAX {
            return ptr::null_mut();
        }
        let sock = sock as usize;
        netpoll_init();
        if !netpoll_enabled() {
            return ptr::null_mut();
        }
        if netpoll_set_nonblock_socket(sock) != 0 {
            return ptr::null_mut();
        }
        poll_desc_new_socket(sock)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_poll_close(pd: *mut gost_poll_desc) {
    unsafe {
        if pd.is_null() {
            return;
        }
        netpoll_del(pd);
        poll_desc_free(pd);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_poll_arm(pd: *mut gost_poll_desc, want: i32) -> i32 {
    unsafe {
        if pd.is_null() {
            return -1;
        }
        let mut w = 0u32;
        if (want & (PD_WANT_READ as i32)) != 0 {
            w |= PD_WANT_READ;
        }
        if (want & (PD_WANT_WRITE as i32)) != 0 {
            w |= PD_WANT_WRITE;
        }
        netpoll_arm(pd, w)
    }
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
    let mut wake_netpoll = false;
    if old_earliest == i64::MAX || new_earliest < old_earliest {
        os_cond_signal(&mut sched.cv);
        wake_netpoll = true;
    }
    os_mutex_unlock(&mut sched.mu);
    if wake_netpoll {
        netpoll_break();
    }
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

unsafe fn timer_cancel_all_on_exit() {
    let sched = g_sched_mut();
    os_mutex_lock(&mut sched.mu);
    let mut list: *mut gost_timer = ptr::null_mut();
    if !sched.timers_heap.is_null() {
        loop {
            let t = th_pop(sched.timers_heap);
            if t.is_null() { break; }
            (*t).next = list;
            list = t;
        }
    }
    os_mutex_unlock(&mut sched.mu);

    while !list.is_null() {
        let next = (*list).next;
        stat_inc(&ST_TIMER_CANCELED_EXIT);
        __gost_chan_release((*list).ch);
        libc::free(list as *mut c_void);
        list = next;
    }
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
    sched.timer_due_batch = 4096;
    sched.timer_wake_batch = 16384;
    sched.timers_heap = ptr::null_mut();
    sched.ps = ptr::null_mut();
    sched.ms = ptr::null_mut();
    sched.mthreads = ptr::null_mut();
    netpoll_init();
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
    if enq {
        netpoll_break();
    }
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
    if enq {
        netpoll_break();
    }
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
    if enq {
        netpoll_break();
    }
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

unsafe fn gopark_committed() {
    let g = tls_g_get();
    let m = tls_m_get();
    if g.is_null() || m.is_null() {
        return;
    }
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
                netpoll_break();
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
            let mut wait_ms = -1i64;
            if next_when != i64::MAX {
                wait_ms = next_when - now;
                if wait_ms < 0 { wait_ms = 0; }
            }
            if netpoll_wait_idle_locked(wait_ms) {
                continue;
            }
            if next_when != i64::MAX {
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
            due = timer_pop_due_batch(now, sched.timer_due_batch);
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

#[unsafe(no_mangle)]
pub extern "C" fn __gost_chan_retain(ch: *mut __gost_chan) {
    unsafe {
        if !chan_is_registered(ch) { return; }
        (*ch).refcount.fetch_add(1, Ordering::Relaxed);
    }
}

unsafe fn __gost_chan_release(ch: *mut __gost_chan) {
    if !chan_is_registered(ch) { return; }
    if (*ch).refcount.fetch_sub(1, Ordering::AcqRel) != 1 { return; }
    chan_unregister(ch);
    os_mutex_destroy(&mut (*ch).mu);
    if !(*ch).buf.is_null() { __gost_free((*ch).buf as *mut c_void, 0, 1); }
    libc::free(ch as *mut c_void);
}

unsafe fn chan_send_nowait_collect(ch: *mut __gost_chan, wake: &mut Vec<*mut gost_g>) {
    if !chan_is_registered(ch) { return; }
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
    let sched = g_sched_mut();
    let wake_batch = sched.timer_wake_batch;
    let mut wake: Vec<*mut gost_g> = Vec::with_capacity(wake_batch.min(4096));
    while !list.is_null() {
        let next = (*list).next;
        chan_send_nowait_collect((*list).ch, &mut wake);
        stat_inc(&ST_TIMER_FIRED);
        __gost_chan_release((*list).ch);
        libc::free(list as *mut c_void);
        list = next;
        if wake.len() >= wake_batch {
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
    if enq {
        netpoll_break();
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_rt_init() {
    G_SCHED_ONCE.call_once(|| unsafe { sched_init_once() });
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_rt_start(main_fn: Option<gost_main_fn>) -> i32 {
    unsafe {
        os_console_utf8_init();
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
        os_mutex_lock(&mut sched.mu);
        sched.timer_due_batch = env_usize_clamp("GOST_TIMER_DUE_BATCH", 4096, 256, 1 << 20);
        sched.timer_wake_batch = env_usize_clamp("GOST_TIMER_WAKE_BATCH", 16384, 256, 1 << 20);
        os_mutex_unlock(&mut sched.mu);

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

        timer_cancel_all_on_exit();

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
pub extern "C" fn __gost_singleton_acquire(name_ptr: *const u8, name_len: i64) -> i32 {
    #[cfg(windows)]
    unsafe {
        const ERROR_ALREADY_EXISTS: u32 = 183;
        let name = match raw_utf8_from(name_ptr, name_len) {
            Ok(v) => v,
            Err(_) => return 0,
        };
        let mut wide: Vec<u16> = name.encode_utf16().collect();
        wide.push(0);
        let h = CreateMutexW(ptr::null_mut(), 1, wide.as_ptr());
        if h.is_null() {
            return 0;
        }
        let err = GetLastError();
        if err == ERROR_ALREADY_EXISTS {
            let _ = CloseHandle(h);
            return 0;
        }
        G_SINGLETON_MUTEX = h;
        return 1;
    }
    #[cfg(not(windows))]
    {
        let _ = (name_ptr, name_len);
        1
    }
}

#[repr(C)]
pub struct gost_string {
    pub ptr: *const u8,
    pub len: i64,
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_string_len(ptr: *const u8, len: i64) -> i64 {
    if std::env::var_os("GOST_DEBUG_STRING").is_some() {
        eprintln!("string_len: ptr={:?} len={}", ptr, len);
    }
    if ptr.is_null() && len != 0 {
        __gost_panic(b"string is null\0".as_ptr(), 14);
    }
    len
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_string_get(ptr: *const u8, len: i64, idx: i64) -> i32 {
    unsafe {
        if ptr.is_null() {
            __gost_panic(b"string is null\0".as_ptr(), 14);
        }
        if idx < 0 || idx >= len {
            __gost_panic(b"string index out of bounds\0".as_ptr(), 27);
        }
        let b = *ptr.add(idx as usize);
        b as i32
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_string_slice(out: *mut gost_string, ptr: *const u8, slen: i64, start: i64, len: i64) {
    unsafe {
        if out.is_null() {
            __gost_panic(b"string out is null\0".as_ptr(), 18);
        }
        if std::env::var_os("GOST_DEBUG_STRING").is_some() {
            eprintln!("string_slice: slen={} start={} len={}", slen, start, len);
        }
        if slen < 0 {
            __gost_panic(b"string length invalid\0".as_ptr(), 22);
        }
        if ptr.is_null() && slen != 0 {
            __gost_panic(b"string is null\0".as_ptr(), 14);
        }
        if start < 0 || len < 0 {
            __gost_panic(b"string slice out of bounds\0".as_ptr(), 29);
        }
        if start > slen || slen - start < len {
            __gost_panic(b"string slice out of bounds\0".as_ptr(), 29);
        }
        if len == 0 {
            (*out).ptr = ptr::null();
            (*out).len = 0;
            return;
        }
        let n = len as usize;
        let dst = __gost_alloc(n, 1) as *mut u8;
        ptr::copy_nonoverlapping(ptr.add(start as usize), dst, n);
        (*out).ptr = dst as *const u8;
        (*out).len = len;
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_string_concat(out: *mut gost_string, a_ptr: *const u8, a_len: i64, b_ptr: *const u8, b_len: i64) {
    unsafe {
        if out.is_null() {
            __gost_panic(b"string out is null\0".as_ptr(), 18);
        }
        if a_len < 0 || b_len < 0 {
            __gost_panic(b"string length invalid\0".as_ptr(), 22);
        }
        if (a_ptr.is_null() && a_len != 0) || (b_ptr.is_null() && b_len != 0) {
            __gost_panic(b"string is null\0".as_ptr(), 14);
        }
        let total = a_len.checked_add(b_len).unwrap_or(-1);
        if total < 0 {
            __gost_panic(b"string length overflow\0".as_ptr(), 24);
        }
        if total == 0 {
            (*out).ptr = ptr::null();
            (*out).len = 0;
            return;
        }
        let n = total as usize;
        let dst = __gost_alloc(n, 1) as *mut u8;
        if a_len > 0 {
            ptr::copy_nonoverlapping(a_ptr, dst, a_len as usize);
        }
        if b_len > 0 {
            ptr::copy_nonoverlapping(b_ptr, dst.add(a_len as usize), b_len as usize);
        }
        (*out).ptr = dst as *const u8;
        (*out).len = total;
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_string_from_byte(out: *mut gost_string, b: i32) {
    unsafe {
        if out.is_null() {
            __gost_panic(b"string out is null\0".as_ptr(), 18);
        }
        if b < 0 || b > 255 {
            __gost_panic(b"byte out of range\0".as_ptr(), 17);
        }
        let dst = __gost_alloc(1, 1) as *mut u8;
        *dst = b as u8;
        (*out).ptr = dst as *const u8;
        (*out).len = 1;
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_last_status() -> i32 {
    NET_LAST_STATUS.with(|v| v.get())
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_last_http_status() -> i32 {
    NET_LAST_HTTP_STATUS.with(|v| v.get())
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_last_error(out: *mut gost_string) {
    unsafe {
        if out.is_null() {
            return;
        }
        NET_LAST_ERR.with(|v| gost_string_set_bytes(out, v.borrow().as_bytes()));
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_last_peer(out: *mut gost_string) {
    unsafe {
        if out.is_null() {
            return;
        }
        NET_LAST_PEER.with(|v| gost_string_set_bytes(out, v.borrow().as_bytes()));
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_tcp_listen(addr_ptr: *const u8, addr_len: i64) -> i64 {
    unsafe {
        let addr = match raw_utf8_from(addr_ptr, addr_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return 0;
            }
        };
        match TcpListener::bind(addr.as_str()) {
            Ok(listener) => {
                net_clear_last_peer();
                net_set_ok();
                let pd = net_open_pd_listener(&listener);
                net_insert_handle(NetHandle::TcpListener {
                    sock: Arc::new(listener),
                    pd: pd as usize,
                })
            }
            Err(e) => {
                net_set_err(format!("tcp listen failed: {}", e));
                0
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_tcp_accept(listener_handle: i64) -> i64 {
    let (listener, pd) = match net_get_tcp_listener(listener_handle) {
        Ok(v) => v,
        Err(e) => {
            net_set_err(e);
            return 0;
        }
    };
    loop {
        match listener.accept() {
            Ok((stream, peer)) => {
                net_tune_tcp_stream(&stream);
                net_set_last_peer(peer.to_string());
                net_set_ok();
                let child_pd = net_open_pd_stream(&stream);
                return net_insert_handle(NetHandle::TcpStream {
                    sock: Arc::new(stream),
                    pd: child_pd as usize,
                });
            }
            Err(e) if e.kind() == ErrorKind::Interrupted => continue,
            Err(e) if e.kind() == ErrorKind::WouldBlock => {
                match unsafe { net_wait_read(pd) } {
                    Ok(()) => continue,
                    Err(msg) => {
                        net_set_err(msg);
                        return 0;
                    }
                }
            }
            Err(e) => {
                net_set_err(format!("tcp accept failed: {}", e));
                return 0;
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_tcp_connect(addr_ptr: *const u8, addr_len: i64) -> i64 {
    unsafe {
        let addr = match raw_utf8_from(addr_ptr, addr_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return 0;
            }
        };
        netpoll_init();
        let conn = if netpoll_enabled() {
            net_tcp_connect_nonblocking(addr.as_str())
        } else {
            net_tcp_connect_blocking(addr.as_str())
        };
        match conn {
            Ok((stream, pd)) => {
                net_clear_last_peer();
                net_set_ok();
                net_insert_handle(NetHandle::TcpStream {
                    sock: Arc::new(stream),
                    pd: pd as usize,
                })
            }
            Err(e) => {
                net_set_err(e);
                0
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_tcp_close(handle: i64) -> i32 {
    if net_remove_handle(handle) {
        net_set_ok();
        0
    } else {
        net_set_err("invalid handle".to_string());
        -1
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_tcp_write(handle: i64, data_ptr: *const u8, data_len: i64) -> i64 {
    unsafe {
        let (stream, pd) = match net_get_tcp_stream(handle) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return -1;
            }
        };
        let payload = match raw_bytes_from(data_ptr, data_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return -1;
            }
        };
        if payload.is_empty() {
            net_set_ok();
            return 0;
        }

        let mut off = 0usize;
        loop {
            let mut sref = &*stream;
            match sref.write(&payload[off..]) {
                Ok(0) => {
                    if off == 0 {
                        net_set_eof();
                        return 0;
                    }
                    net_set_ok();
                    return off as i64;
                }
                Ok(n) => {
                    off += n;
                    net_set_ok();
                    return off as i64;
                }
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) if e.kind() == ErrorKind::WouldBlock => {
                    match net_wait_write(pd) {
                        Ok(()) => continue,
                        Err(msg) => {
                            if off > 0 {
                                net_set_ok();
                                return off as i64;
                            }
                            net_set_err(msg);
                            return -1;
                        }
                    }
                }
                Err(e) => {
                    if off > 0 {
                        net_set_ok();
                        return off as i64;
                    }
                    net_set_err(format!("tcp write failed: {}", e));
                    return -1;
                }
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_tcp_read(out: *mut gost_string, handle: i64, max_len: i32) {
    unsafe {
        gost_string_set_bytes(out, &[]);
        let (stream, pd) = match net_get_tcp_stream(handle) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return;
            }
        };
        let cap = if max_len <= 0 { 4096usize } else { max_len as usize };
        let mut buf = vec![0u8; cap];
        loop {
            let mut sref = &*stream;
            match sref.read(&mut buf) {
                Ok(0) => {
                    net_set_eof();
                    return;
                }
                Ok(n) => {
                    buf.truncate(n);
                    gost_string_set_bytes(out, &buf);
                    net_set_ok();
                    return;
                }
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) if e.kind() == ErrorKind::WouldBlock => {
                    match net_wait_read(pd) {
                        Ok(()) => continue,
                        Err(msg) => {
                            net_set_err(msg);
                            return;
                        }
                    }
                }
                Err(e) => {
                    net_set_err(format!("tcp read failed: {}", e));
                    return;
                }
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_udp_bind(addr_ptr: *const u8, addr_len: i64) -> i64 {
    unsafe {
        let addr = match raw_utf8_from(addr_ptr, addr_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return 0;
            }
        };
        match UdpSocket::bind(addr.as_str()) {
            Ok(sock) => {
                net_set_ok();
                let pd = net_open_pd_udp(&sock);
                net_insert_handle(NetHandle::UdpSocket {
                    sock: Arc::new(sock),
                    pd: pd as usize,
                })
            }
            Err(e) => {
                net_set_err(format!("udp bind failed: {}", e));
                0
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_udp_connect(addr_ptr: *const u8, addr_len: i64) -> i64 {
    unsafe {
        let addr = match raw_utf8_from(addr_ptr, addr_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return 0;
            }
        };
        match UdpSocket::bind("0.0.0.0:0") {
            Ok(sock) => {
                if let Err(e) = sock.connect(addr.as_str()) {
                    net_set_err(format!("udp connect failed: {}", e));
                    return 0;
                }
                net_set_ok();
                let pd = net_open_pd_udp(&sock);
                net_insert_handle(NetHandle::UdpSocket {
                    sock: Arc::new(sock),
                    pd: pd as usize,
                })
            }
            Err(e) => {
                net_set_err(format!("udp socket create failed: {}", e));
                0
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_udp_close(handle: i64) -> i32 {
    if net_remove_handle(handle) {
        net_set_ok();
        0
    } else {
        net_set_err("invalid handle".to_string());
        -1
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_udp_send(handle: i64, data_ptr: *const u8, data_len: i64) -> i64 {
    unsafe {
        let (sock, pd) = match net_get_udp_socket(handle) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return -1;
            }
        };
        let payload = match raw_bytes_from(data_ptr, data_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return -1;
            }
        };
        if payload.is_empty() {
            net_set_ok();
            return 0;
        }
        loop {
            match sock.send(payload) {
                Ok(n) => {
                    net_set_ok();
                    return n as i64;
                }
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) if e.kind() == ErrorKind::WouldBlock => {
                    match net_wait_write(pd) {
                        Ok(()) => continue,
                        Err(msg) => {
                            net_set_err(msg);
                            return -1;
                        }
                    }
                }
                Err(e) => {
                    net_set_err(format!("udp send failed: {}", e));
                    return -1;
                }
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_udp_send_to(
    handle: i64,
    addr_ptr: *const u8,
    addr_len: i64,
    data_ptr: *const u8,
    data_len: i64,
) -> i64 {
    unsafe {
        let (sock, pd) = match net_get_udp_socket(handle) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return -1;
            }
        };
        let addr = match raw_utf8_from(addr_ptr, addr_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return -1;
            }
        };
        let payload = match raw_bytes_from(data_ptr, data_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return -1;
            }
        };
        loop {
            match sock.send_to(payload, addr.as_str()) {
                Ok(n) => {
                    net_set_ok();
                    return n as i64;
                }
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) if e.kind() == ErrorKind::WouldBlock => {
                    match net_wait_write(pd) {
                        Ok(()) => continue,
                        Err(msg) => {
                            net_set_err(msg);
                            return -1;
                        }
                    }
                }
                Err(e) => {
                    net_set_err(format!("udp send_to failed: {}", e));
                    return -1;
                }
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_udp_recv(out: *mut gost_string, handle: i64, max_len: i32) {
    unsafe {
        gost_string_set_bytes(out, &[]);
        net_clear_last_peer();
        let (sock, pd) = match net_get_udp_socket(handle) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return;
            }
        };
        let cap = if max_len <= 0 { 65535usize } else { max_len as usize };
        let mut buf = vec![0u8; cap];
        loop {
            match sock.recv(&mut buf) {
                Ok(n) => {
                    buf.truncate(n);
                    gost_string_set_bytes(out, &buf);
                    net_set_ok();
                    return;
                }
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) if e.kind() == ErrorKind::WouldBlock => {
                    match net_wait_read(pd) {
                        Ok(()) => continue,
                        Err(msg) => {
                            net_set_err(msg);
                            return;
                        }
                    }
                }
                Err(e) => {
                    net_set_err(format!("udp recv failed: {}", e));
                    return;
                }
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_udp_recv_from(out: *mut gost_string, handle: i64, max_len: i32) {
    unsafe {
        gost_string_set_bytes(out, &[]);
        net_clear_last_peer();
        let (sock, pd) = match net_get_udp_socket(handle) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return;
            }
        };
        let cap = if max_len <= 0 { 65535usize } else { max_len as usize };
        let mut buf = vec![0u8; cap];
        loop {
            match sock.recv_from(&mut buf) {
                Ok((n, peer)) => {
                    buf.truncate(n);
                    gost_string_set_bytes(out, &buf);
                    net_set_last_peer(peer.to_string());
                    net_set_ok();
                    return;
                }
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) if e.kind() == ErrorKind::WouldBlock => {
                    match net_wait_read(pd) {
                        Ok(()) => continue,
                        Err(msg) => {
                            net_set_err(msg);
                            return;
                        }
                    }
                }
                Err(e) => {
                    net_set_err(format!("udp recv_from failed: {}", e));
                    return;
                }
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_ws_connect(url_ptr: *const u8, url_len: i64) -> i64 {
    unsafe {
        let url = match raw_utf8_from(url_ptr, url_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return 0;
            }
        };
        if !(url.starts_with("ws://") || url.starts_with("wss://")) {
            net_set_err("websocket url must start with ws:// or wss://".to_string());
            return 0;
        }
        match ws_connect(url.as_str()) {
            Ok((mut ws, _)) => {
                if let Err(e) = ws_set_nonblocking(&mut ws, true) {
                    net_set_err(e);
                    return 0;
                }
                let pd = ws_open_pd(&mut ws);
                net_set_ok();
                net_insert_handle(NetHandle::WsSocket {
                    ws: Arc::new(Mutex::new(ws)),
                    pd: pd as usize,
                })
            }
            Err(e) => {
                net_set_err(format!("ws connect failed: {}", e));
                0
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_ws_close(handle: i64) -> i32 {
    let (ws, _pd) = match net_get_ws(handle) {
        Ok(v) => v,
        Err(e) => {
            net_set_err(e);
            return -1;
        }
    };
    {
        let mut guard = match ws.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        let _ = guard.close(None);
    }
    if net_remove_handle(handle) {
        net_set_ok();
        0
    } else {
        net_set_err("invalid handle".to_string());
        -1
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_ws_send_text(handle: i64, data_ptr: *const u8, data_len: i64) -> i32 {
    unsafe {
        let (ws, pd) = match net_get_ws(handle) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return -1;
            }
        };
        let payload = match raw_utf8_from(data_ptr, data_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return -1;
            }
        };
        loop {
            let mut guard = match ws.lock() {
                Ok(g) => g,
                Err(e) => e.into_inner(),
            };
            match guard.send(WsMessage::Text(payload.clone().into())) {
                Ok(()) => {
                    net_set_ok();
                    return 0;
                }
                Err(WsError::Io(e)) if e.kind() == ErrorKind::WouldBlock => {
                    drop(guard);
                    match net_wait_write(pd) {
                        Ok(()) => continue,
                        Err(msg) => {
                            net_set_err(msg);
                            return -1;
                        }
                    }
                }
                Err(WsError::ConnectionClosed) | Err(WsError::AlreadyClosed) => {
                    net_set_eof();
                    return 1;
                }
                Err(e) => {
                    net_set_err(format!("ws send failed: {}", e));
                    return -1;
                }
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_ws_recv_text(out: *mut gost_string, handle: i64) {
    unsafe {
        gost_string_set_bytes(out, &[]);
        let (ws, pd) = match net_get_ws(handle) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return;
            }
        };
        loop {
            let mut guard = match ws.lock() {
                Ok(g) => g,
                Err(e) => e.into_inner(),
            };
            match guard.read() {
                Ok(WsMessage::Text(txt)) => {
                    gost_string_set_bytes(out, txt.as_bytes());
                    net_set_ok();
                    return;
                }
                Ok(WsMessage::Close(_)) => {
                    net_set_eof();
                    return;
                }
                Ok(_) => continue,
                Err(WsError::Io(e)) if e.kind() == ErrorKind::WouldBlock => {
                    drop(guard);
                    match net_wait_read(pd) {
                        Ok(()) => continue,
                        Err(msg) => {
                            net_set_err(msg);
                            return;
                        }
                    }
                }
                Err(WsError::ConnectionClosed) | Err(WsError::AlreadyClosed) => {
                    net_set_eof();
                    return;
                }
                Err(e) => {
                    net_set_err(format!("ws recv failed: {}", e));
                    return;
                }
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_http_request(
    out: *mut gost_string,
    method_ptr: *const u8,
    method_len: i64,
    url_ptr: *const u8,
    url_len: i64,
    body_ptr: *const u8,
    body_len: i64,
    content_type_ptr: *const u8,
    content_type_len: i64,
) {
    unsafe {
        gost_string_set_bytes(out, &[]);
        net_set_http_status(0);

        let method = match raw_utf8_from(method_ptr, method_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return;
            }
        };
        let url = match raw_utf8_from(url_ptr, url_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return;
            }
        };
        let body = match raw_bytes_from(body_ptr, body_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return;
            }
        };
        let content_type = match raw_utf8_from(content_type_ptr, content_type_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return;
            }
        };

        match net_http_request_impl(
            method.as_str(),
            url.as_str(),
            body,
            content_type.as_str(),
            "",
        ) {
            Ok((status, payload)) => {
                net_set_http_status(status);
                gost_string_set_bytes(out, &payload);
                net_set_ok();
            }
            Err(e) => {
                net_set_http_status(0);
                net_set_err(e);
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_net_http_request_headers(
    out: *mut gost_string,
    method_ptr: *const u8,
    method_len: i64,
    url_ptr: *const u8,
    url_len: i64,
    body_ptr: *const u8,
    body_len: i64,
    content_type_ptr: *const u8,
    content_type_len: i64,
    headers_ptr: *const u8,
    headers_len: i64,
) {
    unsafe {
        gost_string_set_bytes(out, &[]);
        net_set_http_status(0);

        let method = match raw_utf8_from(method_ptr, method_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return;
            }
        };
        let url = match raw_utf8_from(url_ptr, url_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return;
            }
        };
        let body = match raw_bytes_from(body_ptr, body_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return;
            }
        };
        let content_type = match raw_utf8_from(content_type_ptr, content_type_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return;
            }
        };
        let headers = match raw_utf8_from(headers_ptr, headers_len) {
            Ok(v) => v,
            Err(e) => {
                net_set_err(e);
                return;
            }
        };

        match net_http_request_impl(
            method.as_str(),
            url.as_str(),
            body,
            content_type.as_str(),
            headers.as_str(),
        ) {
            Ok((status, payload)) => {
                net_set_http_status(status);
                gost_string_set_bytes(out, &payload);
                net_set_ok();
            }
            Err(e) => {
                net_set_http_status(0);
                net_set_err(e);
            }
        }
    }
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
        chan_register(ch);
        ch
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_chan_send(ch: *mut __gost_chan, elem: *const c_void) -> i32 {
    unsafe {
        if !chan_is_registered(ch) { return 1; }
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
        if !chan_is_registered(ch) { return 1; }
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
        if !chan_is_registered(ch) { return 1; }
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
        if !chan_is_registered(ch) { return; }
        __gost_chan_close(ch);
        __gost_chan_release(ch);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_chan_can_send(ch: *mut __gost_chan) -> i32 {
    unsafe {
        if !chan_is_registered(ch) { return 0; }
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
        if !chan_is_registered(ch) { return 0; }
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
pub extern "C" fn __gost_select_wait(chans: *mut *mut __gost_chan, ops: *const i32, n: u32) -> i32 {
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
            if !chan_is_registered(ch) { continue; }
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

        // Re-check readiness after enqueue to avoid missing a close/send/recv
        let mut ready = false;
        for i in 0..n {
            let ch = *chans.add(i as usize);
            if !chan_is_registered(ch) { continue; }
            let op = if ops.is_null() { 0 } else { *ops.add(i as usize) };
            let r = if op != 0 {
                __gost_chan_can_send(ch)
            } else {
                __gost_chan_can_recv(ch)
            };
            if r != 0 {
                ready = true;
                break;
            }
        }

        if ready {
            (*w).fired.store(1, Ordering::Relaxed);
            let mut node = (*w).nodes;
            while !node.is_null() {
                let next = (*node).next_w;
                if chan_is_registered((*node).ch) {
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
            return 0;
        }

        gopark();

        let mut node = (*w).nodes;
        while !node.is_null() {
            let next = (*node).next_w;
            if chan_is_registered((*node).ch) {
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
                // Use memmove semantics to tolerate allocator-level aliasing edge cases.
                ptr::copy((*s).data, new_data, (*s).elem_size * (*s).len as usize);
            }
            if !(*s).data.is_null() { __gost_free((*s).data as *mut c_void, 0, 1); }
            (*s).data = new_data;
            (*s).cap = new_cap;
        }
        if (*s).elem_size > 0 {
            if elem_bytes.is_null() {
                __gost_panic(b"slice push elem is null\0".as_ptr(), 24);
            }
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
        (*s).strong.store(1, Ordering::Relaxed);
        (*s).weak.store(1, Ordering::Relaxed);
        (*s).drop_payload = drop_payload;
        if payload_size > 0 && !payload_bytes.is_null() {
            ptr::copy_nonoverlapping(payload_bytes as *const u8, (*s).payload.as_mut_ptr(), payload_size);
        }
        s
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_shared_inc(s: *mut __gost_shared) {
    unsafe { if !s.is_null() { (*s).strong.fetch_add(1, Ordering::Relaxed); } }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_shared_dec(s: *mut __gost_shared) {
    unsafe {
        if s.is_null() { return; }
        if (*s).strong.fetch_sub(1, Ordering::AcqRel) == 1 {
            if let Some(drop_fn) = (*s).drop_payload { drop_fn((*s).payload.as_mut_ptr() as *mut c_void); }
            __gost_shared_weak_dec_inner(s);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_shared_get_ptr(s: *mut __gost_shared) -> *mut c_void {
    unsafe { if s.is_null() { ptr::null_mut() } else { (*s).payload.as_mut_ptr() as *mut c_void } }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_shared_is_unique(s: *mut __gost_shared) -> i32 {
    unsafe { if s.is_null() { 0 } else { if (*s).strong.load(Ordering::Relaxed) == 1 { 1 } else { 0 } } }
}

unsafe fn __gost_shared_weak_dec_inner(s: *mut __gost_shared) {
    if s.is_null() { return; }
    if (*s).weak.fetch_sub(1, Ordering::AcqRel) == 1 {
        __gost_free(s as *mut c_void, 0, 1);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_weak_new(s: *mut __gost_shared) -> *mut __gost_weak {
    unsafe {
        if s.is_null() { return ptr::null_mut(); }
        (*s).weak.fetch_add(1, Ordering::Relaxed);
        let w = __gost_alloc(mem::size_of::<__gost_weak>(), 1) as *mut __gost_weak;
        (*w).inner = s;
        w
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_weak_upgrade(w: *mut __gost_weak) -> *mut __gost_shared {
    unsafe {
        if w.is_null() { return ptr::null_mut(); }
        let s = (*w).inner;
        if s.is_null() { return ptr::null_mut(); }
        loop {
            let cur = (*s).strong.load(Ordering::Acquire);
            if cur == 0 {
                return ptr::null_mut();
            }
            if (*s)
                .strong
                .compare_exchange_weak(cur, cur + 1, Ordering::AcqRel, Ordering::Relaxed)
                .is_ok()
            {
                return s;
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __gost_weak_dec(w: *mut __gost_weak) {
    unsafe {
        if w.is_null() { return; }
        let s = (*w).inner;
        if !s.is_null() {
            __gost_shared_weak_dec_inner(s);
        }
        __gost_free(w as *mut c_void, 0, 1);
    }
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
    let add = stats_total(&ST_TIMER_ADD);
    let fired = stats_total(&ST_TIMER_FIRED);
    let canceled = stats_total(&ST_TIMER_CANCELED_EXIT);
    let pending = add - fired - canceled;
    let _ = writeln!(
        &mut out,
        "timer: add={} fired={} canceled_exit={} pending={}",
        add,
        fired,
        canceled,
        pending
    );
    let _ = writeln!(
        &mut out,
        "timerbatch: due={} wake={}",
        (*g_sched_mut()).timer_due_batch,
        (*g_sched_mut()).timer_wake_batch
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
