#include "gostrt.h"

#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdatomic.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
#include <windows.h>
#endif

struct __gost_chan {
    pthread_mutex_t mu;
    pthread_cond_t can_send;
    pthread_cond_t can_recv;
    size_t elem_size;
    int32_t cap;
    uint8_t* buf;
    int32_t len;
    int32_t head;
    int32_t tail;
    int closed;
    int has_value;
    int send_waiters;
    int recv_waiters;
};

struct __gost_slice {
    size_t elem_size;
    int64_t len;
    int64_t cap;
    void (*elem_drop)(void*);
    uint8_t* data;
};

struct __gost_shared {
    _Atomic int64_t refcount;
    void (*drop_payload)(void*);
    uint8_t payload[];
};

struct __gost_map_entry {
    int used;
    union {
        int64_t i64;
        uint64_t u64;
        struct {
            const uint8_t* ptr;
            int64_t len;
        } str;
    } key;
    uint8_t* val;
};

struct __gost_map {
    int32_t key_kind;
    size_t val_size;
    int64_t len;
    int64_t cap;
    struct __gost_map_entry* entries;
};

static pthread_once_t g_select_once = PTHREAD_ONCE_INIT;
static pthread_mutex_t g_select_mu;
static pthread_cond_t g_select_cv;
static unsigned long long g_select_epoch = 0;

static void select_init(void) {
    pthread_mutex_init(&g_select_mu, NULL);
    pthread_cond_init(&g_select_cv, NULL);
    g_select_epoch = 0;
}

static void select_notify(void) {
    pthread_once(&g_select_once, select_init);
    pthread_mutex_lock(&g_select_mu);
    g_select_epoch++;
    pthread_cond_broadcast(&g_select_cv);
    pthread_mutex_unlock(&g_select_mu);
}

static void sleep_ms(int64_t ms) {
    if (ms < 0) {
        ms = 0;
    }
#ifdef _WIN32
    Sleep((DWORD)ms);
#else
    struct timespec ts;
    ts.tv_sec = (time_t)(ms / 1000);
    ts.tv_nsec = (long)((ms % 1000) * 1000000);
    nanosleep(&ts, NULL);
#endif
}

void __gost_rt_init(void) {
    pthread_once(&g_select_once, select_init);
}

void __gost_panic(const uint8_t* msg, size_t len) {
    if (msg && len > 0) {
        fwrite(msg, 1, len, stderr);
    }
    fwrite("\n", 1, 1, stderr);
    fflush(stderr);
    abort();
}

void __gost_println_str(const uint8_t* ptr, size_t len) {
    if (ptr && len > 0) {
        fwrite(ptr, 1, len, stdout);
    }
    fwrite("\n", 1, 1, stdout);
    fflush(stdout);
}

void* __gost_alloc(size_t size, size_t align) {
    (void)align;
    if (size == 0) {
        size = 1;
    }
    void* p = malloc(size);
    if (!p) {
        __gost_panic((const uint8_t*)"out of memory", 13);
    }
    return p;
}

void __gost_free(void* p, size_t size, size_t align) {
    (void)size;
    (void)align;
    free(p);
}

struct thread_start {
    __gost_thread_fn f;
    void* ctx;
};

static void* thread_entry(void* arg) {
    struct thread_start* start = (struct thread_start*)arg;
    __gost_thread_fn f = start->f;
    void* ctx = start->ctx;
    free(start);
    f(ctx);
    return NULL;
}

void __gost_spawn_thread(__gost_thread_fn f, void* ctx) {
    pthread_t t;
    struct thread_start* start = (struct thread_start*)malloc(sizeof(*start));
    if (!start) {
        __gost_panic((const uint8_t*)"out of memory", 13);
    }
    start->f = f;
    start->ctx = ctx;
    if (pthread_create(&t, NULL, thread_entry, start) == 0) {
        pthread_detach(t);
    } else {
        free(start);
        __gost_panic((const uint8_t*)"failed to spawn thread", 22);
    }
}

struct go_start {
    __gost_go_fn f;
};

static void go_entry(void* arg) {
    struct go_start* start = (struct go_start*)arg;
    __gost_go_fn f = start->f;
    free(start);
    f();
}

void __gost_go_spawn(__gost_go_fn f) {
    struct go_start* start = (struct go_start*)malloc(sizeof(*start));
    if (!start) {
        __gost_panic((const uint8_t*)"out of memory", 13);
    }
    start->f = f;
    __gost_spawn_thread(go_entry, start);
}

struct __gost_chan* __gost_chan_new(size_t elem_size, int32_t cap) {
    struct __gost_chan* ch = (struct __gost_chan*)malloc(sizeof(*ch));
    if (!ch) {
        __gost_panic((const uint8_t*)"out of memory", 13);
    }
    memset(ch, 0, sizeof(*ch));
    pthread_mutex_init(&ch->mu, NULL);
    pthread_cond_init(&ch->can_send, NULL);
    pthread_cond_init(&ch->can_recv, NULL);
    ch->elem_size = elem_size;
    ch->cap = cap;
    ch->len = 0;
    ch->head = 0;
    ch->tail = 0;
    ch->closed = 0;
    ch->has_value = 0;
    ch->send_waiters = 0;
    ch->recv_waiters = 0;
    ch->buf = NULL;
    if (elem_size > 0) {
        if (cap > 0) {
            size_t total = elem_size * (size_t)cap;
            ch->buf = (uint8_t*)__gost_alloc(total, 1);
        } else {
            ch->buf = (uint8_t*)__gost_alloc(elem_size, 1);
        }
    }
    return ch;
}

int32_t __gost_chan_send(struct __gost_chan* ch, const void* elem) {
    if (!ch) {
        return 1;
    }
    pthread_mutex_lock(&ch->mu);
    if (ch->closed) {
        pthread_mutex_unlock(&ch->mu);
        return 1;
    }
    if (ch->cap == 0) {
        ch->send_waiters++;
        while (!ch->closed && (ch->recv_waiters == 0 || ch->has_value)) {
            pthread_cond_wait(&ch->can_send, &ch->mu);
        }
        ch->send_waiters--;
        if (ch->closed) {
            pthread_mutex_unlock(&ch->mu);
            return 1;
        }
        if (ch->elem_size > 0) {
            memcpy(ch->buf, elem, ch->elem_size);
        }
        ch->has_value = 1;
        pthread_cond_signal(&ch->can_recv);
        select_notify();
        while (ch->has_value) {
            pthread_cond_wait(&ch->can_send, &ch->mu);
        }
        pthread_mutex_unlock(&ch->mu);
        select_notify();
        return 0;
    }

    while (!ch->closed && ch->len == ch->cap) {
        pthread_cond_wait(&ch->can_send, &ch->mu);
    }
    if (ch->closed) {
        pthread_mutex_unlock(&ch->mu);
        return 1;
    }
    if (ch->elem_size > 0) {
        uint8_t* slot = ch->buf + (size_t)ch->tail * ch->elem_size;
        memcpy(slot, elem, ch->elem_size);
    }
    ch->tail = (ch->tail + 1) % ch->cap;
    ch->len++;
    pthread_cond_signal(&ch->can_recv);
    pthread_mutex_unlock(&ch->mu);
    select_notify();
    return 0;
}

int32_t __gost_chan_recv(struct __gost_chan* ch, void* out_elem) {
    if (!ch) {
        return 1;
    }
    pthread_mutex_lock(&ch->mu);
    if (ch->cap == 0) {
        ch->recv_waiters++;
        pthread_cond_signal(&ch->can_send);
        while (!ch->closed && !ch->has_value) {
            pthread_cond_wait(&ch->can_recv, &ch->mu);
        }
        ch->recv_waiters--;
        if (ch->has_value) {
            if (ch->elem_size > 0) {
                memcpy(out_elem, ch->buf, ch->elem_size);
            }
            ch->has_value = 0;
            pthread_cond_signal(&ch->can_send);
            pthread_mutex_unlock(&ch->mu);
            select_notify();
            return 0;
        }
        pthread_mutex_unlock(&ch->mu);
        select_notify();
        return 1;
    }

    while (ch->len == 0 && !ch->closed) {
        pthread_cond_wait(&ch->can_recv, &ch->mu);
    }
    if (ch->len == 0 && ch->closed) {
        pthread_mutex_unlock(&ch->mu);
        return 1;
    }
    if (ch->elem_size > 0) {
        uint8_t* slot = ch->buf + (size_t)ch->head * ch->elem_size;
        memcpy(out_elem, slot, ch->elem_size);
    }
    ch->head = (ch->head + 1) % ch->cap;
    ch->len--;
    pthread_cond_signal(&ch->can_send);
    pthread_mutex_unlock(&ch->mu);
    select_notify();
    return 0;
}

int32_t __gost_chan_close(struct __gost_chan* ch) {
    if (!ch) {
        return 1;
    }
    pthread_mutex_lock(&ch->mu);
    if (ch->closed) {
        pthread_mutex_unlock(&ch->mu);
        return 1;
    }
    ch->closed = 1;
    pthread_cond_broadcast(&ch->can_send);
    pthread_cond_broadcast(&ch->can_recv);
    pthread_mutex_unlock(&ch->mu);
    select_notify();
    return 0;
}

void __gost_chan_drop(struct __gost_chan* ch) {
    if (!ch) {
        return;
    }
    pthread_mutex_destroy(&ch->mu);
    pthread_cond_destroy(&ch->can_send);
    pthread_cond_destroy(&ch->can_recv);
    if (ch->buf) {
        __gost_free(ch->buf, 0, 1);
    }
    free(ch);
}

int32_t __gost_chan_can_send(struct __gost_chan* ch) {
    if (!ch) {
        return 0;
    }
    pthread_mutex_lock(&ch->mu);
    int ready = 0;
    if (!ch->closed) {
        if (ch->cap == 0) {
            ready = (ch->recv_waiters > 0 && !ch->has_value);
        } else {
            ready = ch->len < ch->cap;
        }
    }
    pthread_mutex_unlock(&ch->mu);
    return ready ? 1 : 0;
}

int32_t __gost_chan_can_recv(struct __gost_chan* ch) {
    if (!ch) {
        return 0;
    }
    pthread_mutex_lock(&ch->mu);
    int ready = 0;
    if (ch->cap == 0) {
        ready = ch->has_value || ch->send_waiters > 0 || ch->closed;
    } else {
        ready = ch->len > 0 || ch->closed;
    }
    pthread_mutex_unlock(&ch->mu);
    return ready ? 1 : 0;
}

struct __gost_slice* __gost_slice_new(
    size_t elem_size,
    int64_t len,
    int64_t cap,
    void (*elem_drop)(void*)
) {
    if (cap < len) {
        __gost_panic((const uint8_t*)"slice cap < len", 15);
    }
    struct __gost_slice* s = (struct __gost_slice*)__gost_alloc(sizeof(*s), 1);
    s->elem_size = elem_size;
    s->len = len;
    s->cap = cap;
    s->elem_drop = elem_drop;
    s->data = NULL;
    if (elem_size > 0 && cap > 0) {
        size_t total = elem_size * (size_t)cap;
        s->data = (uint8_t*)__gost_alloc(total, 1);
        memset(s->data, 0, total);
    }
    return s;
}

void __gost_slice_drop(struct __gost_slice* s) {
    if (!s) {
        return;
    }
    if (s->elem_drop && s->elem_size > 0 && s->data) {
        for (int64_t i = 0; i < s->len; i++) {
            s->elem_drop(s->data + (size_t)i * s->elem_size);
        }
    }
    if (s->data) {
        __gost_free(s->data, 0, 1);
    }
    __gost_free(s, 0, 1);
}

int64_t __gost_slice_len(struct __gost_slice* s) {
    if (!s) {
        return 0;
    }
    return s->len;
}

void* __gost_slice_data(struct __gost_slice* s) {
    if (!s) {
        return NULL;
    }
    return s->data;
}

void __gost_slice_bounds_check(struct __gost_slice* s, int64_t i) {
    if (!s || i < 0 || i >= s->len) {
        __gost_panic((const uint8_t*)"slice index out of bounds", 25);
    }
}

void __gost_slice_push(struct __gost_slice* s, const void* elem_bytes) {
    if (!s) {
        __gost_panic((const uint8_t*)"slice is null", 13);
    }
    if (s->len == s->cap) {
        int64_t new_cap = s->cap > 0 ? s->cap * 2 : 1;
        size_t total = s->elem_size * (size_t)new_cap;
        uint8_t* new_data = (uint8_t*)__gost_alloc(total, 1);
        if (s->data && s->elem_size > 0 && s->len > 0) {
            memcpy(new_data, s->data, s->elem_size * (size_t)s->len);
        }
        if (s->data) {
            __gost_free(s->data, 0, 1);
        }
        s->data = new_data;
        s->cap = new_cap;
    }
    if (s->elem_size > 0) {
        memcpy(s->data + (size_t)s->len * s->elem_size, elem_bytes, s->elem_size);
    }
    s->len += 1;
}

int32_t __gost_slice_pop(struct __gost_slice* s, void* out_elem_bytes) {
    if (!s || s->len == 0) {
        return 1;
    }
    s->len -= 1;
    if (s->elem_size > 0 && s->data && out_elem_bytes) {
        memcpy(out_elem_bytes, s->data + (size_t)s->len * s->elem_size, s->elem_size);
    }
    return 0;
}

struct __gost_shared* __gost_shared_new(
    size_t payload_size,
    void (*drop_payload)(void*),
    const void* payload_bytes
) {
    size_t total = sizeof(struct __gost_shared) + (payload_size ? payload_size : 1);
    struct __gost_shared* s = (struct __gost_shared*)__gost_alloc(total, 1);
    atomic_init(&s->refcount, 1);
    s->drop_payload = drop_payload;
    if (payload_size > 0 && payload_bytes) {
        memcpy(s->payload, payload_bytes, payload_size);
    }
    return s;
}

void __gost_shared_inc(struct __gost_shared* s) {
    if (!s) {
        return;
    }
    atomic_fetch_add_explicit(&s->refcount, 1, memory_order_relaxed);
}

void __gost_shared_dec(struct __gost_shared* s) {
    if (!s) {
        return;
    }
    if (atomic_fetch_sub_explicit(&s->refcount, 1, memory_order_acq_rel) == 1) {
        if (s->drop_payload) {
            s->drop_payload(s->payload);
        }
        __gost_free(s, 0, 1);
    }
}

void* __gost_shared_get_ptr(struct __gost_shared* s) {
    if (!s) {
        return NULL;
    }
    return s->payload;
}

int32_t __gost_shared_is_unique(struct __gost_shared* s) {
    if (!s) {
        return 0;
    }
    return atomic_load_explicit(&s->refcount, memory_order_relaxed) == 1 ? 1 : 0;
}

static int map_key_equal(struct __gost_map* m, struct __gost_map_entry* entry, const void* key_bytes) {
    if (!entry->used) {
        return 0;
    }
    switch (m->key_kind) {
        case 1: {
            int64_t key = *(const int64_t*)key_bytes;
            return entry->key.i64 == key;
        }
        case 2: {
            uint64_t key = *(const uint64_t*)key_bytes;
            return entry->key.u64 == key;
        }
        case 3: {
            const uint8_t* ptr = *(const uint8_t* const*)key_bytes;
            const int64_t* len_ptr = (const int64_t*)((const uint8_t*)key_bytes + sizeof(void*));
            int64_t len = *len_ptr;
            if (entry->key.str.len != len) {
                return 0;
            }
            if (len == 0) {
                return 1;
            }
            return memcmp(entry->key.str.ptr, ptr, (size_t)len) == 0;
        }
        default:
            return 0;
    }
}

static void map_store_key(struct __gost_map* m, struct __gost_map_entry* entry, const void* key_bytes) {
    entry->used = 1;
    switch (m->key_kind) {
        case 1:
            entry->key.i64 = *(const int64_t*)key_bytes;
            break;
        case 2:
            entry->key.u64 = *(const uint64_t*)key_bytes;
            break;
        case 3: {
            const uint8_t* ptr = *(const uint8_t* const*)key_bytes;
            const int64_t* len_ptr = (const int64_t*)((const uint8_t*)key_bytes + sizeof(void*));
            entry->key.str.ptr = ptr;
            entry->key.str.len = *len_ptr;
            break;
        }
        default:
            break;
    }
}

struct __gost_map* __gost_map_new(int32_t key_kind, size_t val_size, int64_t cap) {
    if (cap < 0) {
        cap = 0;
    }
    struct __gost_map* m = (struct __gost_map*)__gost_alloc(sizeof(*m), 1);
    m->key_kind = key_kind;
    m->val_size = val_size;
    m->len = 0;
    m->cap = cap > 0 ? cap : 8;
    m->entries = (struct __gost_map_entry*)__gost_alloc(sizeof(struct __gost_map_entry) * (size_t)m->cap, 1);
    memset(m->entries, 0, sizeof(struct __gost_map_entry) * (size_t)m->cap);
    return m;
}

int32_t __gost_map_get(struct __gost_map* m, const void* key_bytes, void* out_val_bytes) {
    if (!m) {
        return 0;
    }
    for (int64_t i = 0; i < m->len; i++) {
        if (map_key_equal(m, &m->entries[i], key_bytes)) {
            if (out_val_bytes && m->val_size > 0) {
                memcpy(out_val_bytes, m->entries[i].val, m->val_size);
            }
            return 1;
        }
    }
    return 0;
}

void __gost_map_set(struct __gost_map* m, const void* key_bytes, const void* val_bytes) {
    if (!m) {
        return;
    }
    for (int64_t i = 0; i < m->len; i++) {
        if (map_key_equal(m, &m->entries[i], key_bytes)) {
            if (m->val_size > 0 && val_bytes) {
                memcpy(m->entries[i].val, val_bytes, m->val_size);
            }
            return;
        }
    }
    if (m->len == m->cap) {
        int64_t new_cap = m->cap > 0 ? m->cap * 2 : 8;
        struct __gost_map_entry* new_entries =
            (struct __gost_map_entry*)__gost_alloc(sizeof(struct __gost_map_entry) * (size_t)new_cap, 1);
        memset(new_entries, 0, sizeof(struct __gost_map_entry) * (size_t)new_cap);
        for (int64_t i = 0; i < m->len; i++) {
            new_entries[i] = m->entries[i];
        }
        __gost_free(m->entries, 0, 1);
        m->entries = new_entries;
        m->cap = new_cap;
    }
    struct __gost_map_entry* entry = &m->entries[m->len];
    map_store_key(m, entry, key_bytes);
    if (m->val_size > 0) {
        entry->val = (uint8_t*)__gost_alloc(m->val_size, 1);
        if (val_bytes) {
            memcpy(entry->val, val_bytes, m->val_size);
        } else {
            memset(entry->val, 0, m->val_size);
        }
    } else {
        entry->val = NULL;
    }
    m->len += 1;
}

int32_t __gost_map_del(struct __gost_map* m, const void* key_bytes) {
    if (!m) {
        return 0;
    }
    for (int64_t i = 0; i < m->len; i++) {
        if (map_key_equal(m, &m->entries[i], key_bytes)) {
            if (m->entries[i].val) {
                __gost_free(m->entries[i].val, 0, 1);
            }
            if (i != m->len - 1) {
                m->entries[i] = m->entries[m->len - 1];
            }
            m->len -= 1;
            return 1;
        }
    }
    return 0;
}

int64_t __gost_map_len(struct __gost_map* m) {
    if (!m) {
        return 0;
    }
    return m->len;
}

void __gost_map_drop(struct __gost_map* m) {
    if (!m) {
        return;
    }
    for (int64_t i = 0; i < m->len; i++) {
        if (m->entries[i].val) {
            __gost_free(m->entries[i].val, 0, 1);
        }
    }
    if (m->entries) {
        __gost_free(m->entries, 0, 1);
    }
    __gost_free(m, 0, 1);
}

int32_t __gost_select_wait(struct __gost_chan** chans, uint32_t n) {
    (void)chans;
    (void)n;
#ifdef _WIN32
    Sleep(1);
    return 0;
#else
    pthread_once(&g_select_once, select_init);
    pthread_mutex_lock(&g_select_mu);
    unsigned long long seen = g_select_epoch;
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_nsec += 1000000;
    if (ts.tv_nsec >= 1000000000) {
        ts.tv_sec += 1;
        ts.tv_nsec -= 1000000000;
    }
    while (g_select_epoch == seen) {
        int rc = pthread_cond_timedwait(&g_select_cv, &g_select_mu, &ts);
        if (rc == ETIMEDOUT) {
            break;
        }
    }
    pthread_mutex_unlock(&g_select_mu);
    return 0;
#endif
}

struct after_ctx {
    struct __gost_chan* ch;
    int64_t ms;
};

static void after_entry(void* arg) {
    struct after_ctx* ctx = (struct after_ctx*)arg;
    sleep_ms(ctx->ms);
    if (ctx->ch) {
        if (ctx->ch->elem_size == 0) {
            uint8_t dummy = 0;
            __gost_chan_send(ctx->ch, &dummy);
        } else {
            void* buf = __gost_alloc(ctx->ch->elem_size, 1);
            memset(buf, 0, ctx->ch->elem_size);
            __gost_chan_send(ctx->ch, buf);
            __gost_free(buf, ctx->ch->elem_size, 1);
        }
    }
    free(ctx);
}

struct __gost_chan* __gost_after_ms(int64_t ms) {
    struct __gost_chan* ch = __gost_chan_new(0, 1);
    struct after_ctx* ctx = (struct after_ctx*)malloc(sizeof(*ctx));
    if (!ctx) {
        __gost_panic((const uint8_t*)"out of memory", 13);
    }
    ctx->ch = ch;
    ctx->ms = ms;
    __gost_spawn_thread(after_entry, ctx);
    return ch;
}
