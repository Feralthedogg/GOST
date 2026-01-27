#ifndef GOSTRT_H
#define GOSTRT_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

void __gost_rt_init(void);
void __gost_panic(const uint8_t* msg, size_t len);
void __gost_println_str(const uint8_t* ptr, size_t len);

void* __gost_alloc(size_t size, size_t align);
void __gost_free(void* p, size_t size, size_t align);

typedef void (*__gost_thread_fn)(void* ctx);
void __gost_spawn_thread(__gost_thread_fn f, void* ctx);

struct __gost_chan;
struct __gost_slice;
struct __gost_shared;
struct __gost_map;

struct __gost_chan* __gost_chan_new(size_t elem_size, int32_t cap);
int32_t __gost_chan_send(struct __gost_chan* ch, const void* elem);
int32_t __gost_chan_recv(struct __gost_chan* ch, void* out_elem);
int32_t __gost_chan_close(struct __gost_chan* ch);
void __gost_chan_drop(struct __gost_chan* ch);
int32_t __gost_chan_can_send(struct __gost_chan* ch);
int32_t __gost_chan_can_recv(struct __gost_chan* ch);
int32_t __gost_select_wait(struct __gost_chan** chans, uint32_t n);

struct __gost_slice* __gost_slice_new(
    size_t elem_size,
    int64_t len,
    int64_t cap,
    void (*elem_drop)(void*)
);
void __gost_slice_drop(struct __gost_slice* s);
int64_t __gost_slice_len(struct __gost_slice* s);
void* __gost_slice_data(struct __gost_slice* s);
void __gost_slice_bounds_check(struct __gost_slice* s, int64_t i);
void __gost_slice_push(struct __gost_slice* s, const void* elem_bytes);
int32_t __gost_slice_pop(struct __gost_slice* s, void* out_elem_bytes);

struct __gost_shared* __gost_shared_new(
    size_t payload_size,
    void (*drop_payload)(void*),
    const void* payload_bytes
);
void __gost_shared_inc(struct __gost_shared* s);
void __gost_shared_dec(struct __gost_shared* s);
void* __gost_shared_get_ptr(struct __gost_shared* s);
int32_t __gost_shared_is_unique(struct __gost_shared* s);

struct __gost_map* __gost_map_new(int32_t key_kind, size_t val_size, int64_t cap);
int32_t __gost_map_get(struct __gost_map* m, const void* key_bytes, void* out_val_bytes);
void __gost_map_set(struct __gost_map* m, const void* key_bytes, const void* val_bytes);
int32_t __gost_map_del(struct __gost_map* m, const void* key_bytes);
int64_t __gost_map_len(struct __gost_map* m);
void __gost_map_drop(struct __gost_map* m);

struct __gost_chan* __gost_after_ms(int64_t ms);

typedef void (*__gost_go_fn)(void);
void __gost_go_spawn(__gost_go_fn f);

#ifdef __cplusplus
}
#endif

#endif
