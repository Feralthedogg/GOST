# Standard Library Reference

This page summarizes the shipped std modules under `std/`.

Notes:

- Modules are imported as `std/<name>` (or nested path).
- Many modules include helper functions intended as internal implementation detail; focus on the APIs listed here.

## std/error

Path: `std/error/error.gs`

Purpose: basic `error` construction and inspection.

Key API:

- `new(msg: string) -> error`
- `message(err: error) -> string`
- `is_nil(err: error) -> bool`
- `non_nil(err: error) -> bool`

## std/fmt

Path: `std/fmt/fmt.gs`

Purpose: simple text formatting and printing.

Key API:

- `print(msg: string)`
- `println(msg: string)`
- `sprint(args: []string) -> string`
- `printf(format: string, args: []string)`
- `sprintf(format: string, args: []string) -> string`
- argument helpers: `arg_i64`, `arg_i32`, `arg_f64`, `arg_f32`, `arg_bool`

Example:

```gs
module main

import "std/fmt"

fn main() -> i32 {
    fmt.println(string_concat("answer=", fmt.arg_i64(42)))
    return 0
}
```

## std/io

Path: `std/io/io.gs`

Types:

- `Reader { read_fn: fn(i32) -> Result[string, error] }`
- `Writer { write_fn: fn(string) -> Result[i64, error] }`

Key API:

- `new_reader(...) -> Reader`
- `new_writer(...) -> Writer`
- `eof() -> error`
- `read_all(r: Reader) -> Result[string, error]`
- `copy_all(r: Reader, w: Writer) -> Result[i64, error]`
- `copy_n(r: Reader, w: Writer, limit: i64) -> Result[i64, error]`

Example:

```gs
module main

import "std/io"

fn main() -> i32 {
    let r = io.new_reader(|_n: i32| Result.Ok[string, error](""))
    match io.read_all(r) {
        Result.Ok(_text) => { return 0 },
        Result.Err(_) => { return 1 },
        _ => { return 1 },
    }
}
```

## std/bytes

Path: `std/bytes/bytes.gs`

Purpose: byte-slice helpers (`[]u8`).

Key API:

- `len(b: []u8) -> i64`
- `clone(src: []u8) -> []u8`
- `from_string(s: string) -> []u8`
- `to_string(b: []u8) -> string`
- `equal(a: ref[[]u8], b: ref[[]u8]) -> bool`
- `has_prefix`, `has_suffix`, `contains`, `index`
- `repeat`, `trim_space`, `replace`
- `to_upper_ascii`, `to_lower_ascii`

## std/strings

Path: `std/strings/strings.gs`

Purpose: string utility functions.

Key API:

- `len`, `is_empty`
- `equal`
- `has_prefix`, `has_suffix`, `contains`, `index`, `count`
- `split`, `join`, `replace`
- `trim_space`
- `to_upper`, `to_lower`
- `repeat`

## std/strconv

Path: `std/strconv/strconv.gs`

Purpose: string <-> primitive conversion.

Key API:

- formatting: `format_i64`, `itoa`, `format_bool`, `format_f64`
- parsing: `parse_i64`, `atoi`, `parse_bool`, `parse_f64`, `atof`
- loose fallback variants: `parse_i64_loose`, `atoi_loose`, `parse_bool_loose`

## std/path

Path: `std/path/path.gs`

Purpose: lexical path manipulation.

Key API:

- `is_abs(p: string) -> bool`
- `clean(path: string) -> string`
- `base(path: string) -> string`
- `dir(path: string) -> string`
- `ext(path: string) -> string`
- `join2(a: string, b: string) -> string`
- `join(parts: []string) -> string`
- `split(path: string) -> []string` (currently minimal implementation)
- `walk(root: string, visit: fn(string) -> unit)` (currently root callback)

## std/os

Path: `std/os/os.gs`

Types:

- `ExecResult { code: i32, output: string }`

Key API:

- command execution: `exec`, `pipe`
- fs ops: `read_file`, `write_file`, `remove`, `mkdir`, `readdir`, `stat_size`
- env/process: `getenv`, `setenv`, `args`, `exit`
- cwd: `getwd`, `chdir`
- status helpers: `last_status`, `last_error`, `last_output`

Example:

```gs
module main

import "std/os"

fn main() -> i32 {
    match os.write_file("note.txt", "hello\n") {
        Result.Ok(_) => { return 0 },
        Result.Err(_) => { return 1 },
        _ => { return 1 },
    }
}
```

## std/time

Path: `std/time/time.gs`

Types:

- `Instant { ms: i64 }`
- `Duration { ns: i64 }`

Key API:

- clock: `now_ms`, `now`, `unix`, `from_unix`
- duration constructors: `nanoseconds`, `microseconds`, `milliseconds`, `seconds`, `minutes`, `hours`
- duration math: `add`, `sub`, `duration_ms`
- elapsed/wait: `since`, `since_ms`, `until`, `sleep_ms`, `sleep`, `after_ms`, `after_duration`
- text: `format_instant_ms`, `format_duration`, `parse_duration`

## std/sync

Path: `std/sync/sync.gs`

Types:

- `Mutex`, `RWMutex`, `WaitGroup`, `Once`

Key API:

- mutex: `new_mutex`, `lock`, `unlock`, `try_lock`
- rw mutex: `new_rw_mutex`, `r_lock`, `r_unlock`, `w_lock`, `w_unlock`
- waitgroup: `new_wait_group`, `add`, `done`, `wait`
- once: `new_once`, `do_once`

## std/context

Path: `std/context/context.gs`

Types:

- `Context { done: chan[unit], err: shared[error] }`
- `Cancel { ch: chan[unit] }`
- `WithCancel { ctx: Context, cancel: Cancel }`

Key API:

- `background() -> Context`
- `with_cancel(parent: Context) -> WithCancel`
- `with_timeout(parent: Context, ms: i32) -> WithCancel`
- `done(ctx: Context) -> chan[unit]`
- `err(ctx: Context) -> error`
- `cancel(c: Cancel)`
- helpers: `cancel_and_done`, `done_chan`

## std/log

Path: `std/log/log.gs`

Type:

- `Logger { prefix: string, min_level: i32 }`

Key API:

- levels: `level_debug`, `level_info`, `level_warn`, `level_error`
- constructors/config: `new`, `with_level`, `set_min_level`
- logging: `debug`, `info`, `warn`, `error`
- format variants: `debugf`, `infof`, `warnf`, `errorf`
- convenience: `print_debug`, `print_info`, `print_warn`, `print_error`

## std/sort

Path: `std/sort/sort.gs`

Purpose: insertion-sort based helpers.

Key API:

- generic: `sort_by[T]`, `is_sorted_by[T]`, `search_by[T]`
- specialized: `sort_i32`, `is_sorted_i32`, `search_i32`, `sort_i64`, `is_sorted_i64`, `search_i64`

## std/regexp

Path: `std/regexp/regexp.gs`

Type:

- `Regexp { pattern: string }`

Key API:

- compile: `compile`, `must_compile`
- match/find: `is_match`, `match_string`, `find_string`, `find`
- replace: `replace_all_string`, `replace_all`

Implemented engine is a compact subset (literal chars, `.`, `*`, `?`).

## std/net

Path: `std/net/net.gs`

Types:

- `TcpListener`, `TcpConn`, `TcpAccept`
- `UdpConn`, `UdpPacket`
- `HttpResponse`

Key API:

- tcp: `tcp_listen`, `tcp_accept`, `tcp_accept_with_peer`, `tcp_connect`, `tcp_close`, `tcp_close_listener`, `tcp_write`, `tcp_read`
- udp: `udp_bind`, `udp_connect`, `udp_close`, `udp_send`, `udp_send_to`, `udp_recv`, `udp_recv_from`
- http: `http_request`, `http_request_headers`, `http_get`, `http_post`, `https_request`, `https_get`, `https_post`
- status: `last_status`, `last_http_status`, `last_error`, `last_peer`

## std/net/socket

Path: `std/net/socket/socket.gs`

Type:

- `WebSocket { id: i64 }`

Key API:

- `ws_connect(url) -> Result[WebSocket, error]`
- `ws_close(ws) -> i32`
- `ws_send_text(ws, payload) -> Result[i32, error]`
- `ws_recv_text(ws) -> string`
- `ws_last_status() -> i32`

## std/encoding/base64

Path: `std/encoding/base64/base64.gs`

Key API:

- `encode_to_string`, `decode_string`
- raw/padded variants: `raw_std_encode_to_string`, `raw_std_decode_string`
- URL variants: `url_encode_to_string`, `url_decode_string`, `raw_url_encode_to_string`, `raw_url_decode_string`

## std/encoding/base32

Path: `std/encoding/base32/base32.gs`

Key API:

- `encode_to_string`, `decode_string`
- raw std variants: `raw_std_encode_to_string`, `raw_std_decode_string`
- hex alphabet variants: `hex_encode_to_string`, `hex_decode_string`, `raw_hex_encode_to_string`, `raw_hex_decode_string`

## std/encoding/hex

Path: `std/encoding/hex/hex.gs`

Key API:

- `encoded_len`, `decoded_len`
- `encode_to_string`, `decode_string`

## std/encoding/json

Path: `std/encoding/json/json.gs`

Type:

- `copy enum Json { Null, Bool(bool), Number(string), String(string), Array(string), Object(string) }`
- `Decoder`, `Encoder`

Key API:

- value conversion: `parse`, `stringify`, `marshal`, `unmarshal`, `valid`
- formatting: `compact`, `indent`, `marshal_indent`, `html_escape`
- streaming-like wrappers: `new_decoder`, `decode`, `decoder_use_number`, `decoder_disallow_unknown_fields`, `new_encoder`, `encoder_set_indent`, `encoder_set_escape_html`, `encode`

Example:

```gs
module main

import "std/encoding/json"
import "std/fmt"

fn main() -> i32 {
    let raw = "{\"ok\":true,\"n\":42}"
    match json.indent(raw, "", "  ") {
        Result.Ok(pretty) => {
            fmt.println(pretty)
            return 0
        },
        Result.Err(_) => { return 1 },
        _ => { return 1 },
    }
}
```

## std/encoding/toml

Path: `std/encoding/toml/toml.gs`

Type:

- `copy enum Toml { Null, Bool(bool), Number(string), String(string), Array(string), Table(string) }`
- `Decoder`, `Encoder`

Key API:

- `parse`, `stringify`, `marshal`, `unmarshal`, `valid`, `format`
- streaming-like wrappers: `new_decoder`, `decode`, `new_encoder`, `encoder_set_newline`, `encode`

## std/encoding/yaml

Path: `std/encoding/yaml/yaml.gs`

Type:

- `copy enum Yaml { Null, Bool(bool), Number(string), String(string), Seq(string), Map(string) }`
- `Decoder`, `Encoder`

Key API:

- `parse`, `stringify`, `marshal`, `unmarshal`, `valid`, `format`
- streaming-like wrappers: `new_decoder`, `decode`, `new_encoder`, `encoder_set_indent`, `encode`

## std/unicode

Path: `std/unicode/unicode.gs`

Purpose: ASCII-focused character classification and normalization helpers.

Key API:

- `is_letter`, `is_digit`, `is_space`
- `is_upper`, `is_lower`
- `to_upper`, `to_lower`

## std/bufio

Path: `std/bufio/bufio.gs`

Types:

- `Reader { src: string, pos: i64 }`
- `Scanner { src: string, pos: i64, token: string, done: bool, last_err: error }`

Key API:

- `new_reader`, `read_file`
- `remaining`, `is_eof`
- `read_bytes`, `read_line`
- `new_scanner`, `scan`, `text`, `err`

## std/flag

Path: `std/flag/flag.gs`

Type:

- `FlagSet { args_blob: string }`

Key API:

- `parse_args(args: []string) -> FlagSet`
- `parse() -> FlagSet` (reads argv via `__gost_os_args`)
- `has`, `string_value`, `bool_value`, `int_value`
- `positional`, `parse_errors`

## std/math

Path: `std/math/math.gs`

Key API:

- integer/float min/max/abs variants
- `floor`, `ceil`
- `pow`, `log2`

## std/math/rand

Path: `std/math/rand/rand.gs`

Type:

- `Rand { state: u64 }`

Key API:

- stateful: `new`, `seed`, `seed_now`, `next_u64`, `next_i64`, `intn`, `float64`
- global helpers: `global_seed`, `global_seed_now`, `global_u64`, `global_i64`, `global_intn`, `global_float64`

## std/hash

Path: `std/hash/hash.gs`

Key API:

- `fnv64(data: string) -> u64`
- `crc32(data: string) -> u32`

## std/hash/fnv

Path: `std/hash/fnv/fnv.gs`

Type:

- `Hash64`

Key API:

- `new64`
- `write_string`
- `sum64_state`
- `sum64`

## std/hash/crc32

Path: `std/hash/crc32/crc32.gs`

Type:

- `Digest`

Key API:

- `new_digest`
- `write_string`
- `sum32_state`
- `checksum`

## std/errors

Path: `std/errors/errors.gs`

Purpose: Go-style error helpers and wrapping.

Key API:

- `new`, `wrap`, `message`
- `is`, `unwrap`

## std/filepath

Path: `std/filepath/filepath.gs`

Purpose: path manipulation with OS-aware separator behavior.

Key API:

- `separator`, `is_sep`, `clean`
- `base`, `dir`, `ext`
- `join2`, `join`
- `is_abs`
- `to_slash`, `from_slash`

## std/container/list

Path: `std/container/list/list.gs`

Purpose: linked-list semantics backed by indexed node storage.

Key API:

- `new`, `len`, `is_empty`, `clear`
- `push_front`, `push_back`, `remove`
- `front_index`, `back_index`, `next_index`, `prev_index`
- `value`

## std/testing

Path: `std/testing/testing.gs`

Type:

- `T { name: string, failed: bool, logs: []string }`

Key API:

- lifecycle/logging: `new`, `log`, `fail`, `error`, `fatal`, `failed`, `logs`
- asserts: `assert_true`, `assert_false`, `assert_eq_i64`, `assert_eq_i32`, `assert_eq_string`
- runner: `run(name, test_fn)`

## std/compress/gzip

Path: `std/compress/gzip/gzip.gs`

Status:

- This module currently provides a stable placeholder frame codec, not DEFLATE.
- Frame format is `GZL0;` + raw payload.

Key API:

- `compress`, `decompress`
- aliases: `encode`, `decode`

## std/crypto/sha256

Path: `std/crypto/sha256/sha256.gs`

Key API:

- `sum_hex(data: string) -> string`
- `sum(data: string) -> string` (alias)

## std/net/http

Path: `std/net/http/http.gs`

Types:

- `Request`, `Response`, `Client`

Key API:

- request builders: `new_request`, `default_client`
- client calls: `do`, `get`, `post`

Example:

```gs
module main

import "std/fmt"
import "std/net/http"

fn main() -> i32 {
    match http.get("https://example.com") {
        Result.Ok(resp) => {
            fmt.println(string_concat("status=", fmt.arg_i32(resp.status)))
            return 0
        },
        Result.Err(_) => { return 1 },
        _ => { return 1 },
    }
}
```
