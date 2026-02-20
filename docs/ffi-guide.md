# FFI Guide (C Interop)

This guide covers practical C interop in Gost:

- declaring external functions/globals
- linking native libraries
- layout compatibility (`repr`, `pack`, `bitfield`)
- generating bindings with `gs bindgen`

## 1. Minimal Function Call

`main.gs`:

```gs
module main

extern "C" fn native_add(a: i64, b: i64) -> i64

unsafe fn call_native() -> i64 {
    return native_add(20 as i64, 22 as i64)
}

fn main() -> i32 {
    let v = unsafe { call_native() }
    if v != 42 as i64 {
        return 1
    }
    return 0
}
```

`native.c`:

```c
#include <stdint.h>

int64_t native_add(int64_t a, int64_t b) {
    return a + b;
}
```

## 2. Build and Link

Build native archive:

```powershell
clang -c native.c -o native.o
ar rcs libnative.a native.o
```

Pass link flags through `GOST_LDFLAGS`:

```powershell
$env:GOST_LDFLAGS="-L. -lnative"
gs run main.gs
```

Notes:

- `gs build` emits LLVM IR (`.ll`) only.
- Final native linking happens in `gs run`.
- `GOST_LDFLAGS` is whitespace-split; pass plain tokens (`-L... -l...`).

## 3. Extern Globals and `unsafe`

Accessing extern globals is unsafe:

```gs
extern "C" let native_counter: i64

unsafe fn inc() {
    native_counter = native_counter + 1 as i64
}
```

Keep reads/writes wrapped in small `unsafe` helper functions.

## 4. Layout Compatibility for Structs/Enums

Use explicit layout attributes for C-compatible memory layout.

```gs
repr(C) pack(1) struct Header {
    tag: u16
    len: u32
}

repr(C) bitfield struct Flags {
    a: u32
    b: u32
}

repr(u8) enum Kind {
    A
    B
}
```

Rules:

- `pack(N)` and `bitfield` require `repr(C)`.
- `repr(transparent)` is struct-only and must have exactly one field.
- integer `repr(...)` is enum-only.

## 5. Generate Bindings with `gs bindgen`

For simple C headers:

```powershell
gs bindgen native.h -o native.gs --module native --abi C
```

Then import generated declarations:

```gs
import "native"
```

## 6. ABI Strings

Common ABI values:

- `C`
- `system`
- `stdcall`
- `fastcall`
- `vectorcall`
- `thiscall`
- `win64`
- `sysv64`
- `aapcs`

Pick the ABI that matches the C side declaration and platform.

## 7. Troubleshooting

- Undefined symbols at link:
  - check `GOST_LDFLAGS` (`-L<dir> -l<name>`)
  - verify archive/import library naming
- ABI mismatch crashes:
  - verify extern ABI string and parameter/return types
- Struct field mismatch:
  - add `repr(C)` and required packing attributes
- Still failing:
  - build with `gs run` and inspect full linker command error output
