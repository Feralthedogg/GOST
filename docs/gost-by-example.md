# Gost by Example: HTTP JSON CLI

This tutorial builds a small CLI tool from scratch:

- initialize a module with `gost.mod`
- call HTTP with `std/net/http`
- pretty-print JSON with `std/encoding/json`
- handle errors and exit codes cleanly

## 1. Initialize a Project

```powershell
mkdir gost-status-check
cd gost-status-check
gs mod init github.com/you/gost-status-check
```

You should now have a `gost.mod` file in the project root.

## 2. Write `main.gs`

```gs
module main

import "std/encoding/json"
import "std/fmt"
import "std/net/http"
import "std/os"

fn usage() {
    fmt.println("usage: gs run main.gs <url>")
}

fn pretty_or_raw(body: string) -> string {
    if !json.valid(body) {
        return body
    }
    match json.indent(body, "", "  ") {
        Result.Ok(pretty) => { return pretty },
        Result.Err(_) => { return body },
        _ => { return body },
    }
}

fn run(url: string) -> i32 {
    match http.get(url) {
        Result.Ok(resp) => {
            fmt.println(string_concat("status=", fmt.arg_i32(resp.status)))
            fmt.println(pretty_or_raw(resp.body))
            return 0
        },
        Result.Err(err) => {
            let _ = err
            fmt.println("request failed")
            return 1
        },
        _ => {
            fmt.println("request failed: unknown result")
            return 1
        },
    }
}

fn main() -> i32 {
    let argv = os.args()
    if slice_len[string](&argv) < 2 as i64 {
        usage()
        return 1
    }
    let url = slice_get_copy[string](&argv, 1 as i64)
    return run(url)
}
```

## 3. Run It

```powershell
gs run main.gs https://httpbin.org/json
```

Expected behavior:

- prints HTTP status
- prints prettified JSON when response body is JSON
- exits `1` on network/parse errors

## 4. Optional: Add a Dependency

When you import a third-party module path, run:

```powershell
gs mod tidy
```

`gost.mod`/`gost.lock` will be updated with resolved dependencies.
For local development, use `replace` entries (see [Modules and Packages](modules-and-packages.md)).

## 5. Next

- [Standard Library Reference](stdlib-reference.md)
- [Modules and Packages](modules-and-packages.md)
- [Testing Guide](testing-guide.md)
