# Modules and Packages

Gost supports module/dependency workflows through `gost.mod` and `gost.lock`.

## 1. Files

### 1.1 `gost.mod` (TOML)

Shape (`src/pkg/modfile.rs`):

```toml
module = "github.com/you/project"

[[require]]
module = "github.com/org/lib"
version = "main"

[[replace]]
module = "github.com/org/lib"
path = "../local-lib"

[[source]]
module = "github.com/org/lib"
url = "https://github.com/org/lib.git"
```

Fields:

- `module`: main module path
- `require`: requested dependencies
- `replace`: local path override for a module
- `source`: explicit source URL override

### 1.2 `gost.lock` (JSON)

Shape (`src/pkg/lockfile.rs`):

```json
{
  "schema": 1,
  "main": { "module": "github.com/you/project" },
  "modules": [
    {
      "module": "github.com/org/lib",
      "source": "https://github.com/org/lib.git",
      "requested": "main",
      "rev": "<resolved-commit>",
      "checksum": null,
      "local": null
    }
  ]
}
```

## 2. Import Resolution Model

Implemented in `src/pkg/resolve.rs`.

Resolution order for `import "..."`:

1. `std/...` -> resolved under std root (`./std` or `GOST_HOME/std`)
2. imports under main module path -> resolved from project root
3. external module imports -> resolved via `require`/`lock`/cache checkout

Import path must resolve to a directory containing `.gs` files.

## 3. Cache

Default cache root comes from OS project cache location (`directories::ProjectDirs`).

Override with:

- `GOST_CACHE_DIR`

Cache includes mirrored VCS data and checked-out module trees.

## 4. Offline/Online Policy

- Global `--offline` disables any fetch/clone and has highest priority.
- `gs mod verify`, `gs mod download`, and `gs mod graph` accept `--online`.
- For `mod graph`, `--online` is optional and for richer status output; graph remains read-only.
- `run/build` reject `--online` (online behavior is controlled by module mode and offline flag).

## 5. Commands

### 5.1 `gs mod init [module]`

- Creates `gost.mod`
- Creates initial `gost.lock` if missing
- If module is omitted, attempts git-origin inference, then fallback placeholder

### 5.2 `gs mod tidy`

- Scans project `.gs` files
- Infers required module roots from imports
- Updates `gost.mod`
- In mod mode, refreshes lock by resolving modules

`--readonly` turns tidy into verification mode (no writes).

### 5.3 `gs mod verify`

Two-phase verification:

1. strict readonly tidy checks
2. resolve checks (offline by default, `--online` optional)

Does not mutate lock in verify mode.

### 5.4 `gs mod download`

- Prefetches dependency cache
- Default is cache-only/offline behavior
- `--online` allows fetch/clone when missing
- Does not update `gost.lock`

### 5.5 `gs mod graph`

Prints:

- main module
- direct requirements
- transitive lock entries (when available)
- optional cache presence details

## 6. Module Modes in `run/build`

`-mod=mod|readonly`:

- `mod`: may resolve and refresh lock
- `readonly`: requires lock consistency and refuses lock-missing/mismatch states

Defaults:

- `gs run`: `mod`
- `gs build`: `readonly`

## 7. Practical Workflow

1. `gs mod init github.com/you/project`
2. add imports in code
3. `gs mod tidy`
4. `gs mod verify --offline` (CI-friendly)
5. `gs run main.gs` / `gs build main.gs`
