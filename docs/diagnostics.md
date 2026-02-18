# Diagnostics

This document is used by `gs explain` / `gs --explain`.

## Quick Usage

```text
gs explain E1101
gs --explain E1003
```

Supported shorthand keys:

- `field` -> `E1101`
- `method` -> `E1102`
- `variant` -> `E1103`
- `name` -> `E1002`
- `type` -> `E1003`
- `import` -> `E2001`
- `package-empty` -> `E2002`

## Code Families

- `E0xxx`: lexer/parser
- `E1xxx`: name/type/member resolution
- `E2xxx`: module/import/package
- `E3xxx`: type checking/inference/constraints
- `E4xxx`: backend/runtime compile issues

## Implemented Common Codes

- `E1002`: undefined name
- `E1003`: unknown type
- `E1004`: unknown function
- `E1101`: unknown field
- `E1102`: unknown method
- `E1103`: unknown enum variant
- `E1104`: receiver not addressable
- `E1105`: mutable receiver required
- `E2000`: module/resolve pipeline error wrapper
- `E2001`: import not found
- `E2002`: package exists but has no `.gs` files

## E1002

Undefined name.

Typical cause:

- symbol not declared in scope
- typo in local/global/imported symbol

Fix:

- declare the symbol before use
- import/select the symbol
- check spelling

## E1003

Unknown type.

Typical cause:

- typo in builtin/user type
- missing import or alias confusion

Fix:

- use a known builtin or declared type
- verify module import path and type name

## E1004

Unknown function.

Typical cause:

- calling undeclared function
- expected namespaced call but used bare name

Fix:

- declare/import function
- call through alias namespace when selective imports are used

## E1101

Unknown field.

Typical cause:

- field does not exist on struct
- field access on wrong value shape

Fix:

- verify struct definition
- correct field name

## E1102

Unknown method.

Typical cause:

- no matching receiver-first function / impl method
- typo in method name

Fix:

- check impl/trait methods for receiver type
- fix method spelling

## E1103

Unknown enum variant.

Typical cause:

- variant name typo
- wrong enum type prefix

Fix:

- use declared variant from the same enum

## E1104

Receiver not addressable.

Typical cause:

- calling ref/mutref receiver method on temporary expression

Example pattern:

```text
make().read()
```

Fix:

- bind temporary to local first, then call method

## E1105

Mutable receiver required.

Typical cause:

- calling mutating receiver method through non-mutable receiver value

Fix:

- use mutable binding path that permits mutable borrow

## E2000

Module/resolve stage error wrapper.

Typical cause:

- `gost.mod`/`gost.lock` mismatch
- offline mode + missing cache
- unresolved import roots

Fix:

- run `gs mod tidy`
- run `gs mod verify` (optionally `--online`)
- inspect module cache/network policy

## E2001

Import not found.

Typical cause:

- directory not present under std/main module/dependency module

Fix:

- correct import path
- ensure module is required/resolved

## E2002

Package empty.

Typical cause:

- directory exists but has no `.gs` files

Fix:

- add `.gs` source files in that package directory

## Non-coded Diagnostics

Many semantic errors currently appear as message-only diagnostics without a stable code.
Examples:

- assignment/return type mismatch
- borrow conflicts
- invalid map key type
- view escape violations
- loop/control-flow misuse

When triaging CI failures, always inspect full message text, span labels, and help/note lines.
