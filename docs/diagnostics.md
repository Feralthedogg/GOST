# Diagnostics

## Code ranges

- E0xxx: lexer / parser
- E1xxx: name resolution / types (identifier, type name, member)
- E2xxx: modules / imports / packages
- E3xxx: type checking (mismatch, inference, constraints)
- E4xxx: runtime / backend (llvm / codegen)

## Common codes

- E1002: undefined name
- E1003: unknown type
- E1101: unknown field
- E1102: unknown method
- E1103: unknown enum variant
- E2001: import not found
- E2002: package empty (no .gs files)

## E1002

Undefined name.

This error is reported when a name is not found in the current scope.

Tips:
- Check spelling and case.
- Ensure the symbol is declared or imported.

## E1003

Unknown type.

This error is reported when a type name cannot be resolved.

Tips:
- Check spelling and case.
- Ensure the type is declared or imported.

## E1101

Unknown field.

This error is reported when a field access refers to a non-existent field on a struct.

Tips:
- Check the field name spelling.
- Inspect the struct definition to confirm available fields.

## E1102

Unknown method.

This error is reported when a method call does not resolve to a known method or receiver-first function.

Tips:
- Check spelling and case.
- Verify that the receiver type supports the method.

## E1103

Unknown enum variant.

This error is reported when an enum variant name does not exist.

Tips:
- Check spelling and case.
- Inspect the enum definition for valid variants.

## E2001

Import not found.

The import path could not be resolved to a directory.

Tips:
- Check the path for typos.
- Ensure the module is present in the cache or fetched (online mode).

## E2002

Package empty.

The import path exists but contains no `.gs` files.

Tips:
- Ensure the package directory contains `.gs` sources.
