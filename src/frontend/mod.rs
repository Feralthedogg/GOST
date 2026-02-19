// Purpose: Frontend module hub that wires lexer/parser/diagnostics together.
// Inputs/Outputs: Exposes frontend components used by compile pipeline and tests.
// Invariants: Public frontend API should remain a thin composition over specialized modules.
// Gotchas: Keep exports minimal to avoid leaking unstable parser-internal contracts.

pub mod ast;
pub mod diagnostic;
pub mod lexer;
pub mod parser;
pub mod suggest;
pub mod symbols;
