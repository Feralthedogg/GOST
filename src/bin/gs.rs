// Purpose: Provide gs command entrypoint wrapper around shared CLI implementation.
// Inputs/Outputs: Forwards argv to CLI module and exits with returned status code.
// Invariants: Wrapper must stay thin so all behavior remains centralized in cli/mod.rs.
// Gotchas: Any flag or mode change belongs in cli/mod.rs, not this shim.

fn main() {
    let code = gost::cli::run_cli(std::env::args().skip(1));
    std::process::exit(code);
}
