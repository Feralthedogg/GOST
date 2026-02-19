// Purpose: Provide default binary entry for compiler-oriented CLI execution.
// Inputs/Outputs: Reads process args and returns process exit code from CLI dispatcher.
// Invariants: Main must not bypass centralized CLI argument/diagnostic handling.
// Gotchas: Keep behavior aligned with gs binary wrapper to avoid drift in user experience.

fn main() {
    let code = gost::cli::run_cli(std::env::args().skip(1));
    std::process::exit(code);
}
