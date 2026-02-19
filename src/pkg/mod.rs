// Purpose: Package-management module root exposing modfile/lock/resolve command helpers.
// Inputs/Outputs: Re-exports package workflow components used by CLI and compile pipeline.
// Invariants: Public pkg API should keep command/resolve boundaries explicit.
// Gotchas: Avoid cyclic dependencies with compile/cli modules when extending pkg surface.

pub mod cache;
pub mod import_scan;
pub mod lockfile;
pub mod modcmd;
pub mod modfile;
pub mod resolve;
pub mod vcs;
