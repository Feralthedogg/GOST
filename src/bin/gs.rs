fn main() {
    let code = gost::cli::run_cli(std::env::args().skip(1));
    std::process::exit(code);
}
