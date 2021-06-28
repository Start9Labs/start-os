use clap::Arg;
use embassy::context::{CliContext, EitherContext};
use embassy::Error;
use rpc_toolkit::run_cli;

fn inner_main() -> Result<(), Error> {
    simple_logging::log_to_stderr(log::LevelFilter::Info);
    run_cli!(
        embassy::portable_api,
        app => app.name("Embassy SDK"),
        matches => EitherContext::Cli(CliContext::init(matches)?),
        |code| if code < 0 { 1 } else { code }
    )
}

fn main() {
    match inner_main() {
        Ok(_) => (),
        Err(e) => {
            eprintln!("{}", e.source);
            log::debug!("{:?}", e.source);
            drop(e.source);
            std::process::exit(e.kind as i32)
        }
    }
}
