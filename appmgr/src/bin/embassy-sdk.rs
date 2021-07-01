use clap::Arg;
use embassy::context::{CliContext, EitherContext};
use embassy::Error;
use rpc_toolkit::run_cli;

fn inner_main() -> Result<(), Error> {
    run_cli!(
        embassy::portable_api,
        app => app
            .name("Embassy SDK")
            .arg(
                clap::Arg::with_name("config")
                    .short("c")
                    .long("config")
                    .takes_value(true),
            )
            .arg(
                clap::Arg::with_name("verbosity")
                    .short("v")
                    .multiple(true)
                    .takes_value(false),
            ),
        matches => {
            simple_logging::log_to_stderr(match matches.occurrences_of("verbosity") {
                0 => log::LevelFilter::Off,
                1 => log::LevelFilter::Error,
                2 => log::LevelFilter::Warn,
                3 => log::LevelFilter::Info,
                4 => log::LevelFilter::Debug,
                _ => log::LevelFilter::Trace,
            });
            EitherContext::Cli(CliContext::init(matches)?)
        },
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
