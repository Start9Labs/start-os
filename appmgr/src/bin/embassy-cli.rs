use clap::Arg;
use embassy::context::{CliContext, EitherContext};
use embassy::Error;
use rpc_toolkit::run_cli;
use rpc_toolkit::yajrc::RpcError;
use serde_json::Value;

fn inner_main() -> Result<(), Error> {
    run_cli!(
        embassy::main_api,
        app => app
            .name("Embassy CLI")
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
            )
            .arg(Arg::with_name("host").long("host").short("h").takes_value(true))
            .arg(Arg::with_name("port").long("port").short("p").takes_value(true)),
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
        |e: RpcError| {
            match e.data {
                Some(Value::String(s)) => eprintln!("{}: {}", e.message, s),
                Some(Value::Object(o)) => if let Some(Value::String(s)) = o.get("details") {
                    eprintln!("{}: {}", e.message, s)
                }
                Some(a) => eprintln!("{}: {}", e.message, a),
                None => eprintln!("{}", e.message),
            }

            std::process::exit(e.code);
        }
    );
    Ok(())
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
