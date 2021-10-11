use clap::Arg;
use embassy::context::CliContext;
use embassy::util::logger::EmbassyLogger;
use embassy::Error;
use rpc_toolkit::run_cli;
use rpc_toolkit::yajrc::RpcError;
use serde_json::Value;
use tracing::metadata::LevelFilter;

fn inner_main() -> Result<(), Error> {
    run_cli!({
        command: embassy::main_api,
        app: app => app
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
            .arg(Arg::with_name("proxy").long("proxy").short("p").takes_value(true)),
        context: matches => {
            EmbassyLogger::init(
                match matches.occurrences_of("verbosity") {
                    0 => LevelFilter::OFF,
                    1 => LevelFilter::ERROR,
                    2 => LevelFilter::WARN,
                    3 => LevelFilter::INFO,
                    4 => LevelFilter::DEBUG,
                    _ => LevelFilter::TRACE,
                },
                Default::default(),
                None,
                false,
                Default::default(),
            );
            CliContext::init(matches)?
        },
        exit: |e: RpcError| {
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
    });
    Ok(())
}

fn main() {
    match inner_main() {
        Ok(_) => (),
        Err(e) => {
            eprintln!("{}", e.source);
            tracing::debug!("{:?}", e.source);
            drop(e.source);
            std::process::exit(e.kind as i32)
        }
    }
}
