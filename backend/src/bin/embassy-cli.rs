use clap::Arg;
use embassy::context::CliContext;
use embassy::util::logger::EmbassyLogger;
use embassy::version::{Current, VersionT};
use embassy::Error;
use rpc_toolkit::run_cli;
use rpc_toolkit::yajrc::RpcError;
use serde_json::Value;

lazy_static::lazy_static! {
    static ref VERSION_STRING: String = Current::new().semver().to_string();
}

fn inner_main() -> Result<(), Error> {
    run_cli!({
        command: embassy::main_api,
        app: app => app
            .name("Embassy CLI")
            .version(&**VERSION_STRING)
            .arg(
                clap::Arg::with_name("config")
                    .short('c')
                    .long("config")
                    .takes_value(true),
            )
            .arg(Arg::with_name("host").long("host").short('h').takes_value(true))
            .arg(Arg::with_name("proxy").long("proxy").short('p').takes_value(true)),
        context: matches => {
            EmbassyLogger::init();
            CliContext::init(matches)?
        },
        exit: |e: RpcError| {
            match e.data {
                Some(Value::String(s)) => eprintln!("{}: {}", e.message, s),
                Some(Value::Object(o)) => if let Some(Value::String(s)) = o.get("details") {
                    eprintln!("{}: {}", e.message, s);
                    if let Some(Value::String(s)) = o.get("debug") {
                        tracing::debug!("{}", s)
                    }
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
