use embassy::context::SdkContext;
use embassy::util::logger::EmbassyLogger;
use embassy::version::{Current, VersionT};
use embassy::Error;
use rpc_toolkit::run_cli;
use rpc_toolkit::yajrc::RpcError;
use serde_json::Value;

fn inner_main() -> Result<(), Error> {
    run_cli!({
        command: embassy::portable_api,
        app: app => app
            .name("Embassy SDK")
            .version(Current::new().semver().to_string().as_str())
            .arg(
                clap::Arg::with_name("config")
                    .short("c")
                    .long("config")
                    .takes_value(true),
            ),
        context: matches => {
            if let Err(_) = std::env::var("RUST_LOG") {
                std::env::set_var("RUST_LOG", "embassy=warn,js_engine=warn");
            }
            EmbassyLogger::init();
            SdkContext::init(matches)?
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
