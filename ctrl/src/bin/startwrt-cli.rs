use rpc_toolkit::{serde_json, CliApp};
use serde_json::Value;
use startwrt_ctrl::{init_logging, main_api, CliContext};

pub fn main() {
    let _guard = init_logging("startwrt-cli");

    if let Err(e) = CliApp::new(|ctx: CliContext| Ok(ctx), main_api()).run(std::env::args_os()) {
        match e.data {
            Some(Value::String(s)) => eprintln!("{}: {}", e.message, s),
            Some(Value::Object(o)) => {
                if let Some(Value::String(s)) = o.get("details") {
                    eprintln!("{}: {}", e.message, s);
                    if let Some(Value::String(s)) = o.get("debug") {
                        tracing::debug!("{}", s)
                    }
                }
            }
            Some(a) => eprintln!("{}: {}", e.message, a),
            None => eprintln!("{}", e.message),
        }

        std::process::exit(e.code);
    }
}
