use std::collections::VecDeque;
use std::ffi::OsString;

use rpc_toolkit::{serde_json, CliApp};
use serde_json::Value;

use crate::{init_logging, main_api, CliArgs, CliContext};

pub fn main(args: VecDeque<OsString>) {
    // Intercept local-only subcommands before initializing the syslog logger —
    // these run on the serial console and don't need syslog, and the socket
    // may not exist yet during early boot (which would panic in init_logging).
    match args.front().and_then(|s| s.to_str()) {
        Some("init") => {
            if let Err(e) = crate::init::run_init() {
                eprintln!("init failed: {e}");
                std::process::exit(1);
            }
            return;
        }
        Some("flash") => {
            if let Err(e) = crate::flash::run_flash().map(|_| ()) {
                eprintln!("flash failed: {e}");
                std::process::exit(1);
            }
            return;
        }
        Some("manufacture") => {
            if let Err(e) = crate::flash::run_manufacture() {
                eprintln!("manufacture failed: {e}");
                std::process::exit(1);
            }
            return;
        }
        _ => {}
    }

    init_logging("startwrt-cli");

    // Reconstruct args with synthetic argv[0] for clap/rpc-toolkit
    let full_args = std::iter::once(OsString::from("startwrt-cli")).chain(args);

    if let Err(e) = CliApp::new(
        |args: CliArgs| CliContext::init(args).map_err(|e| e.into()),
        main_api(),
    )
    .run(full_args)
    {
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
