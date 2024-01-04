use std::ffi::OsString;

use rpc_toolkit::CliApp;
use serde_json::Value;

use crate::context::config::ClientConfig;
use crate::context::CliContext;
use crate::util::logger::EmbassyLogger;
use crate::version::{Current, VersionT};
use crate::Error;

lazy_static::lazy_static! {
    static ref VERSION_STRING: String = Current::new().semver().to_string();
}

pub fn main(args: impl IntoIterator<Item = OsString>) -> Result<(), Error> {
    EmbassyLogger::init();
    if let Err(e) = CliApp::new(
        |cfg: ClientConfig| Ok(CliContext::init(cfg.load()?)?),
        crate::main_api(),
    )
    .run(args)
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
    Ok(())
}
