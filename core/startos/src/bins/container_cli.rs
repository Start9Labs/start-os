use std::ffi::OsString;

use rpc_toolkit::CliApp;
use serde_json::Value;

use crate::service::cli::{ContainerCliContext, ContainerClientConfig};
use crate::util::logger::LOGGER;
use crate::version::{Current, VersionT};

lazy_static::lazy_static! {
    static ref VERSION_STRING: String = Current::default().semver().to_string();
}

pub fn main(args: impl IntoIterator<Item = OsString>) {
    LOGGER.enable();
    if let Err(e) = CliApp::new(
        |cfg: ContainerClientConfig| Ok(ContainerCliContext::init(cfg)),
        crate::service::effects::handler(),
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
}
