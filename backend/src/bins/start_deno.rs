use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{command, run_cli, Context};
use serde_json::Value;

use crate::procedure::js_scripts::ExecuteArgs;
use crate::s9pk::manifest::PackageId;
use crate::util::serde::{display_serializable, parse_stdin_deserializable};
use crate::version::{Current, VersionT};
use crate::Error;

lazy_static::lazy_static! {
    static ref VERSION_STRING: String = Current::new().semver().to_string();
}

struct DenoContext;
impl Context for DenoContext {}

#[command(subcommands(execute, sandbox))]
fn deno_api() -> Result<(), Error> {
    Ok(())
}

#[command(cli_only, display(display_serializable))]
async fn execute(
    #[arg(stdin, parse(parse_stdin_deserializable))] arg: ExecuteArgs,
) -> Result<Result<Value, (i32, String)>, Error> {
    let ExecuteArgs {
        procedure,
        directory,
        pkg_id,
        pkg_version,
        name,
        volumes,
        input,
    } = arg;
    PackageLogger::init(&pkg_id);
    procedure
        .execute_impl(&directory, &pkg_id, &pkg_version, name, &volumes, input)
        .await
}
#[command(cli_only, display(display_serializable))]
async fn sandbox(
    #[arg(stdin, parse(parse_stdin_deserializable))] arg: ExecuteArgs,
) -> Result<Result<Value, (i32, String)>, Error> {
    let ExecuteArgs {
        procedure,
        directory,
        pkg_id,
        pkg_version,
        name,
        volumes,
        input,
    } = arg;
    PackageLogger::init(&pkg_id);
    procedure
        .sandboxed_impl(&directory, &pkg_id, &pkg_version, &volumes, input, name)
        .await
}

use tracing::Subscriber;
use tracing_subscriber::util::SubscriberInitExt;

#[derive(Clone)]
struct PackageLogger {}

impl PackageLogger {
    fn base_subscriber(id: &PackageId) -> impl Subscriber {
        use tracing_error::ErrorLayer;
        use tracing_subscriber::prelude::*;
        use tracing_subscriber::{fmt, EnvFilter};

        let filter_layer = EnvFilter::default()
            .add_directive(
                format!("{}=warn", std::module_path!().split("::").next().unwrap())
                    .parse()
                    .unwrap(),
            );
        let fmt_layer = fmt::layer().with_writer(std::io::stderr).with_target(true);
        let journald_layer = tracing_journald::layer()
            .unwrap()
            .with_syslog_identifier(format!("{id}.embassy"));

        let sub = tracing_subscriber::registry()
            .with(filter_layer)
            .with(fmt_layer)
            .with(journald_layer)
            .with(ErrorLayer::default());

        sub
    }
    pub fn init(id: &PackageId) -> Self {
        Self::base_subscriber(id).init();
        color_eyre::install().unwrap_or_else(|_| tracing::warn!("tracing too many times"));

        Self {}
    }
}

fn inner_main() -> Result<(), Error> {
    run_cli!({
        command: deno_api,
        app: app => app
            .name("StartOS Deno Executor")
            .version(&**VERSION_STRING),
        context: _m => DenoContext,
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

pub fn main() {
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
