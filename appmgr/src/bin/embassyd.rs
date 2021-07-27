use std::path::Path;
use std::time::Duration;

use embassy::context::{EitherContext, RpcContext};
use embassy::db::model::Database;
use embassy::middleware::cors::cors;
use embassy::status::{check_all, synchronize_all};
use embassy::util::daemon;
use embassy::{Error, ErrorKind};
use futures::TryFutureExt;
use patch_db::json_ptr::JsonPointer;
use rpc_toolkit::hyper::StatusCode;
use rpc_toolkit::rpc_server;

fn status_fn(_: i32) -> StatusCode {
    StatusCode::OK
}

async fn inner_main(cfg_path: Option<&str>) -> Result<(), Error> {
    let rpc_ctx = RpcContext::init(cfg_path).await?;
    if !rpc_ctx.db.exists(&<JsonPointer>::default()).await? {
        rpc_ctx
            .db
            .put(&<JsonPointer>::default(), &Database::init(), None)
            .await?;
    }
    let ctx = EitherContext::Rpc(rpc_ctx.clone());
    let server = rpc_server!({
        command: embassy::main_api,
        context: ctx,
        status: status_fn,
        middleware: [
            cors
        ]
    });
    let status_ctx = rpc_ctx.clone();
    let status_daemon = daemon(
        move || {
            let ctx = status_ctx.clone();
            async move {
                if let Err(e) = synchronize_all(&ctx).await {
                    log::error!("Error in Status Sync daemon: {}", e);
                    log::debug!("{:?}", e);
                } else {
                    log::info!("Status Sync completed successfully");
                }
            }
        },
        Duration::from_millis(500),
    );
    let health_ctx = rpc_ctx.clone();
    let health_daemon = daemon(
        move || {
            let ctx = health_ctx.clone();
            async move {
                if let Err(e) = check_all(&ctx).await {
                    log::error!("Error in Health Check daemon: {}", e);
                    log::debug!("{:?}", e);
                } else {
                    log::info!("Health Check completed successfully");
                }
            }
        },
        Duration::from_millis(500),
    );
    futures::try_join!(
        server.map_err(|e| Error::new(e, ErrorKind::Network)),
        status_daemon.map_err(|e| Error::new(
            e.context("Status Sync daemon panicked!"),
            ErrorKind::Unknown
        )),
        health_daemon.map_err(|e| Error::new(
            e.context("Health Check daemon panicked!"),
            ErrorKind::Unknown
        )),
    )?;
    Ok(())
}

fn main() {
    let matches = clap::App::new("embassyd")
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
        .get_matches();

    simple_logging::log_to_stderr(match matches.occurrences_of("verbosity") {
        0 => log::LevelFilter::Off,
        1 => log::LevelFilter::Error,
        2 => log::LevelFilter::Warn,
        3 => log::LevelFilter::Info,
        4 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    });
    let cfg_path = matches.value_of("config");
    let rt = tokio::runtime::Runtime::new().expect("failed to initialize runtime");
    match rt.block_on(inner_main(cfg_path)) {
        Ok(_) => (),
        Err(e) => {
            drop(rt);
            eprintln!("{}", e.source);
            log::debug!("{:?}", e.source);
            drop(e.source);
            std::process::exit(e.kind as i32)
        }
    }
}
