use std::time::Duration;

use embassy::context::{EitherContext, RpcContext};
use embassy::db::model::Database;
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

async fn inner_main() -> Result<(), Error> {
    simple_logging::log_to_stderr(log::LevelFilter::Info);
    let rpc_ctx = RpcContext::init().await?;
    if !rpc_ctx.db.exists(&<JsonPointer>::default()).await? {
        rpc_ctx
            .db
            .put(&<JsonPointer>::default(), &Database::init(), None)
            .await?;
    }
    let ctx = EitherContext::Rpc(rpc_ctx.clone());
    let server = rpc_server!(embassy::main_api, ctx, status_fn);
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
    let rt = tokio::runtime::Runtime::new().expect("failed to initialize runtime");
    match rt.block_on(inner_main()) {
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
