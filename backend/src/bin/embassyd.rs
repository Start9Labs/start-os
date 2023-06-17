use std::net::{Ipv6Addr, SocketAddr};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use color_eyre::eyre::eyre;
use embassy::context::{DiagnosticContext, RpcContext};
use embassy::net::web_server::WebServer;
use embassy::shutdown::Shutdown;
use embassy::system::launch_metrics_task;
use embassy::util::logger::EmbassyLogger;
use embassy::{Error, ErrorKind, ResultExt};
use futures::{FutureExt, TryFutureExt};
use tokio::signal::unix::signal;
use tracing::instrument;

#[instrument(skip_all)]
async fn inner_main(cfg_path: Option<PathBuf>) -> Result<Option<Shutdown>, Error> {
    let (rpc_ctx, server, shutdown) = {
        let rpc_ctx = RpcContext::init(
            cfg_path,
            Arc::new(
                tokio::fs::read_to_string("/media/embassy/config/disk.guid") // unique identifier for volume group - keeps track of the disk that goes with your embassy
                    .await?
                    .trim()
                    .to_owned(),
            ),
        )
        .await?;
        embassy::hostname::sync_hostname(&rpc_ctx.account.read().await.hostname).await?;
        let server = WebServer::main(
            SocketAddr::new(Ipv6Addr::UNSPECIFIED.into(), 80),
            rpc_ctx.clone(),
        )
        .await?;

        let mut shutdown_recv = rpc_ctx.shutdown.subscribe();

        let sig_handler_ctx = rpc_ctx.clone();
        let sig_handler = tokio::spawn(async move {
            use tokio::signal::unix::SignalKind;
            futures::future::select_all(
                [
                    SignalKind::interrupt(),
                    SignalKind::quit(),
                    SignalKind::terminate(),
                ]
                .iter()
                .map(|s| {
                    async move {
                        signal(*s)
                            .unwrap_or_else(|_| panic!("register {:?} handler", s))
                            .recv()
                            .await
                    }
                    .boxed()
                }),
            )
            .await;
            sig_handler_ctx
                .shutdown
                .send(None)
                .map_err(|_| ())
                .expect("send shutdown signal");
        });

        let metrics_ctx = rpc_ctx.clone();
        let metrics_task = tokio::spawn(async move {
            launch_metrics_task(&metrics_ctx.metrics_cache, || {
                metrics_ctx.shutdown.subscribe()
            })
            .await
        });

        embassy::sound::CHIME.play().await?;

        metrics_task
            .map_err(|e| {
                Error::new(
                    eyre!("{}", e).wrap_err("Metrics daemon panicked!"),
                    ErrorKind::Unknown,
                )
            })
            .map_ok(|_| tracing::debug!("Metrics daemon Shutdown"))
            .await?;

        let shutdown = shutdown_recv
            .recv()
            .await
            .with_kind(crate::ErrorKind::Unknown)?;

        sig_handler.abort();

        (rpc_ctx, server, shutdown)
    };
    server.shutdown().await;
    rpc_ctx.shutdown().await?;

    tracing::info!("RPC Context is dropped");

    Ok(shutdown)
}

fn main() {
    let matches = clap::App::new("embassyd")
        .arg(
            clap::Arg::with_name("config")
                .short('c')
                .long("config")
                .takes_value(true),
        )
        .get_matches();

    EmbassyLogger::init();

    let cfg_path = matches.value_of("config").map(|p| Path::new(p).to_owned());

    let res = {
        let rt = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("failed to initialize runtime");
        rt.block_on(async {
            match inner_main(cfg_path.clone()).await {
                Ok(a) => Ok(a),
                Err(e) => {
                    async {
                        tracing::error!("{}", e.source);
                        tracing::debug!("{:?}", e.source);
                        embassy::sound::BEETHOVEN.play().await?;
                        let ctx = DiagnosticContext::init(
                            cfg_path,
                            if tokio::fs::metadata("/media/embassy/config/disk.guid")
                                .await
                                .is_ok()
                            {
                                Some(Arc::new(
                                    tokio::fs::read_to_string("/media/embassy/config/disk.guid") // unique identifier for volume group - keeps track of the disk that goes with your embassy
                                        .await?
                                        .trim()
                                        .to_owned(),
                                ))
                            } else {
                                None
                            },
                            e,
                        )
                        .await?;

                        let server = WebServer::diagnostic(
                            SocketAddr::new(Ipv6Addr::UNSPECIFIED.into(), 80),
                            ctx.clone(),
                        )
                        .await?;

                        let mut shutdown = ctx.shutdown.subscribe();

                        let shutdown =
                            shutdown.recv().await.with_kind(crate::ErrorKind::Unknown)?;

                        server.shutdown().await;

                        Ok::<_, Error>(shutdown)
                    }
                    .await
                }
            }
        })
    };

    match res {
        Ok(None) => (),
        Ok(Some(s)) => s.execute(),
        Err(e) => {
            eprintln!("{}", e.source);
            tracing::debug!("{:?}", e.source);
            drop(e.source);
            std::process::exit(e.kind as i32)
        }
    }
}
