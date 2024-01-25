use std::ffi::OsString;
use std::net::{Ipv6Addr, SocketAddr};
use std::path::Path;
use std::sync::Arc;

use clap::Parser;
use color_eyre::eyre::eyre;
use futures::{FutureExt, TryFutureExt};
use tokio::signal::unix::signal;
use tracing::instrument;

use crate::context::config::ServerConfig;
use crate::context::{DiagnosticContext, RpcContext};
use crate::net::web_server::WebServer;
use crate::shutdown::Shutdown;
use crate::system::launch_metrics_task;
use crate::util::logger::EmbassyLogger;
use crate::{Error, ErrorKind, ResultExt};

#[instrument(skip_all)]
async fn inner_main(config: &ServerConfig) -> Result<Option<Shutdown>, Error> {
    let (rpc_ctx, server, shutdown) = async {
        let rpc_ctx = RpcContext::init(
            config,
            Arc::new(
                tokio::fs::read_to_string("/media/embassy/config/disk.guid") // unique identifier for volume group - keeps track of the disk that goes with your embassy
                    .await?
                    .trim()
                    .to_owned(),
            ),
        )
        .await?;
        crate::hostname::sync_hostname(&rpc_ctx.account.read().await.hostname).await?;
        let server = WebServer::main(
            SocketAddr::new(Ipv6Addr::UNSPECIFIED.into(), 80),
            rpc_ctx.clone(),
        )?;

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

        crate::sound::CHIME.play().await?;

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

        Ok::<_, Error>((rpc_ctx, server, shutdown))
    }
    .await?;
    server.shutdown().await;
    rpc_ctx.shutdown().await?;

    tracing::info!("RPC Context is dropped");

    Ok(shutdown)
}

pub fn main(args: impl IntoIterator<Item = OsString>) {
    EmbassyLogger::init();

    let config = ServerConfig::parse_from(args).load().unwrap();

    if !Path::new("/run/embassy/initialized").exists() {
        super::start_init::main(&config);
        std::fs::write("/run/embassy/initialized", "").unwrap();
    }

    let res = {
        let rt = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("failed to initialize runtime");
        rt.block_on(async {
            match inner_main(&config).await {
                Ok(a) => Ok(a),
                Err(e) => {
                    async {
                        tracing::error!("{}", e.source);
                        tracing::debug!("{:?}", e.source);
                        crate::sound::BEETHOVEN.play().await?;
                        let ctx = DiagnosticContext::init(
                            &config,
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
                        )?;

                        let server = WebServer::diagnostic(
                            SocketAddr::new(Ipv6Addr::UNSPECIFIED.into(), 80),
                            ctx.clone(),
                        )?;

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
