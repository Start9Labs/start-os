use std::ffi::OsString;

use clap::Parser;
use futures::FutureExt;
use rpc_toolkit::CliApp;
use tokio::signal::unix::signal;
use tracing::instrument;

use crate::context::CliContext;
use crate::context::config::ClientConfig;
use crate::net::web_server::{Acceptor, WebServer};
use crate::prelude::*;
use crate::tunnel::context::{TunnelConfig, TunnelContext};
use crate::util::logger::LOGGER;

#[instrument(skip_all)]
async fn inner_main(config: &TunnelConfig) -> Result<(), Error> {
    let server = async {
        let ctx = TunnelContext::init(config).await?;
        let mut server = WebServer::new(Acceptor::bind([ctx.listen]).await?);
        server.serve_tunnel(ctx.clone());

        let mut shutdown_recv = ctx.shutdown.subscribe();

        let sig_handler_ctx = ctx;
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
                .send(())
                .map_err(|_| ())
                .expect("send shutdown signal");
        });

        shutdown_recv
            .recv()
            .await
            .with_kind(crate::ErrorKind::Unknown)?;

        sig_handler.abort();

        Ok::<_, Error>(server)
    }
    .await?;
    server.shutdown().await;

    Ok(())
}

pub fn main(args: impl IntoIterator<Item = OsString>) {
    LOGGER.enable();

    let config = TunnelConfig::parse_from(args).load().unwrap();

    let res = {
        let rt = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("failed to initialize runtime");
        rt.block_on(inner_main(&config))
    };

    match res {
        Ok(()) => (),
        Err(e) => {
            eprintln!("{}", e.source);
            tracing::debug!("{:?}", e.source);
            drop(e.source);
            std::process::exit(e.kind as i32)
        }
    }
}

pub fn cli(args: impl IntoIterator<Item = OsString>) {
    LOGGER.enable();

    if let Err(e) = CliApp::new(
        |cfg: ClientConfig| Ok(CliContext::init(cfg.load()?)?),
        crate::tunnel::api::tunnel_api(),
    )
    .run(args)
    {
        match e.data {
            Some(serde_json::Value::String(s)) => eprintln!("{}: {}", e.message, s),
            Some(serde_json::Value::Object(o)) => {
                if let Some(serde_json::Value::String(s)) = o.get("details") {
                    eprintln!("{}: {}", e.message, s);
                    if let Some(serde_json::Value::String(s)) = o.get("debug") {
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
