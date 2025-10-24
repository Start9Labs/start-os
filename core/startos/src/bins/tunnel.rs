use std::ffi::OsString;
use std::time::Duration;

use clap::Parser;
use futures::FutureExt;
use helpers::NonDetachingJoinHandle;
use rpc_toolkit::CliApp;
use tokio::signal::unix::signal;
use tracing::instrument;

use crate::context::config::ClientConfig;
use crate::context::CliContext;
use crate::net::web_server::{Acceptor, WebServer};
use crate::prelude::*;
use crate::tunnel::context::{TunnelConfig, TunnelContext};
use crate::util::logger::LOGGER;

#[instrument(skip_all)]
async fn inner_main(config: &TunnelConfig) -> Result<(), Error> {
    let server = async {
        let ctx = TunnelContext::init(config).await?;
        let listen = ctx.listen;
        let mut server = WebServer::new(Acceptor::bind([listen]).await?);
        let https_thread: NonDetachingJoinHandle<()> = tokio::spawn(async move {
            let mut sub = setter_db.subscribe("/webserver".parse().unwrap()).await;
            while sub.recv().await.is_some() {
                while let Err(e) = async {
                    let external = setter_db.peek().await.into_webserver().de()?;

                    let mut bind_err = None;

                    setter.send_modify(|a| {
                        a.retain(|a, _| *a == listen || Some(*a) == external);
                        if let Some(external) = external {
                            if !a.contains_key(&external) {
                                match mio::net::TcpListener::bind(external) {
                                    Ok(l) => {
                                        a.insert(external, TcpListener::from_std(l.into()));
                                    }
                                    Err(e) => bind_err = Some(e),
                                }
                            }
                        }
                    });

                    if let Some(e) = bind_err {
                        return Err(e);
                    }

                    Ok::<_, Error>(())
                }
                .await
                {
                    tracing::error!("error updating webserver bind: {e}");
                    tracing::debug!("{e:?}");
                    tokio::time::sleep(Duration::from_secs(5)).await;
                }
            }
        })
        .into();
        server.serve_tunnel(ctx.clone());

        let mut shutdown_recv = ctx.shutdown.subscribe();

        let sig_handler_ctx = ctx;
        let sig_handler: NonDetachingJoinHandle<()> = tokio::spawn(async move {
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
        })
        .into();

        shutdown_recv
            .recv()
            .await
            .with_kind(crate::ErrorKind::Unknown)?;

        sig_handler.wait_for_abort().await;
        setter_thread.wait_for_abort().await;

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
