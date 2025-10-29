use std::collections::BTreeMap;
use std::ffi::OsString;
use std::net::SocketAddr;
use std::time::Duration;

use clap::Parser;
use futures::FutureExt;
use helpers::NonDetachingJoinHandle;
use rpc_toolkit::CliApp;
use tokio::signal::unix::signal;
use tracing::instrument;
use visit_rs::Visit;

use crate::context::config::ClientConfig;
use crate::context::CliContext;
use crate::net::gateway::{Bind, BindTcp};
use crate::net::tls::{ChainedHandler, TlsListener};
use crate::net::web_server::{Accept, Acceptor, MetadataVisitor, WebServer};
use crate::prelude::*;
use crate::tunnel::context::{TunnelConfig, TunnelContext};
use crate::tunnel::tunnel_router;
use crate::util::logger::LOGGER;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum WebserverListener {
    Http,
    Https(SocketAddr),
}
impl<V: MetadataVisitor> Visit<V> for WebserverListener {
    fn visit(&self, visitor: &mut V) -> <V as visit_rs::Visitor>::Result {
        visitor.visit(self)
    }
}

#[instrument(skip_all)]
async fn inner_main(config: &TunnelConfig) -> Result<(), Error> {
    let server = async {
        let ctx = TunnelContext::init(config).await?;
        let listen = ctx.listen;
        let server = WebServer::new(
            Acceptor::bind_map_dyn([(WebserverListener::Http, listen)]).await?,
            tunnel_router(ctx.clone()),
        );
        let acceptor_setter = server.acceptor_setter();
        let https_db = ctx.db.clone();
        let https_thread: NonDetachingJoinHandle<()> = tokio::spawn(async move {
            let mut sub = https_db.subscribe("/webserver".parse().unwrap()).await;
            while sub.recv().await.is_some() {
                while let Err(e) = async {
                    if let Some(addr) = https_db.peek().await.as_webserver().de()? {
                        acceptor_setter.send_if_modified(|a| {
                            let key = WebserverListener::Https(addr);
                            if !a.contains_key(&key) {
                                match (|| {
                                    Ok::<_, Error>(TlsListener::new(
                                        BindTcp.bind(addr)?,
                                        BasicCertHandler(https_db.clone()),
                                    ))
                                })() {
                                    Ok(l) => {
                                        a.retain(|k, _| *k == WebserverListener::Http);
                                        a.insert(key, l.into_dyn());

                                        true
                                    }
                                    Err(e) => {
                                        tracing::error!("error adding ssl listener: {e}");
                                        tracing::debug!("{e:?}");

                                        false
                                    }
                                }
                            } else {
                                false
                            }
                        });
                    } else {
                        acceptor_setter.send_if_modified(|a| {
                            let before = a.len();
                            a.retain(|k, _| *k == WebserverListener::Http);
                            a.len() != before
                        });
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
        https_thread.wait_for_abort().await;

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
