use std::ffi::OsString;

use clap::Parser;
use futures::{FutureExt, TryStreamExt};
use tokio::signal::unix::signal;
use tracing::instrument;

use crate::net::web_server::{Acceptor, WebServer};
use crate::prelude::*;
use crate::registry::context::{RegistryConfig, RegistryContext};
use crate::util::logger::LOGGER;

#[instrument(skip_all)]
async fn inner_main(config: &RegistryConfig) -> Result<(), Error> {
    let server = async {
        let ctx = RegistryContext::init(config).await?;
        let mut server = WebServer::new(Acceptor::bind([ctx.listen]).await?);
        server.serve_registry(ctx.clone());

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

    let config = RegistryConfig::parse_from(args).load().unwrap();

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
