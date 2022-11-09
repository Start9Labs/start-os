use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use color_eyre::eyre::eyre;
use embassy::context::{DiagnosticContext, RpcContext};

use embassy::hostname::get_current_ip;
use embassy::net::embassy_service_http_server::EmbassyServiceHTTPServer;
#[cfg(feature = "avahi")]
use embassy::net::mdns::MdnsController;
use embassy::net::net_controller::NetController;
use embassy::net::net_utils::ResourceFqdn;
use embassy::net::static_server::diag_ui_file_router;
use embassy::net::tor::tor_health_check;
use embassy::shutdown::Shutdown;
use embassy::system::launch_metrics_task;
use embassy::util::daemon;
use embassy::util::logger::EmbassyLogger;
use embassy::{Error, ErrorKind, ResultExt};
use futures::{FutureExt, TryFutureExt};
use reqwest::{Client, Proxy};
use tokio::signal::unix::signal;
use tracing::instrument;

#[instrument]
async fn inner_main(cfg_path: Option<PathBuf>) -> Result<Option<Shutdown>, Error> {
    let (rpc_ctx, shutdown) = {
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
        NetController::setup_embassy_ui(rpc_ctx.clone()).await?;

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

        {
            let mut db = rpc_ctx.db.handle();
            let receipts = embassy::context::rpc::RpcSetHostNameReceipts::new(&mut db).await?;
            embassy::hostname::sync_hostname(&mut db, &receipts.hostname_receipts).await?;
        }

        let metrics_ctx = rpc_ctx.clone();
        let metrics_task = tokio::spawn(async move {
            launch_metrics_task(&metrics_ctx.metrics_cache, || {
                metrics_ctx.shutdown.subscribe()
            })
            .await
        });

        let tor_health_ctx = rpc_ctx.clone();
        let tor_client = Client::builder()
            .proxy(
                Proxy::http(format!(
                    "socks5h://{}:{}",
                    rpc_ctx.tor_socks.ip(),
                    rpc_ctx.tor_socks.port()
                ))
                .with_kind(crate::ErrorKind::Network)?,
            )
            .build()
            .with_kind(crate::ErrorKind::Network)?;
        let tor_health_daemon = daemon(
            move || {
                let ctx = tor_health_ctx.clone();
                let client = tor_client.clone();
                async move { tor_health_check(&client, &ctx.net_controller.tor).await }
            },
            Duration::from_secs(300),
            rpc_ctx.shutdown.subscribe(),
        );

        embassy::sound::CHIME.play().await?;

        futures::try_join!(
            metrics_task
                .map_err(|e| Error::new(
                    eyre!("{}", e).wrap_err("Metrics daemon panicked!"),
                    ErrorKind::Unknown
                ))
                .map_ok(|_| tracing::debug!("Metrics daemon Shutdown")),
            tor_health_daemon
                .map_err(|e| Error::new(
                    e.wrap_err("Tor Health daemon panicked!"),
                    ErrorKind::Unknown
                ))
                .map_ok(|_| tracing::debug!("Tor Health daemon Shutdown")),
        )?;

        let mut shutdown = shutdown_recv
            .recv()
            .await
            .with_kind(crate::ErrorKind::Unknown)?;

        sig_handler.abort();

        if let Some(shutdown) = &mut shutdown {
            drop(shutdown.db_handle.take());
        }

        (rpc_ctx, shutdown)
    };
    rpc_ctx.shutdown().await?;

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
                    (|| async {
                        tracing::error!("{}", e.source);
                        tracing::debug!("{:?}", e.source);
                        embassy::sound::BEETHOVEN.play().await?;
                        #[cfg(feature = "avahi")]
                        let _mdns = MdnsController::init().await?;
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

                        let embassy_ip = get_current_ip(ctx.ethernet_interface.to_owned()).await?;
                        let embassy_ip_fqdn: ResourceFqdn = embassy_ip.parse()?;
                        let embassy_fqdn: ResourceFqdn = "embassy.local".parse()?;

                        let diag_ui_handler = diag_ui_file_router(ctx.clone()).await?;

                        let mut diag_http_server =
                            EmbassyServiceHTTPServer::new([0, 0, 0, 0].into(), 80, None).await?;
                        diag_http_server
                            .add_svc_handler_mapping(embassy_ip_fqdn, diag_ui_handler.clone())
                            .await?;
                        diag_http_server
                            .add_svc_handler_mapping(embassy_fqdn, diag_ui_handler)
                            .await?;

                        let mut shutdown = ctx.shutdown.subscribe();

                        shutdown.recv().await.with_kind(crate::ErrorKind::Unknown)
                    })()
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
