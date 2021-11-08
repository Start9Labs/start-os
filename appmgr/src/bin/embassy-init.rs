use std::path::Path;
use std::sync::Arc;

use embassy::context::rpc::RpcContextConfig;
use embassy::context::{DiagnosticContext, SetupContext};
use embassy::disk::main::DEFAULT_PASSWORD;
use embassy::middleware::cors::cors;
use embassy::middleware::diagnostic::diagnostic;
use embassy::middleware::encrypt::encrypt;
#[cfg(feature = "avahi")]
use embassy::net::mdns::MdnsController;
use embassy::shutdown::Shutdown;
use embassy::sound::MARIO_COIN;
use embassy::util::logger::EmbassyLogger;
use embassy::util::Invoke;
use embassy::{Error, ResultExt};
use http::StatusCode;
use rpc_toolkit::rpc_server;
use tokio::process::Command;
use tracing::instrument;

fn status_fn(_: i32) -> StatusCode {
    StatusCode::OK
}

#[instrument]
async fn setup_or_init(cfg_path: Option<&str>) -> Result<(), Error> {
    embassy::disk::util::mount("LABEL=EMBASSY", "/embassy-os").await?;
    if tokio::fs::metadata("/embassy-os/disk.guid").await.is_err() {
        #[cfg(feature = "avahi")]
        let _mdns = MdnsController::init();
        tokio::fs::write(
            "/etc/nginx/sites-available/default",
            include_str!("../nginx/setup-wizard.conf"),
        )
        .await
        .with_ctx(|_| {
            (
                embassy::ErrorKind::Filesystem,
                "/etc/nginx/sites-available/default",
            )
        })?;
        Command::new("systemctl")
            .arg("reload")
            .arg("nginx")
            .invoke(embassy::ErrorKind::Nginx)
            .await?;
        let ctx = SetupContext::init(cfg_path).await?;
        let keysource_ctx = ctx.clone();
        let keysource = move || {
            let ctx = keysource_ctx.clone();
            async move { ctx.product_key().await }
        };
        let encrypt = encrypt(keysource);
        MARIO_COIN.play().await?;
        rpc_server!({
            command: embassy::setup_api,
            context: ctx.clone(),
            status: status_fn,
            middleware: [
                cors,
                encrypt,
            ]
        })
        .with_graceful_shutdown({
            let mut shutdown = ctx.shutdown.subscribe();
            async move {
                shutdown.recv().await.expect("context dropped");
            }
        })
        .await
        .with_kind(embassy::ErrorKind::Network)?;
    } else {
        let cfg = RpcContextConfig::load(cfg_path).await?;
        embassy::disk::main::import(
            tokio::fs::read_to_string("/embassy-os/disk.guid") // unique identifier for volume group - keeps track of the disk that goes with your embassy
                .await?
                .trim(),
            cfg.datadir(),
            DEFAULT_PASSWORD,
        )
        .await?;
        tracing::info!("Loaded Disk");
        embassy::init::init(&cfg).await?;
    }

    Ok(())
}

async fn run_script_if_exists<P: AsRef<Path>>(path: P) {
    let script = path.as_ref();
    if script.exists() {
        match Command::new("/bin/bash").arg(script).spawn() {
            Ok(mut c) => {
                if let Err(e) = c.wait().await {
                    tracing::error!("Error Running {}: {}", script.display(), e);
                    tracing::debug!("{:?}", e);
                }
            }
            Err(e) => {
                tracing::error!("Error Running {}: {}", script.display(), e);
                tracing::debug!("{:?}", e);
            }
        }
    }
}

#[instrument]
async fn inner_main(cfg_path: Option<&str>) -> Result<Option<Shutdown>, Error> {
    embassy::sound::BEP.play().await?;

    run_script_if_exists("/embassy-os/preinit.sh").await;

    let res = if let Err(e) = setup_or_init(cfg_path).await {
        async {
            tracing::error!("{}", e.source);
            tracing::debug!("{}", e.source);
            embassy::sound::BEETHOVEN.play().await?;
            #[cfg(feature = "avahi")]
            let _mdns = MdnsController::init();
            tokio::fs::write(
                "/etc/nginx/sites-available/default",
                include_str!("../nginx/diagnostic-ui.conf"),
            )
            .await
            .with_ctx(|_| {
                (
                    embassy::ErrorKind::Filesystem,
                    "/etc/nginx/sites-available/default",
                )
            })?;
            Command::new("systemctl")
                .arg("reload")
                .arg("nginx")
                .invoke(embassy::ErrorKind::Nginx)
                .await?;
            let ctx = DiagnosticContext::init(
                cfg_path,
                if tokio::fs::metadata("/embassy-os/disk.guid").await.is_ok() {
                    Some(Arc::new(
                        tokio::fs::read_to_string("/embassy-os/disk.guid") // unique identifier for volume group - keeps track of the disk that goes with your embassy
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
            let mut shutdown_recv = ctx.shutdown.subscribe();
            rpc_server!({
                command: embassy::diagnostic_api,
                context: ctx.clone(),
                status: status_fn,
                middleware: [
                    cors,
                    diagnostic,
                ]
            })
            .with_graceful_shutdown({
                let mut shutdown = ctx.shutdown.subscribe();
                async move {
                    shutdown.recv().await.expect("context dropped");
                }
            })
            .await
            .with_kind(embassy::ErrorKind::Network)?;

            Ok::<_, Error>(
                shutdown_recv
                    .recv()
                    .await
                    .with_kind(embassy::ErrorKind::Network)?,
            )
        }
        .await
    } else {
        Ok(None)
    };

    run_script_if_exists("/embassy-os/postinit.sh").await;

    res
}

fn main() {
    let matches = clap::App::new("embassyd")
        .arg(
            clap::Arg::with_name("config")
                .short("c")
                .long("config")
                .takes_value(true),
        )
        .get_matches();

    EmbassyLogger::no_sharing();

    let cfg_path = matches.value_of("config");
    let res = {
        let rt = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("failed to initialize runtime");
        rt.block_on(inner_main(cfg_path))
    };

    match res {
        Ok(Some(shutdown)) => shutdown.execute(),
        Ok(None) => (),
        Err(e) => {
            eprintln!("{}", e.source);
            tracing::debug!("{:?}", e.source);
            drop(e.source);
            std::process::exit(e.kind as i32)
        }
    }
}
