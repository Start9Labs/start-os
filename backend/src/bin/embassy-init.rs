use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use embassy::context::rpc::RpcContextConfig;
use embassy::context::{DiagnosticContext, SetupContext};
use embassy::disk::fsck::RepairStrategy;
use embassy::disk::main::DEFAULT_PASSWORD;
use embassy::disk::REPAIR_DISK_PATH;
use embassy::init::STANDBY_MODE_PATH;
use embassy::middleware::cors::cors;
use embassy::middleware::diagnostic::diagnostic;
#[cfg(feature = "avahi")]
use embassy::net::mdns::MdnsController;
use embassy::shutdown::Shutdown;
use embassy::sound::CHIME;
use embassy::util::logger::EmbassyLogger;
use embassy::util::Invoke;
use embassy::{Error, ErrorKind, ResultExt};
use http::StatusCode;
use rpc_toolkit::rpc_server;
use tokio::process::Command;
use tracing::instrument;

fn status_fn(_: i32) -> StatusCode {
    StatusCode::OK
}

#[instrument]
async fn setup_or_init(cfg_path: Option<PathBuf>) -> Result<(), Error> {
    if tokio::fs::metadata("/media/embassy/config/disk.guid")
        .await
        .is_err()
    {
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
        tokio::time::sleep(Duration::from_secs(1)).await; // let the record state that I hate this
        CHIME.play().await?;
        rpc_server!({
            command: embassy::setup_api,
            context: ctx.clone(),
            status: status_fn,
            middleware: [
                cors,
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
        let guid_string = tokio::fs::read_to_string("/media/embassy/config/disk.guid") // unique identifier for volume group - keeps track of the disk that goes with your embassy
            .await?;
        let guid = guid_string.trim();
        let requires_reboot = embassy::disk::main::import(
            guid,
            cfg.datadir(),
            if tokio::fs::metadata(REPAIR_DISK_PATH).await.is_ok() {
                RepairStrategy::Aggressive
            } else {
                RepairStrategy::Preen
            },
            DEFAULT_PASSWORD,
        )
        .await?;
        if tokio::fs::metadata(REPAIR_DISK_PATH).await.is_ok() {
            tokio::fs::remove_file(REPAIR_DISK_PATH)
                .await
                .with_ctx(|_| (embassy::ErrorKind::Filesystem, REPAIR_DISK_PATH))?;
        }
        if requires_reboot.0 {
            embassy::disk::main::export(guid, cfg.datadir()).await?;
            Command::new("reboot")
                .invoke(embassy::ErrorKind::Unknown)
                .await?;
        }
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
async fn inner_main(cfg_path: Option<PathBuf>) -> Result<Option<Shutdown>, Error> {
    if tokio::fs::metadata(STANDBY_MODE_PATH).await.is_ok() {
        tokio::fs::remove_file(STANDBY_MODE_PATH).await?;
        Command::new("sync").invoke(ErrorKind::Filesystem).await?;
        embassy::sound::SHUTDOWN.play().await?;
        futures::future::pending::<()>().await;
    }

    embassy::sound::BEP.play().await?;

    run_script_if_exists("/media/embassy/config/preinit.sh").await;

    let res = if let Err(e) = setup_or_init(cfg_path.clone()).await {
        async move {
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

    run_script_if_exists("/media/embassy/config/postinit.sh").await;

    res
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
