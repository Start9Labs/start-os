use std::net::{Ipv6Addr, SocketAddr};
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use helpers::NonDetachingJoinHandle;
use tokio::process::Command;
use tracing::instrument;

use crate::context::config::ServerConfig;
use crate::context::{DiagnosticContext, InstallContext, SetupContext};
use crate::disk::fsck::{RepairStrategy, RequiresReboot};
use crate::disk::main::DEFAULT_PASSWORD;
use crate::disk::REPAIR_DISK_PATH;
use crate::firmware::update_firmware;
use crate::init::STANDBY_MODE_PATH;
use crate::net::web_server::WebServer;
use crate::shutdown::Shutdown;
use crate::sound::{BEP, CHIME};
use crate::util::Invoke;
use crate::{Error, ErrorKind, ResultExt, PLATFORM};

#[instrument(skip_all)]
async fn setup_or_init(config: &ServerConfig) -> Result<Option<Shutdown>, Error> {
    let song = NonDetachingJoinHandle::from(tokio::spawn(async {
        loop {
            BEP.play().await.unwrap();
            BEP.play().await.unwrap();
            tokio::time::sleep(Duration::from_secs(30)).await;
        }
    }));

    match update_firmware().await {
        Ok(RequiresReboot(true)) => {
            return Ok(Some(Shutdown {
                export_args: None,
                restart: true,
            }))
        }
        Err(e) => {
            tracing::warn!("Error performing firmware update: {e}");
            tracing::debug!("{e:?}");
        }
        _ => (),
    }

    Command::new("ln")
        .arg("-sf")
        .arg("/usr/lib/startos/scripts/fake-apt")
        .arg("/usr/local/bin/apt")
        .invoke(crate::ErrorKind::Filesystem)
        .await?;
    Command::new("ln")
        .arg("-sf")
        .arg("/usr/lib/startos/scripts/fake-apt")
        .arg("/usr/local/bin/apt-get")
        .invoke(crate::ErrorKind::Filesystem)
        .await?;
    Command::new("ln")
        .arg("-sf")
        .arg("/usr/lib/startos/scripts/fake-apt")
        .arg("/usr/local/bin/aptitude")
        .invoke(crate::ErrorKind::Filesystem)
        .await?;

    Command::new("make-ssl-cert")
        .arg("generate-default-snakeoil")
        .arg("--force-overwrite")
        .invoke(crate::ErrorKind::OpenSsl)
        .await?;

    if tokio::fs::metadata("/run/live/medium").await.is_ok() {
        Command::new("sed")
            .arg("-i")
            .arg("s/PasswordAuthentication no/PasswordAuthentication yes/g")
            .arg("/etc/ssh/sshd_config")
            .invoke(crate::ErrorKind::Filesystem)
            .await?;
        Command::new("systemctl")
            .arg("reload")
            .arg("ssh")
            .invoke(crate::ErrorKind::OpenSsh)
            .await?;

        let ctx = InstallContext::init().await?;

        let server = WebServer::install(
            SocketAddr::new(Ipv6Addr::UNSPECIFIED.into(), 80),
            ctx.clone(),
        )?;

        drop(song);
        tokio::time::sleep(Duration::from_secs(1)).await; // let the record state that I hate this
        CHIME.play().await?;

        ctx.shutdown
            .subscribe()
            .recv()
            .await
            .expect("context dropped");

        server.shutdown().await;

        Command::new("reboot")
            .invoke(crate::ErrorKind::Unknown)
            .await?;
    } else if tokio::fs::metadata("/media/startos/config/disk.guid")
        .await
        .is_err()
    {
        let ctx = SetupContext::init(config)?;

        let server = WebServer::setup(
            SocketAddr::new(Ipv6Addr::UNSPECIFIED.into(), 80),
            ctx.clone(),
        )?;

        drop(song);
        tokio::time::sleep(Duration::from_secs(1)).await; // let the record state that I hate this
        CHIME.play().await?;

        let mut shutdown = ctx.shutdown.subscribe();
        shutdown.recv().await.expect("context dropped");

        server.shutdown().await;

        drop(shutdown);

        tokio::task::yield_now().await;
        if let Err(e) = Command::new("killall")
            .arg("firefox-esr")
            .invoke(ErrorKind::NotFound)
            .await
        {
            tracing::error!("Failed to kill kiosk: {}", e);
            tracing::debug!("{:?}", e);
        }
    } else {
        let guid_string = tokio::fs::read_to_string("/media/startos/config/disk.guid") // unique identifier for volume group - keeps track of the disk that goes with your embassy
            .await?;
        let guid = guid_string.trim();
        let requires_reboot = crate::disk::main::import(
            guid,
            config.datadir(),
            if tokio::fs::metadata(REPAIR_DISK_PATH).await.is_ok() {
                RepairStrategy::Aggressive
            } else {
                RepairStrategy::Preen
            },
            if guid.ends_with("_UNENC") {
                None
            } else {
                Some(DEFAULT_PASSWORD)
            },
        )
        .await?;
        if tokio::fs::metadata(REPAIR_DISK_PATH).await.is_ok() {
            tokio::fs::remove_file(REPAIR_DISK_PATH)
                .await
                .with_ctx(|_| (crate::ErrorKind::Filesystem, REPAIR_DISK_PATH))?;
        }
        if requires_reboot.0 {
            crate::disk::main::export(guid, config.datadir()).await?;
            Command::new("reboot")
                .invoke(crate::ErrorKind::Unknown)
                .await?;
        }
        tracing::info!("Loaded Disk");
        crate::init::init(config).await?;
        drop(song);
    }

    Ok(None)
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

#[instrument(skip_all)]
async fn inner_main(config: &ServerConfig) -> Result<Option<Shutdown>, Error> {
    if &*PLATFORM == "raspberrypi" && tokio::fs::metadata(STANDBY_MODE_PATH).await.is_ok() {
        tokio::fs::remove_file(STANDBY_MODE_PATH).await?;
        Command::new("sync").invoke(ErrorKind::Filesystem).await?;
        crate::sound::SHUTDOWN.play().await?;
        futures::future::pending::<()>().await;
    }

    crate::sound::BEP.play().await?;

    run_script_if_exists("/media/startos/config/preinit.sh").await;

    let res = match setup_or_init(config).await {
        Err(e) => {
            async move {
                tracing::error!("{}", e.source);
                tracing::debug!("{}", e.source);
                crate::sound::BEETHOVEN.play().await?;

                let ctx = DiagnosticContext::init(
                    config,
                    if tokio::fs::metadata("/media/startos/config/disk.guid")
                        .await
                        .is_ok()
                    {
                        Some(Arc::new(
                            tokio::fs::read_to_string("/media/startos/config/disk.guid") // unique identifier for volume group - keeps track of the disk that goes with your embassy
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

                let shutdown = ctx.shutdown.subscribe().recv().await.unwrap();

                server.shutdown().await;

                Ok(shutdown)
            }
            .await
        }
        Ok(s) => Ok(s),
    };

    run_script_if_exists("/media/startos/config/postinit.sh").await;

    res
}

pub fn main(config: &ServerConfig) {
    let res = {
        let rt = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("failed to initialize runtime");
        rt.block_on(inner_main(config))
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
