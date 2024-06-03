use std::path::Path;
use std::sync::Arc;

use tokio::process::Command;
use tracing::instrument;

use crate::context::config::ServerConfig;
use crate::context::{DiagnosticContext, InitContext, InstallContext, SetupContext};
use crate::disk::fsck::RepairStrategy;
use crate::disk::main::DEFAULT_PASSWORD;
use crate::disk::REPAIR_DISK_PATH;
use crate::firmware::{check_for_firmware_update, update_firmware};
use crate::init::STANDBY_MODE_PATH;
use crate::net::web_server::WebServer;
use crate::shutdown::Shutdown;
use crate::util::Invoke;
use crate::{Error, ErrorKind, ResultExt, PLATFORM};

#[instrument(skip_all)]
async fn setup_or_init(
    server: &mut WebServer,
    config: &ServerConfig,
) -> Result<Option<Shutdown>, Error> {
    if let Some(firmware) = check_for_firmware_update()
        .await
        .map_err(|e| {
            tracing::warn!("Error checking for firmware update: {e}");
            tracing::debug!("{e:?}");
        })
        .ok()
        .and_then(|a| a)
    {
        let init_ctx = InitContext::init().await?;
        let handle = init_ctx.progress.handle();
        let mut update_phase = handle.add_phase("Updating Firmware".into(), Some(10));
        let mut reboot_phase = handle.add_phase("Rebooting".into(), Some(1));

        server.serve_init(init_ctx);

        update_phase.start();
        if let Err(e) = update_firmware(firmware).await {
            tracing::warn!("Error performing firmware update: {e}");
            tracing::debug!("{e:?}");
        } else {
            update_phase.complete();
            reboot_phase.start();
            return Ok(Some(Shutdown {
                export_args: None,
                restart: true,
            }));
        }
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

        server.serve_install(ctx.clone());

        ctx.shutdown
            .subscribe()
            .recv()
            .await
            .expect("context dropped");

        return Ok(Some(Shutdown {
            export_args: None,
            restart: true,
        }));
    } else if tokio::fs::metadata("/media/startos/config/disk.guid")
        .await
        .is_err()
    {
        let ctx = SetupContext::init(config)?;

        server.serve_setup(ctx.clone());

        let mut shutdown = ctx.shutdown.subscribe();
        shutdown.recv().await.expect("context dropped");

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

        run_script_if_exists("/media/startos/config/preinit.sh").await;
        crate::init::init(config).await?;
        run_script_if_exists("/media/startos/config/postinit.sh").await;
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
pub async fn main(
    server: &mut WebServer,
    config: &ServerConfig,
) -> Result<Option<Shutdown>, Error> {
    if &*PLATFORM == "raspberrypi" && tokio::fs::metadata(STANDBY_MODE_PATH).await.is_ok() {
        tokio::fs::remove_file(STANDBY_MODE_PATH).await?;
        Command::new("sync").invoke(ErrorKind::Filesystem).await?;
        crate::sound::SHUTDOWN.play().await?;
        futures::future::pending::<()>().await;
    }

    let res = match setup_or_init(server, config).await {
        Err(e) => {
            async move {
                tracing::error!("{e}");
                tracing::debug!("{e:?}");

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

                server.serve_diagnostic(ctx.clone());

                let shutdown = ctx.shutdown.subscribe().recv().await.unwrap();

                Ok(shutdown)
            }
            .await
        }
        Ok(s) => Ok(s),
    };

    res
}
