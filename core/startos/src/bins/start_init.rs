use std::sync::Arc;

use tokio::process::Command;
use tracing::instrument;

use crate::context::config::ServerConfig;
use crate::context::rpc::InitRpcContextPhases;
use crate::context::{DiagnosticContext, InitContext, InstallContext, RpcContext, SetupContext};
use crate::disk::fsck::RepairStrategy;
use crate::disk::main::DEFAULT_PASSWORD;
use crate::disk::REPAIR_DISK_PATH;
use crate::firmware::{check_for_firmware_update, update_firmware};
use crate::init::{InitPhases, InitResult, STANDBY_MODE_PATH};
use crate::net::web_server::WebServer;
use crate::prelude::*;
use crate::progress::FullProgressTracker;
use crate::shutdown::Shutdown;
use crate::util::Invoke;
use crate::PLATFORM;

#[instrument(skip_all)]
async fn setup_or_init(
    server: &mut WebServer,
    config: &ServerConfig,
) -> Result<Result<(RpcContext, FullProgressTracker), Shutdown>, Error> {
    if let Some(firmware) = check_for_firmware_update()
        .await
        .map_err(|e| {
            tracing::warn!("Error checking for firmware update: {e}");
            tracing::debug!("{e:?}");
        })
        .ok()
        .and_then(|a| a)
    {
        let init_ctx = InitContext::init(config).await?;
        let handle = &init_ctx.progress;
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
            return Ok(Err(Shutdown {
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

        return Ok(Err(Shutdown {
            export_args: None,
            restart: true,
        }));
    }

    if tokio::fs::metadata("/media/startos/config/disk.guid")
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

        Ok(Ok(match ctx.result.get() {
            Some(Ok((_, rpc_ctx))) => (rpc_ctx.clone(), ctx.progress.clone()),
            Some(Err(e)) => return Err(e.clone_output()),
            None => {
                return Err(Error::new(
                    eyre!("Setup mode exited before setup completed"),
                    ErrorKind::Unknown,
                ))
            }
        }))
    } else {
        let init_ctx = InitContext::init(config).await?;
        let handle = init_ctx.progress.clone();
        let err_channel = init_ctx.error.clone();

        let mut disk_phase = handle.add_phase("Opening data drive".into(), Some(10));
        let init_phases = InitPhases::new(&handle);
        let rpc_ctx_phases = InitRpcContextPhases::new(&handle);

        server.serve_init(init_ctx);

        async {
            disk_phase.start();
            let guid_string = tokio::fs::read_to_string("/media/startos/config/disk.guid") // unique identifier for volume group - keeps track of the disk that goes with your embassy
                .await?;
            let disk_guid = Arc::new(String::from(guid_string.trim()));
            let requires_reboot = crate::disk::main::import(
                &**disk_guid,
                config.datadir(),
                if tokio::fs::metadata(REPAIR_DISK_PATH).await.is_ok() {
                    RepairStrategy::Aggressive
                } else {
                    RepairStrategy::Preen
                },
                if disk_guid.ends_with("_UNENC") {
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
            disk_phase.complete();
            tracing::info!("Loaded Disk");

            if requires_reboot.0 {
                tracing::info!("Rebooting...");
                let mut reboot_phase = handle.add_phase("Rebooting".into(), Some(1));
                reboot_phase.start();
                return Ok(Err(Shutdown {
                    export_args: Some((disk_guid, config.datadir().to_owned())),
                    restart: true,
                }));
            }

            let InitResult { net_ctrl } = crate::init::init(config, init_phases).await?;

            let rpc_ctx =
                RpcContext::init(config, disk_guid, Some(net_ctrl), rpc_ctx_phases).await?;

            Ok::<_, Error>(Ok((rpc_ctx, handle)))
        }
        .await
        .map_err(|e| {
            err_channel.send_replace(Some(e.clone_output()));
            e
        })
    }
}

#[instrument(skip_all)]
pub async fn main(
    server: &mut WebServer,
    config: &ServerConfig,
) -> Result<Result<(RpcContext, FullProgressTracker), Shutdown>, Error> {
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

                Ok(Err(shutdown))
            }
            .await
        }
        Ok(s) => Ok(s),
    };

    res
}
