use std::path::PathBuf;
use std::sync::Arc;

use rpc_toolkit::command;

use crate::context::RpcContext;
use crate::disk::main::export;
use crate::init::{STANDBY_MODE_PATH, SYSTEM_REBUILD_PATH};
use crate::sound::SHUTDOWN;
use crate::util::docker::CONTAINER_TOOL;
use crate::util::{display_none, Invoke};
use crate::{Error, PLATFORM};

#[derive(Debug, Clone)]
pub struct Shutdown {
    pub export_args: Option<(Arc<String>, PathBuf)>,
    pub restart: bool,
}
impl Shutdown {
    /// BLOCKING
    pub fn execute(&self) {
        use std::process::Command;

        if self.restart {
            tracing::info!("Beginning server restart");
        } else {
            tracing::info!("Beginning server shutdown");
        }

        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap();
        rt.block_on(async {
            use tokio::process::Command;

            if let Err(e) = Command::new("systemctl")
                .arg("stop")
                .arg("systemd-journald")
                .invoke(crate::ErrorKind::Journald)
                .await
            {
                tracing::error!("Error Stopping Journald: {}", e);
                tracing::debug!("{:?}", e);
            }
            if CONTAINER_TOOL == "docker" {
                if let Err(e) = Command::new("systemctl")
                    .arg("stop")
                    .arg("docker")
                    .invoke(crate::ErrorKind::Docker)
                    .await
                {
                    tracing::error!("Error Stopping Docker: {}", e);
                    tracing::debug!("{:?}", e);
                }
            }
            if let Some((guid, datadir)) = &self.export_args {
                if let Err(e) = export(guid, datadir).await {
                    tracing::error!("Error Exporting Volume Group: {}", e);
                    tracing::debug!("{:?}", e);
                }
            }
            if &*PLATFORM != "raspberrypi" || self.restart {
                if let Err(e) = SHUTDOWN.play().await {
                    tracing::error!("Error Playing Shutdown Song: {}", e);
                    tracing::debug!("{:?}", e);
                }
            }
        });
        drop(rt);
        if &*PLATFORM == "raspberrypi" {
            if !self.restart {
                std::fs::write(STANDBY_MODE_PATH, "").unwrap();
                Command::new("sync").spawn().unwrap().wait().unwrap();
            }
            Command::new("reboot").spawn().unwrap().wait().unwrap();
        } else if self.restart {
            Command::new("reboot").spawn().unwrap().wait().unwrap();
        } else {
            Command::new("shutdown")
                .arg("-h")
                .arg("now")
                .spawn()
                .unwrap()
                .wait()
                .unwrap();
        }
    }
}

#[command(display(display_none))]
pub async fn shutdown(#[context] ctx: RpcContext) -> Result<(), Error> {
    ctx.shutdown
        .send(Some(Shutdown {
            export_args: Some((ctx.disk_guid.clone(), ctx.datadir.clone())),
            restart: false,
        }))
        .map_err(|_| ())
        .expect("receiver dropped");
    Ok(())
}

#[command(display(display_none))]
pub async fn restart(#[context] ctx: RpcContext) -> Result<(), Error> {
    ctx.shutdown
        .send(Some(Shutdown {
            export_args: Some((ctx.disk_guid.clone(), ctx.datadir.clone())),
            restart: true,
        }))
        .map_err(|_| ())
        .expect("receiver dropped");
    Ok(())
}

#[command(display(display_none))]
pub async fn rebuild(#[context] ctx: RpcContext) -> Result<(), Error> {
    tokio::fs::write(SYSTEM_REBUILD_PATH, b"").await?;
    restart(ctx).await
}
