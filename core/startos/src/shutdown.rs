use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::context::RpcContext;
use crate::disk::main::export;
use crate::init::{STANDBY_MODE_PATH, SYSTEM_REBUILD_PATH};
use crate::prelude::*;
use crate::sound::SHUTDOWN;
use crate::util::Invoke;
use crate::{DATA_DIR, PLATFORM};

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

pub async fn shutdown(ctx: RpcContext) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_server_info_mut()
                .as_status_info_mut()
                .as_shutting_down_mut()
                .ser(&true)
        })
        .await
        .result?;
    ctx.shutdown
        .send(Some(Shutdown {
            export_args: Some((ctx.disk_guid.clone(), Path::new(DATA_DIR).to_owned())),
            restart: false,
        }))
        .map_err(|_| eyre!("receiver dropped"))
        .log_err();
    Ok(())
}

pub async fn restart(ctx: RpcContext) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_server_info_mut()
                .as_status_info_mut()
                .as_restarting_mut()
                .ser(&true)
        })
        .await
        .result?;
    ctx.shutdown
        .send(Some(Shutdown {
            export_args: Some((ctx.disk_guid.clone(), Path::new(DATA_DIR).to_owned())),
            restart: true,
        }))
        .map_err(|_| eyre!("receiver dropped"))
        .log_err();
    Ok(())
}

pub async fn rebuild(ctx: RpcContext) -> Result<(), Error> {
    tokio::fs::write(SYSTEM_REBUILD_PATH, b"").await?;
    restart(ctx).await
}
