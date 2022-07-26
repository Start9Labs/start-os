use std::path::PathBuf;
use std::sync::Arc;

use patch_db::{LockType, PatchDbHandle};
use rpc_toolkit::command;

use crate::context::RpcContext;
use crate::disk::main::export;
use crate::init::{STANDBY_MODE_PATH, SYSTEM_REBUILD_PATH};
use crate::sound::SHUTDOWN;
use crate::util::{display_none, Invoke};
use crate::{Error, ErrorKind};

#[derive(Debug, Clone)]
pub struct Shutdown {
    pub datadir: PathBuf,
    pub disk_guid: Option<Arc<String>>,
    pub restart: bool,
    pub db_handle: Option<Arc<PatchDbHandle>>,
}
impl Shutdown {
    /// BLOCKING
    pub fn execute(&self) {
        use std::process::Command;

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
            if let Err(e) = Command::new("systemctl")
                .arg("stop")
                .arg("docker")
                .invoke(crate::ErrorKind::Docker)
                .await
            {
                tracing::error!("Error Stopping Docker: {}", e);
                tracing::debug!("{:?}", e);
            }
            if let Some(guid) = &self.disk_guid {
                if let Err(e) = export(guid, &self.datadir).await {
                    tracing::error!("Error Exporting Volume Group: {}", e);
                    tracing::debug!("{:?}", e);
                }
            }
            if self.restart {
                if let Err(e) = SHUTDOWN.play().await {
                    tracing::error!("Error Playing Shutdown Song: {}", e);
                    tracing::debug!("{:?}", e);
                }
            } else {
                tokio::fs::write(STANDBY_MODE_PATH, "").await.unwrap();
                Command::new("sync")
                    .invoke(ErrorKind::Filesystem)
                    .await
                    .unwrap();
            }
        });
        drop(rt);
        if !self.restart {
            std::fs::write(STANDBY_MODE_PATH, "").unwrap();
        }
        Command::new("reboot").spawn().unwrap().wait().unwrap();
    }
}

#[command(display(display_none))]
pub async fn shutdown(#[context] ctx: RpcContext) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    crate::db::DatabaseModel::new()
        .lock(&mut db, LockType::Write)
        .await?;
    ctx.shutdown
        .send(Some(Shutdown {
            datadir: ctx.datadir.clone(),
            disk_guid: Some(ctx.disk_guid.clone()),
            restart: false,
            db_handle: Some(Arc::new(db)),
        }))
        .map_err(|_| ())
        .expect("receiver dropped");
    Ok(())
}

#[command(display(display_none))]
pub async fn restart(#[context] ctx: RpcContext) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    crate::db::DatabaseModel::new()
        .lock(&mut db, LockType::Write)
        .await?;
    ctx.shutdown
        .send(Some(Shutdown {
            datadir: ctx.datadir.clone(),
            disk_guid: Some(ctx.disk_guid.clone()),
            restart: true,
            db_handle: Some(Arc::new(db)),
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
