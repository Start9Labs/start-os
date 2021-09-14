use std::sync::Arc;

use patch_db::{LockType, PatchDbHandle};
use rpc_toolkit::command;

use crate::context::RpcContext;
use crate::disk::main::export;
use crate::sound::MARIO_DEATH;
use crate::util::{display_none, Invoke};
use crate::Error;

#[derive(Clone)]
pub struct Shutdown {
    zfs_pool: Arc<String>,
    restart: bool,
    db_handle: Arc<PatchDbHandle>,
}
impl Shutdown {
    /// BLOCKING
    pub fn execute(&self) {
        use std::process::Command;

        tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(async {
                use tokio::process::Command;

                if let Err(e) = Command::new("systemctl")
                    .arg("stop")
                    .arg("systemd-journald")
                    .invoke(crate::ErrorKind::Journald)
                    .await
                {
                    log::error!("Error Stopping Journald: {}", e);
                }
                if let Err(e) = Command::new("systemctl")
                    .arg("stop")
                    .arg("docker")
                    .invoke(crate::ErrorKind::Docker)
                    .await
                {
                    log::error!("Error Stopping Docker: {}", e);
                }
                if let Err(e) = export(&self.zfs_pool).await {
                    log::error!("Error Exporting ZFS Pool: {}", e);
                }
                if let Err(e) = MARIO_DEATH.play().await {
                    log::error!("Error Playing Shutdown Song: {}", e);
                }
            });
        if self.restart {
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
    let mut db = ctx.db.handle();
    crate::db::DatabaseModel::new()
        .lock(&mut db, LockType::Write)
        .await;
    ctx.shutdown
        .send(Some(Shutdown {
            zfs_pool: ctx.zfs_pool_name.clone(),
            restart: false,
            db_handle: Arc::new(db),
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
        .await;
    ctx.shutdown
        .send(Some(Shutdown {
            zfs_pool: ctx.zfs_pool_name.clone(),
            restart: true,
            db_handle: Arc::new(db),
        }))
        .map_err(|_| ())
        .expect("receiver dropped");
    Ok(())
}
