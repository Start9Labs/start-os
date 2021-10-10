use anyhow::anyhow;
use patch_db::{DbHandle, ModelDataMut, PatchDbHandle};
use std::path::PathBuf;

use crate::context::RpcContext;
use crate::db::model::{Database, PackageDataEntry, ServerStatus};
use crate::util::display_none;
use crate::Error;
use rpc_toolkit::command;

#[command(rename = "create", display(display_none))]
pub async fn backup_all(
    #[context] ctx: RpcContext,
    #[arg] logicalname: PathBuf,
    #[arg] password: String,
) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    assure_backing_up(&mut db).await?;
    let mut tx = db.begin().await?;
    let mut db_model = crate::db::DatabaseModel::new().get_mut(&mut tx).await?;

    for (package_id, package_entry) in db_model.package_data.0.iter_mut() {
        todo!("Backup state preferences");
        todo!("Backup application");
    }

    todo!("Backup secrets md");

    todo!("Embassy");
    todo!("s9pks ");
}

async fn assure_backing_up(db: &mut PatchDbHandle) -> Result<(), Error> {
    let begin = db.begin().await?;
    let mut tx = begin;
    let mut info = crate::db::DatabaseModel::new()
        .server_info()
        .get_mut(&mut tx)
        .await?;
    match &info.status {
        ServerStatus::Updating => {
            return Err(Error::new(
                anyhow!("Server is already updating!"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
        ServerStatus::Updated => {
            return Err(Error::new(
                anyhow!("Server is backed up and needs to be reset"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
        ServerStatus::BackingUp => {
            return Err(Error::new(
                anyhow!("Server is backing up!"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
        ServerStatus::Running => (),
    }
    info.status = ServerStatus::BackingUp;
    info.save(&mut tx).await?;
    Ok(())
}
