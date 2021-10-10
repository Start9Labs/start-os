use anyhow::anyhow;
use patch_db::{DbHandle, ModelDataMut, PatchDbHandle, Revision};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::process::Command;

use crate::context::RpcContext;
use crate::db::model::{Database, PackageDataEntry, ServerStatus};
use crate::disk::util::{mount, unmount};
use crate::install::PKG_ARCHIVE_DIR;
use crate::status::MainStatus;
use crate::util::Invoke;
use crate::util::{display_none, GeneralGuard};
use crate::volume::BACKUP_MNT;
use crate::{Error, ErrorKind, ResultExt, BACKUP_DIR};
use rpc_toolkit::command;

#[command(rename = "create", display(display_none))]
pub async fn backup_all(
    #[context] ctx: RpcContext,
    #[arg(rename = "logicalname")] logical_name: PathBuf,
    #[arg] password: String,
) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    assure_backing_up(&mut db).await?;
    let mut tx = db.begin().await?;
    let mut db_model = crate::db::DatabaseModel::new().get_mut(&mut tx).await?;
    mount(logical_name, BACKUP_MNT).await?;
    let mounted = GeneralGuard::new(|| tokio::spawn(unmount(BACKUP_MNT)));

    // TODO Fetch from db instead of pulling all of model and doing the settings.
    for (package_id, mut installed) in db_model
        .package_data
        .0
        .iter_mut()
        .filter_map(|(id, pde)| pde.installed_mut().map(|i| (id, i)))
    {
        // todo!("Backup state preferences");
        let (started, health) = match &installed.status.main {
            MainStatus::Running { started, health } => (Some(started.clone()), health.clone()),
            MainStatus::Stopped | MainStatus::Stopping => (None, Default::default()),
            MainStatus::Restoring { .. } => {
                todo!("Can't do backup because one of the services is in a restoring state");
            }
            MainStatus::BackingUp { .. } => {
                todo!("Can't do backup because one of the services is in a backing up state");
            }
        };
        installed.status.main = MainStatus::BackingUp {
            started: started.clone(),
            health: health.clone(),
        };
        // todo!("Backup application");
        installed
            .manifest
            .backup
            .create(
                &ctx,
                package_id,
                &installed.manifest.version,
                &installed.manifest.volumes,
            )
            .await?;
        installed.status.main = match started {
            Some(started) => MainStatus::Running { started, health },
            None => MainStatus::Stopped,
        };
    }
    let secrets = ctx.datadir.join("main/secrets.db");
    tokio::fs::copy(secrets, Path::new(BACKUP_DIR).join("secrets.db")).await?;

    let embassy = ctx.datadir.join("main/embassy.db");
    tokio::fs::copy(embassy, Path::new(BACKUP_DIR).join("embassy.db")).await?;

    let s9pk = ctx.datadir.join(PKG_ARCHIVE_DIR);
    Command::new("cp")
        .arg("-r")
        .arg(s9pk)
        .arg(Path::new(BACKUP_DIR).join("archive"))
        .invoke(crate::ErrorKind::Filesystem)
        .await?;

    mounted.drop().await.with_kind(ErrorKind::Unknown)??;
    Ok(())
}

async fn assure_backing_up(db: &mut PatchDbHandle) -> Result<Option<Arc<Revision>>, Error> {
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
    Ok(tx.commit(None).await?)
}
