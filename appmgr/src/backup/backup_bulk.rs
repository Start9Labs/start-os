use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use color_eyre::eyre::eyre;
use patch_db::{DbHandle, LockType, ModelDataMut, PatchDbHandle, Revision};
use rpc_toolkit::command;
use tokio::process::Command;
use tracing::instrument;

use crate::context::RpcContext;
use crate::db::model::{Database, PackageDataEntry, ServerStatus};
use crate::db::util::WithRevision;
use crate::disk::util::{mount, unmount};
use crate::install::PKG_ARCHIVE_DIR;
use crate::status::MainStatus;
use crate::util::{display_none, GeneralGuard, Invoke};
use crate::volume::BACKUP_MNT;
use crate::{Error, ErrorKind, ResultExt, BACKUP_DIR};

#[command(rename = "create", display(display_none))]
pub async fn backup_all(
    #[context] ctx: RpcContext,
    #[arg] logicalname: PathBuf,
    #[arg] password: String,
) -> Result<WithRevision<()>, Error> {
    let mut db = ctx.db.handle();
    let revision = assure_backing_up(&mut db).await?;
    tokio::task::spawn(async move {
        match perform_backup(ctx, db, logicalname).await {
            Ok(()) => todo!(),
            Err(e) => todo!(),
        }
    });
    Ok(WithRevision {
        response: (),
        revision,
    })
}

#[instrument(skip(db))]
async fn assure_backing_up(db: &mut PatchDbHandle) -> Result<Option<Arc<Revision>>, Error> {
    let mut tx = db.begin().await?;
    let mut info = crate::db::DatabaseModel::new()
        .server_info()
        .get_mut(&mut tx)
        .await?;
    match &info.status {
        ServerStatus::Updating => {
            return Err(Error::new(
                eyre!("Server is updating!"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
        ServerStatus::Updated => {
            return Err(Error::new(
                eyre!("Server is updated and needs to be reset"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
        ServerStatus::BackingUp => {
            return Err(Error::new(
                eyre!("Server is already backing up!"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
        ServerStatus::Running => (),
    }
    info.status = ServerStatus::BackingUp;
    info.save(&mut tx).await?;
    Ok(tx.commit(None).await?)
}

#[instrument(skip(ctx, db))]
async fn perform_backup<Db: DbHandle>(
    ctx: RpcContext,
    mut db: Db,
    logical_name: PathBuf,
) -> Result<(), Error> {
    mount(logical_name, BACKUP_MNT).await?;
    let mounted = GeneralGuard::new(|| tokio::spawn(unmount(BACKUP_MNT)));

    let mut backup_report = BTreeMap::new();

    for package_id in crate::db::DatabaseModel::new()
        .package_data()
        .keys(&mut db, true)
        .await?
    {
        let installed_model = if let Some(installed_model) = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(&package_id)
            .and_then(|m| m.installed())
            .check(&mut db)
            .await?
        {
            installed_model
        } else {
            continue;
        };
        installed_model.lock(&mut db, LockType::Write).await;
        let manifest = installed_model
            .clone()
            .manifest()
            .get(&mut db, true)
            .await?;
        let main_status_model = installed_model.clone().status().main();
        let (started, health) = match main_status_model.get(&mut db, true).await?.into_owned() {
            MainStatus::Running { started, health } => (Some(started.clone()), health.clone()),
            MainStatus::Stopped | MainStatus::Stopping => (None, Default::default()),
            MainStatus::Restoring { .. } => {
                backup_report.insert(
                    package_id,
                    Err(Error::new(
                        eyre!("Can't do backup because service is in a restoring state"),
                        crate::ErrorKind::InvalidRequest,
                    )),
                );
                continue;
            }
            MainStatus::BackingUp { .. } => {
                backup_report.insert(
                    package_id,
                    Err(Error::new(
                        eyre!("Can't do backup because service is in a backing up state"),
                        crate::ErrorKind::InvalidRequest,
                    )),
                );
                continue;
            }
        };
        main_status_model
            .put(
                &mut db,
                &MainStatus::BackingUp {
                    started: started.clone(),
                    health: health.clone(),
                },
            )
            .await?;

        let res = manifest
            .backup
            .create(&ctx, &package_id, &manifest.version, &manifest.volumes)
            .await;
        backup_report.insert(package_id, res);

        main_status_model
            .put(
                &mut db,
                &match started {
                    Some(started) => MainStatus::Running { started, health },
                    None => MainStatus::Stopped,
                },
            )
            .await?;
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
