use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use clap::Parser;
use color_eyre::eyre::eyre;
use imbl_value::InternedString;
use rpc_toolkit::{command, from_fn_async, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use super::{BackupTarget, BackupTargetId};
use crate::context::{CliContext, RpcContext};
use crate::db::model::DatabaseModel;
use crate::disk::mount::filesystem::cifs::Cifs;
use crate::disk::mount::filesystem::ReadOnly;
use crate::disk::mount::guard::{GenericMountGuard, TmpMountGuard};
use crate::disk::util::{recovery_info, EmbassyOsRecoveryInfo};
use crate::prelude::*;
use crate::util::serde::KeyVal;

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct CifsTargets(pub BTreeMap<u32, Cifs>);
impl CifsTargets {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }
}
impl Map for CifsTargets {
    type Key = u32;
    type Value = Cifs;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Self::key_string(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(InternedString::from_display(key))
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CifsBackupTarget {
    hostname: String,
    path: PathBuf,
    username: String,
    mountable: bool,
    start_os: Option<EmbassyOsRecoveryInfo>,
}

pub fn cifs() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "add",
            from_fn_async(add)
                .no_display()
                .with_remote_cli::<CliContext>(),
        )
        .subcommand(
            "update",
            from_fn_async(update)
                .no_display()
                .with_remote_cli::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove)
                .no_display()
                .with_remote_cli::<CliContext>(),
        )
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct AddParams {
    pub hostname: String,
    pub path: PathBuf,
    pub username: String,
    pub password: Option<String>,
}

pub async fn add(
    ctx: RpcContext,
    AddParams {
        hostname,
        path,
        username,
        password,
    }: AddParams,
) -> Result<KeyVal<BackupTargetId, BackupTarget>, Error> {
    let cifs = Cifs {
        hostname,
        path: Path::new("/").join(path),
        username,
        password,
    };
    let guard = TmpMountGuard::mount(&cifs, ReadOnly).await?;
    let start_os = recovery_info(guard.path()).await?;
    guard.unmount().await?;
    let id = ctx
        .db
        .mutate(|db| {
            let id = db
                .as_private()
                .as_cifs()
                .keys()?
                .into_iter()
                .max()
                .map_or(0, |a| a + 1);
            db.as_private_mut().as_cifs_mut().insert(&id, &cifs)?;
            Ok(id)
        })
        .await?;
    Ok(KeyVal {
        key: BackupTargetId::Cifs { id },
        value: BackupTarget::Cifs(CifsBackupTarget {
            hostname: cifs.hostname,
            path: cifs.path,
            username: cifs.username,
            mountable: true,
            start_os,
        }),
    })
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct UpdateParams {
    pub id: BackupTargetId,
    pub hostname: String,
    pub path: PathBuf,
    pub username: String,
    pub password: Option<String>,
}

pub async fn update(
    ctx: RpcContext,
    UpdateParams {
        id,
        hostname,
        path,
        username,
        password,
    }: UpdateParams,
) -> Result<KeyVal<BackupTargetId, BackupTarget>, Error> {
    let id = if let BackupTargetId::Cifs { id } = id {
        id
    } else {
        return Err(Error::new(
            eyre!("Backup Target ID {} Not Found", id),
            ErrorKind::NotFound,
        ));
    };
    let cifs = Cifs {
        hostname,
        path: Path::new("/").join(path),
        username,
        password,
    };
    let guard = TmpMountGuard::mount(&cifs, ReadOnly).await?;
    let start_os = recovery_info(guard.path()).await?;
    guard.unmount().await?;
    ctx.db
        .mutate(|db| {
            db.as_private_mut()
                .as_cifs_mut()
                .as_idx_mut(&id)
                .ok_or_else(|| {
                    Error::new(
                        eyre!("Backup Target ID {} Not Found", BackupTargetId::Cifs { id }),
                        ErrorKind::NotFound,
                    )
                })?
                .ser(&cifs)
        })
        .await?;
    Ok(KeyVal {
        key: BackupTargetId::Cifs { id },
        value: BackupTarget::Cifs(CifsBackupTarget {
            hostname: cifs.hostname,
            path: cifs.path,
            username: cifs.username,
            mountable: true,
            start_os,
        }),
    })
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct RemoveParams {
    pub id: BackupTargetId,
}

pub async fn remove(ctx: RpcContext, RemoveParams { id }: RemoveParams) -> Result<(), Error> {
    let id = if let BackupTargetId::Cifs { id } = id {
        id
    } else {
        return Err(Error::new(
            eyre!("Backup Target ID {} Not Found", id),
            ErrorKind::NotFound,
        ));
    };
    ctx.db
        .mutate(|db| db.as_private_mut().as_cifs_mut().remove(&id))
        .await?;
    Ok(())
}

pub fn load(db: &DatabaseModel, id: u32) -> Result<Cifs, Error> {
    db.as_private()
        .as_cifs()
        .as_idx(&id)
        .ok_or_else(|| {
            Error::new(
                eyre!("Backup Target ID {} Not Found", id),
                ErrorKind::NotFound,
            )
        })?
        .de()
}

pub async fn list(db: &DatabaseModel) -> Result<Vec<(u32, CifsBackupTarget)>, Error> {
    let mut cifs = Vec::new();
    for (id, model) in db.as_private().as_cifs().as_entries()? {
        let mount_info = model.de()?;
        let start_os = async {
            let guard = TmpMountGuard::mount(&mount_info, ReadOnly).await?;
            let start_os = recovery_info(guard.path()).await?;
            guard.unmount().await?;
            Ok::<_, Error>(start_os)
        }
        .await;
        cifs.push((
            id,
            CifsBackupTarget {
                hostname: mount_info.hostname,
                path: mount_info.path,
                username: mount_info.username,
                mountable: start_os.is_ok(),
                start_os: start_os.ok().and_then(|a| a),
            },
        ));
    }

    Ok(cifs)
}
