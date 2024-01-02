use std::path::{Path, PathBuf};

use clap::Parser;
use color_eyre::eyre::eyre;
use futures::TryStreamExt;
use rpc_toolkit::{command, from_fn_async, ParentHandler};
use serde::{Deserialize, Serialize};
use sqlx::{Executor, Postgres};

use super::{BackupTarget, BackupTargetId};
use crate::context::{CliContext, RpcContext};
use crate::disk::mount::filesystem::cifs::Cifs;
use crate::disk::mount::filesystem::ReadOnly;
use crate::disk::mount::guard::TmpMountGuard;
use crate::disk::util::{recovery_info, EmbassyOsRecoveryInfo};
use crate::prelude::*;
use crate::util::display_none;
use crate::util::serde::KeyVal;

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct CifsBackupTarget {
    hostname: String,
    path: PathBuf,
    username: String,
    mountable: bool,
    embassy_os: Option<EmbassyOsRecoveryInfo>,
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
            from_fn_async(update).with_remote_cli::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove).with_remote_cli::<CliContext>(),
        )
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
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
        path,
        username,
        password,
    };
    let guard = TmpMountGuard::mount(&cifs, ReadOnly).await?;
    let embassy_os = recovery_info(&guard).await?;
    guard.unmount().await?;
    let path_string = Path::new("/").join(&cifs.path).display().to_string();
    let id: i32 = sqlx::query!(
        "INSERT INTO cifs_shares (hostname, path, username, password) VALUES ($1, $2, $3, $4) RETURNING id",
        cifs.hostname,
        path_string,
        cifs.username,
        cifs.password,
    )
    .fetch_one(&ctx.secret_store)
    .await?.id;
    Ok(KeyVal {
        key: BackupTargetId::Cifs { id },
        value: BackupTarget::Cifs(CifsBackupTarget {
            hostname: cifs.hostname,
            path: cifs.path,
            username: cifs.username,
            mountable: true,
            embassy_os,
        }),
    })
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
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
        path,
        username,
        password,
    };
    let guard = TmpMountGuard::mount(&cifs, ReadOnly).await?;
    let embassy_os = recovery_info(&guard).await?;
    guard.unmount().await?;
    let path_string = Path::new("/").join(&cifs.path).display().to_string();
    if sqlx::query!(
        "UPDATE cifs_shares SET hostname = $1, path = $2, username = $3, password = $4 WHERE id = $5",
        cifs.hostname,
        path_string,
        cifs.username,
        cifs.password,
        id,
    )
    .execute(&ctx.secret_store)
    .await?
    .rows_affected()
        == 0
    {
        return Err(Error::new(
            eyre!("Backup Target ID {} Not Found", BackupTargetId::Cifs { id }),
            ErrorKind::NotFound,
        ));
    };
    Ok(KeyVal {
        key: BackupTargetId::Cifs { id },
        value: BackupTarget::Cifs(CifsBackupTarget {
            hostname: cifs.hostname,
            path: cifs.path,
            username: cifs.username,
            mountable: true,
            embassy_os,
        }),
    })
}

#[command(display(display_none))]
pub async fn remove(#[context] ctx: RpcContext, #[arg] id: BackupTargetId) -> Result<(), Error> {
    let id = if let BackupTargetId::Cifs { id } = id {
        id
    } else {
        return Err(Error::new(
            eyre!("Backup Target ID {} Not Found", id),
            ErrorKind::NotFound,
        ));
    };
    if sqlx::query!("DELETE FROM cifs_shares WHERE id = $1", id)
        .execute(&ctx.secret_store)
        .await?
        .rows_affected()
        == 0
    {
        return Err(Error::new(
            eyre!("Backup Target ID {} Not Found", BackupTargetId::Cifs { id }),
            ErrorKind::NotFound,
        ));
    };
    Ok(())
}

pub async fn load<Ex>(secrets: &mut Ex, id: i32) -> Result<Cifs, Error>
where
    for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
{
    let record = sqlx::query!(
        "SELECT hostname, path, username, password FROM cifs_shares WHERE id = $1",
        id
    )
    .fetch_one(secrets)
    .await?;

    Ok(Cifs {
        hostname: record.hostname,
        path: PathBuf::from(record.path),
        username: record.username,
        password: record.password,
    })
}

pub async fn list<Ex>(secrets: &mut Ex) -> Result<Vec<(i32, CifsBackupTarget)>, Error>
where
    for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
{
    let mut records =
        sqlx::query!("SELECT id, hostname, path, username, password FROM cifs_shares")
            .fetch_many(secrets);

    let mut cifs = Vec::new();
    while let Some(query_result) = records.try_next().await? {
        if let Some(record) = query_result.right() {
            let mount_info = Cifs {
                hostname: record.hostname,
                path: PathBuf::from(record.path),
                username: record.username,
                password: record.password,
            };
            let embassy_os = async {
                let guard = TmpMountGuard::mount(&mount_info, ReadOnly).await?;
                let embassy_os = recovery_info(&guard).await?;
                guard.unmount().await?;
                Ok::<_, Error>(embassy_os)
            }
            .await;
            cifs.push((
                record.id,
                CifsBackupTarget {
                    hostname: mount_info.hostname,
                    path: mount_info.path,
                    username: mount_info.username,
                    mountable: embassy_os.is_ok(),
                    embassy_os: embassy_os.ok().and_then(|a| a),
                },
            ));
        }
    }

    Ok(cifs)
}
