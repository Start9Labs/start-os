use std::path::{Path, PathBuf};

use color_eyre::eyre::eyre;
use futures::TryStreamExt;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use sqlx::{Executor, Sqlite};

use super::{BackupTarget, BackupTargetId};
use crate::context::RpcContext;
use crate::disk::mount::filesystem::cifs::Cifs;
use crate::disk::mount::filesystem::ReadOnly;
use crate::disk::mount::guard::TmpMountGuard;
use crate::disk::util::{recovery_info, EmbassyOsRecoveryInfo};
use crate::util::display_none;
use crate::util::serde::KeyVal;
use crate::Error;

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct CifsBackupTarget {
    hostname: String,
    path: PathBuf,
    username: String,
    mountable: bool,
    embassy_os: Option<EmbassyOsRecoveryInfo>,
}

#[command(subcommands(add, update, remove))]
pub fn cifs() -> Result<(), Error> {
    Ok(())
}

#[command(display(display_none))]
pub async fn add(
    #[context] ctx: RpcContext,
    #[arg] hostname: String,
    #[arg] path: PathBuf,
    #[arg] username: String,
    #[arg] password: Option<String>,
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
    let id: u32 = sqlx::query!(
        "INSERT INTO cifs_shares (hostname, path, username, password) VALUES (?, ?, ?, ?) RETURNING id AS \"id: u32\"",
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

#[command(display(display_none))]
pub async fn update(
    #[context] ctx: RpcContext,
    #[arg] id: BackupTargetId,
    #[arg] hostname: String,
    #[arg] path: PathBuf,
    #[arg] username: String,
    #[arg] password: Option<String>,
) -> Result<KeyVal<BackupTargetId, BackupTarget>, Error> {
    let id = if let BackupTargetId::Cifs { id } = id {
        id
    } else {
        return Err(Error::new(
            eyre!("Backup Target ID {} Not Found", id),
            crate::ErrorKind::NotFound,
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
        "UPDATE cifs_shares SET hostname = ?, path = ?, username = ?, password = ? WHERE id = ?",
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
            crate::ErrorKind::NotFound,
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
            crate::ErrorKind::NotFound,
        ));
    };
    if sqlx::query!("DELETE FROM cifs_shares WHERE id = ?", id)
        .execute(&ctx.secret_store)
        .await?
        .rows_affected()
        == 0
    {
        return Err(Error::new(
            eyre!("Backup Target ID {} Not Found", BackupTargetId::Cifs { id }),
            crate::ErrorKind::NotFound,
        ));
    };
    Ok(())
}

pub async fn load<Ex>(secrets: &mut Ex, id: u32) -> Result<Cifs, Error>
where
    for<'a> &'a mut Ex: Executor<'a, Database = Sqlite>,
{
    let record = sqlx::query!(
        "SELECT hostname, path, username, password FROM cifs_shares WHERE id = ?",
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

pub async fn list<Ex>(secrets: &mut Ex) -> Result<Vec<(u32, CifsBackupTarget)>, Error>
where
    for<'a> &'a mut Ex: Executor<'a, Database = Sqlite>,
{
    let mut records = sqlx::query!(
        "SELECT id AS \"id: u32\", hostname, path, username, password FROM cifs_shares"
    )
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
