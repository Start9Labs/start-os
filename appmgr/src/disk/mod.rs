use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use chrono::{DateTime, Utc};
use clap::ArgMatches;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use self::util::{mount, mount_ecryptfs, unmount, DiskInfo};
use crate::context::RpcContext;
use crate::disk::util::TMP_MOUNTPOINT;
use crate::s9pk::manifest::PackageId;
use crate::util::{display_serializable, GeneralGuard, IoFormat, Version};
use crate::volume::{BACKUP_DIR, BACKUP_DIR_CRYPT, BACKUP_MNT};
use crate::{Error, ResultExt};

pub mod main;
pub mod util;

#[command(subcommands(list))]
pub fn disk() -> Result<(), Error> {
    Ok(())
}

fn display_disk_info(info: Vec<DiskInfo>, matches: &ArgMatches<'_>) {
    use prettytable::*;

    if matches.is_present("format") {
        return display_serializable(info, matches);
    }

    let mut table = Table::new();
    table.add_row(row![bc =>
        "LOGICALNAME",
        "LABEL",
        "CAPACITY",
        "USED",
        "EMBASSY OS VERSION"
    ]);
    for disk in info {
        let row = row![
            disk.logicalname.display(),
            "N/A",
            &format!("{:.2} GiB", disk.capacity as f64 / 1024.0 / 1024.0 / 1024.0),
            "N/A",
            "N/A",
        ];
        table.add_row(row);
        for part in disk.partitions {
            let row = row![
                part.logicalname.display(),
                if let Some(label) = part.label.as_ref() {
                    label
                } else {
                    "N/A"
                },
                part.capacity,
                if let Some(used) = part
                    .used
                    .map(|u| format!("{:.2} GiB", u as f64 / 1024.0 / 1024.0 / 1024.0))
                    .as_ref()
                {
                    used
                } else {
                    "N/A"
                },
                if let Some(eos) = part.embassy_os.as_ref() {
                    eos.version.as_str()
                } else {
                    "N/A"
                },
            ];
            table.add_row(row);
        }
    }
    table.print_tty(false);
}

#[command(display(display_disk_info))]
pub async fn list(
    #[allow(unused_variables)]
    #[arg]
    format: Option<IoFormat>,
) -> Result<Vec<DiskInfo>, Error> {
    crate::disk::util::list().await
}

#[derive(Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct BackupInfo {
    pub version: Version,
    pub timestamp: DateTime<Utc>,
    pub package_backups: BTreeMap<PackageId, PackageBackupInfo>,
}

#[derive(Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct PackageBackupInfo {
    pub version: Version,
    pub os_version: Version,
    pub timestamp: DateTime<Utc>,
}

fn display_backup_info(info: BackupInfo, matches: &ArgMatches<'_>) {
    use prettytable::*;

    if matches.is_present("format") {
        return display_serializable(info, matches);
    }

    let mut table = Table::new();
    table.add_row(row![bc =>
        "ID",
        "VERSION",
        "OS VERSION",
        "TIMESTAMP",
    ]);
    table.add_row(row![
        "EMBASSY OS",
        info.version.as_str(),
        info.version.as_str(),
        &info.timestamp.to_string()
    ]);
    for (id, info) in info.package_backups {
        let row = row![
            id.as_str(),
            info.version.as_str(),
            info.os_version.as_str(),
            &info.timestamp.to_string(),
        ];
        table.add_row(row);
    }
    table.print_tty(false);
}

#[command(rename = "backup-info", display(display_backup_info))]
#[instrument(skip(ctx, password))]
pub async fn backup_info(
    #[context] ctx: RpcContext,
    #[arg] logicalname: PathBuf,
    #[arg] password: String,
) -> Result<BackupInfo, Error> {
    mount(logicalname, BACKUP_MNT).await?;
    mount_ecryptfs(BACKUP_DIR_CRYPT, BACKUP_DIR, &password).await?;
    let mounted = GeneralGuard::new(|| {
        tokio::spawn(async move {
            unmount(BACKUP_DIR).await?;
            unmount(BACKUP_MNT).await
        })
    });

    mounted
        .drop()
        .await
        .with_kind(crate::ErrorKind::Unknown)??;
    Ok(todo!())
}
