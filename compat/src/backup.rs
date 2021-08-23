use std::os::unix::process::ExitStatusExt;
use std::path::Path;
use argon2::Config;
use emver::Version;
use futures::try_join;
use futures::TryStreamExt;

use rand::Rng;
use serde::Serialize;
use embassy::util::from_yaml_async_reader;
use embassy::util::to_yaml_async_writer;
use embassy::util::Invoke;
// use embassy::util::PersistencePath;
use embassy::version::VersionT;
use anyhow::anyhow;
use embassy::Error;
// use embassy::{self,Error};
use embassy::ResultExt;

#[macro_use]
use embassy::ensure_code;

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Metadata {
    pub app_version: Version,
    pub os_version: &'static Version,
}

pub async fn create_backup<P: AsRef<Path>>(
    path: P,
    app_id: &str,
    password: &str,
) -> Result<(), embassy::Error> {
    let path = tokio::fs::canonicalize(path).await?;
    ensure_code!(
        path.is_dir(),
        embassy::ErrorKind::Filesystem,
        "Backup Path Must Be Directory"
    );
    let metadata_path = path.join("metadata.yaml");
    let data_path = path.join("data");
    let volume_path = Path::new(embassy::VOLUMES).join(app_id);

    // let info = crate::apps::info(app_id).await?;
    // to_yaml_async_writer(
    //     tokio::fs::File::create(metadata_path).await?,
    //     &Metadata {
    //         app_version: info.version,
    //         os_version: crate::version::Current::new().semver(),
    //     },
    // )
    // .await?;

    let exclude = if volume_path.is_dir() {
        let ignore_path = volume_path.join(".backupignore");
        if ignore_path.is_file() {
            use tokio::io::AsyncBufReadExt;
            tokio_stream::wrappers::LinesStream::new(tokio::io::BufReader::new(tokio::fs::File::open(ignore_path).await?).lines())
                .try_filter(|l| futures::future::ready(!l.is_empty()))
                .try_collect()
                .await?
        } else {
            Vec::new()
        }
    } else {
        return Err(Error {
            source: anyhow::anyhow!("Volume For {} Does Not Exist", app_id),
            kind: embassy::ErrorKind::NotFound,
            revision: None,
        })
    };
    let mut data_cmd = tokio::process::Command::new("duplicity");
    for exclude in exclude {
        if exclude.starts_with('!') {
            data_cmd.arg(format!(
                "--include={}",
                volume_path.join(exclude.trim_start_matches('!')).display()
            ));
        } else {
            data_cmd.arg(format!("--exclude={}", volume_path.join(exclude).display()));
        }
    }
    let data_res = data_cmd
        .arg(volume_path)
        .arg(format!("file://{}", data_path.display()))
        .invoke(embassy::ErrorKind::Duplicity)
        .await;
    data_res?;

    Ok(())
}

pub async fn restore_backup<P: AsRef<Path>>(
    path: P,
    app_id: &str,
    password: &str,
) -> Result<(), Error> {
    let path = tokio::fs::canonicalize(path).await?;
    ensure_code!(
        path.is_dir(),
        embassy::ErrorKind::Filesystem,
        "Backup Path Must Be Directory"
    );
    let metadata_path = path.join("metadata.yaml");
    let data_path = path.join("data");
    let volume_path = Path::new(embassy::VOLUMES).join(app_id);

    let mut data_cmd = tokio::process::Command::new("duplicity");
    data_cmd
        .arg("--force")
        .arg(format!("file://{}", data_path.display()))
        .arg(&volume_path);

    let data_output = data_cmd.status().await?;
    embassy::ensure_code!(
        data_output.success(),
        embassy::ErrorKind::Backup,
        "Duplicity Error"
    );

    tokio::fs::copy(
        metadata_path,
        Path::new(embassy::VOLUMES)
            .join(app_id)
            .join("start9")
            .join("restore.yaml"),
    )
    .await?;

    // TODO: Needed? - Attempt to configure the service with the config coming from restoration
    // let cfg_path = Path::new(embassy::VOLUMES)
    //     .join(app_id)
    //     .join("start9")
    //     .join("config.yaml");
    // if cfg_path.exists() {
    //     let cfg = from_yaml_async_reader(tokio::fs::File::open(cfg_path).await?).await?;
    //     if let Err(e) = embassy::config::configure(app_id, cfg, None, false).await {
    //         log::warn!("Could not restore backup configuration: {}", e);
    //     }
    // }

    Ok(())
}

pub async fn backup_to_partition(
    logicalname: &str,
    app_id: &str,
    password: &str,
) -> Result<(), Error> {
    let backup_mount_path = Path::new(embassy::BACKUP_MOUNT_POINT);
    let guard = embassy::volume::disk::MountGuard::new(logicalname, &backup_mount_path).await?;
    let backup_dir_path = backup_mount_path.join(embassy::BACKUP_DIR).join(app_id);
    tokio::fs::create_dir_all(&backup_dir_path).await?;

    let res = create_backup(backup_dir_path, app_id, password).await;

    guard.unmount().await?;

    res
}

pub async fn restore_from_partition(
    logicalname: &str,
    app_id: &str,
    password: &str,
) -> Result<(), Error> {
    let backup_mount_path = Path::new(embassy::BACKUP_MOUNT_POINT);
    let guard = embassy::volume::disk::MountGuard::new(logicalname, &backup_mount_path).await?;
    let backup_dir_path = backup_mount_path.join(embassy::BACKUP_DIR).join(app_id);

    let res = restore_backup(backup_dir_path, app_id, password).await;

    guard.unmount().await?;

    res
}