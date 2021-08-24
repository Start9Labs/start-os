use std::path::Path;
use emver::Version;
use futures::TryStreamExt;

use serde::Serialize;
use embassy::util::{Invoke, to_yaml_async_writer};
use anyhow::anyhow;
use embassy::Error;

#[macro_use]
use embassy::ensure_code;

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Metadata {
    pub app_version: Version,
    pub os_version: &'static Version,
}

pub async fn create_backup<P: AsRef<Path>>(
    mountpoint: P,
    data_path: P,
    app_id: &str,
) -> Result<(), embassy::Error> {
    let path = tokio::fs::canonicalize(mountpoint).await?;
    ensure_code!(
        path.is_dir(),
        embassy::ErrorKind::Filesystem,
        "Backup Path Must Be Directory"
    );
    let volume_path = Path::new(embassy::VOLUMES).join(app_id);

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
        .arg(format!("file://{}", data_path.as_ref().display().to_string()))
        .invoke(embassy::ErrorKind::Duplicity)
        .await;
    data_res?;

    Ok(())
}

pub async fn restore_backup<P: AsRef<Path>>(
    path: P,
    data_path: P,
    app_id: &str,
) -> Result<(), embassy::Error> {
    let path = tokio::fs::canonicalize(path).await?;
    ensure_code!(
        path.is_dir(),
        embassy::ErrorKind::Filesystem,
        "Backup Path Must Be Directory"
    );
    let metadata_path = path.join("metadata.yaml");
    let volume_path = Path::new(embassy::VOLUMES).join(app_id);

    let mut data_cmd = tokio::process::Command::new("duplicity");
    data_cmd
        .arg("--force")
        .arg(format!("file://{:#?}", data_path.as_ref().display().to_string()))
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