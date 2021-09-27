use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use anyhow::anyhow;
use futures::TryStreamExt;
use indexmap::IndexSet;
use regex::Regex;
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::process::Command;

use crate::util::io::from_yaml_async_reader;
use crate::util::{GeneralGuard, Invoke, Version};
use crate::{Error, ResultExt as _};

pub const TMP_MOUNTPOINT: &'static str = "/media/embassy-os";

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct DiskInfo {
    pub logicalname: PathBuf,
    pub vendor: Option<String>,
    pub model: Option<String>,
    pub partitions: Vec<PartitionInfo>,
    pub capacity: usize,
    pub embassy_os: Option<EmbassyOsDiskInfo>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct PartitionInfo {
    pub logicalname: PathBuf,
    pub label: Option<String>,
    pub capacity: usize,
    pub used: Option<usize>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct EmbassyOsDiskInfo {
    pub version: Version,
}

const DISK_PATH: &'static str = "/dev/disk/by-path";
const SYS_BLOCK_PATH: &'static str = "/sys/block";

lazy_static::lazy_static! {
    static ref PARTITION_REGEX: Regex = Regex::new("-part[0-9]+$").unwrap();
}

pub async fn get_vendor<P: AsRef<Path>>(path: P) -> Result<Option<String>, Error> {
    let vendor = tokio::fs::read_to_string(
        Path::new(SYS_BLOCK_PATH)
            .join(path.as_ref().strip_prefix("/dev").map_err(|_| {
                Error::new(
                    anyhow!("not a canonical block device"),
                    crate::ErrorKind::BlockDevice,
                )
            })?)
            .join("device")
            .join("vendor"),
    )
    .await?;
    Ok(if vendor.is_empty() {
        None
    } else {
        Some(vendor)
    })
}

pub async fn get_model<P: AsRef<Path>>(path: P) -> Result<Option<String>, Error> {
    let model = tokio::fs::read_to_string(
        Path::new(SYS_BLOCK_PATH)
            .join(path.as_ref().strip_prefix("/dev").map_err(|_| {
                Error::new(
                    anyhow!("not a canonical block device"),
                    crate::ErrorKind::BlockDevice,
                )
            })?)
            .join("device")
            .join("model"),
    )
    .await?;
    Ok(if model.is_empty() { None } else { Some(model) })
}

pub async fn get_capacity<P: AsRef<Path>>(path: P) -> Result<usize, Error> {
    Ok(String::from_utf8(
        Command::new("blockdev")
            .arg("--getsize64")
            .arg(path.as_ref())
            .invoke(crate::ErrorKind::BlockDevice)
            .await?,
    )?
    .parse()?)
}

pub async fn get_label<P: AsRef<Path>>(path: P) -> Result<Option<String>, Error> {
    let label = String::from_utf8(
        Command::new("lsblk")
            .arg("-no")
            .arg("label")
            .arg(path.as_ref())
            .invoke(crate::ErrorKind::BlockDevice)
            .await?,
    )?;
    Ok(if label.is_empty() { None } else { Some(label) })
}

pub async fn get_used<P: AsRef<Path>>(path: P) -> Result<usize, Error> {
    Ok(String::from_utf8(
        Command::new("df")
            .arg("--output=used")
            .arg(path.as_ref())
            .invoke(crate::ErrorKind::Filesystem)
            .await?,
    )?
    .lines()
    .skip(1)
    .next()
    .unwrap_or_default()
    .trim()
    .parse()?)
}

pub async fn list() -> Result<Vec<DiskInfo>, Error> {
    if tokio::fs::metadata(TMP_MOUNTPOINT).await.is_err() {
        tokio::fs::create_dir_all(TMP_MOUNTPOINT)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, TMP_MOUNTPOINT))?;
    }

    let disks = tokio_stream::wrappers::ReadDirStream::new(
        tokio::fs::read_dir(DISK_PATH)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, DISK_PATH))?,
    )
    .map_err(|e| {
        Error::new(
            anyhow::Error::from(e).context(DISK_PATH),
            crate::ErrorKind::Filesystem,
        )
    })
    .try_fold(BTreeMap::new(), |mut disks, dir_entry| async move {
        if let Some(disk_path) = dir_entry.path().file_name().and_then(|s| s.to_str()) {
            let (disk_path, part_path) = if let Some(end) = PARTITION_REGEX.find(disk_path) {
                (
                    disk_path.strip_suffix(end.as_str()).unwrap_or_default(),
                    Some(disk_path),
                )
            } else {
                (disk_path, None)
            };
            let disk_path = Path::new(DISK_PATH).join(disk_path);
            let disk = tokio::fs::canonicalize(&disk_path).await.with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    disk_path.display().to_string(),
                )
            })?;
            if !disks.contains_key(&disk) {
                disks.insert(disk.clone(), IndexSet::new());
            }
            if let Some(part_path) = part_path {
                let part_path = Path::new(DISK_PATH).join(part_path);
                let part = tokio::fs::canonicalize(&part_path).await.with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        part_path.display().to_string(),
                    )
                })?;
                disks.get_mut(&disk).unwrap().insert(part);
            }
        }
        Ok(disks)
    })
    .await?;

    let mut res = Vec::with_capacity(disks.len());
    for (disk, parts) in disks {
        let mut partitions = Vec::with_capacity(parts.len());
        let vendor = get_vendor(&disk)
            .await
            .map_err(|e| log::warn!("Could not get vendor of {}: {}", disk.display(), e.source))
            .unwrap_or_default();
        let model = get_model(&disk)
            .await
            .map_err(|e| log::warn!("Could not get model of {}: {}", disk.display(), e.source))
            .unwrap_or_default();
        let capacity = get_capacity(&disk)
            .await
            .map_err(|e| log::warn!("Could not get capacity of {}: {}", disk.display(), e.source))
            .unwrap_or_default();
        let mut embassy_os = None;
        for part in parts {
            let label = get_label(&part).await?;
            let capacity = get_capacity(&part)
                .await
                .map_err(|e| {
                    log::warn!("Could not get capacity of {}: {}", part.display(), e.source)
                })
                .unwrap_or_default();
            let mut used = None;

            let tmp_mountpoint = Path::new(TMP_MOUNTPOINT).join(&part);
            if let Err(e) = mount(&part, &tmp_mountpoint).await {
                log::warn!("Could not collect usage information: {}", e.source)
            } else {
                let mount_guard = GeneralGuard::new(|| {
                    let path = tmp_mountpoint.clone();
                    tokio::spawn(unmount(path))
                });
                used = get_used(&tmp_mountpoint)
                    .await
                    .map_err(|e| {
                        log::warn!("Could not get usage of {}: {}", part.display(), e.source)
                    })
                    .ok();
                if label.as_deref() == Some("rootfs") {
                    let version_path = tmp_mountpoint.join("root").join("appmgr").join("version");
                    if tokio::fs::metadata(&version_path).await.is_ok() {
                        embassy_os = Some(EmbassyOsDiskInfo {
                            version: from_yaml_async_reader(File::open(&version_path).await?)
                                .await?,
                        })
                    }
                }
                mount_guard
                    .drop()
                    .await
                    .with_kind(crate::ErrorKind::Unknown)??;
            }

            partitions.push(PartitionInfo {
                logicalname: part,
                label,
                capacity,
                used,
            });
        }
        res.push(DiskInfo {
            logicalname: disk,
            vendor,
            model,
            partitions,
            capacity,
            embassy_os,
        })
    }

    Ok(res)
}

pub async fn mount<P0: AsRef<Path>, P1: AsRef<Path>>(
    logicalname: P0,
    mount_point: P1,
) -> Result<(), Error> {
    let is_mountpoint = tokio::process::Command::new("mountpoint")
        .arg(mount_point.as_ref())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .await?;
    if is_mountpoint.success() {
        unmount(mount_point.as_ref()).await?;
    }
    tokio::fs::create_dir_all(&mount_point).await?;
    let mount_output = tokio::process::Command::new("mount")
        .arg(logicalname.as_ref())
        .arg(mount_point.as_ref())
        .output()
        .await?;
    crate::ensure_code!(
        mount_output.status.success(),
        crate::ErrorKind::Filesystem,
        "Error Mounting {} to {}: {}",
        logicalname.as_ref().display(),
        mount_point.as_ref().display(),
        std::str::from_utf8(&mount_output.stderr).unwrap_or("Unknown Error")
    );
    Ok(())
}

pub async fn mount_encfs<P0: AsRef<Path>, P1: AsRef<Path>>(
    src: P0,
    dst: P1,
    password: &str,
) -> Result<(), Error> {
    let mut encfs = tokio::process::Command::new("encfs")
        .arg("--standard")
        .arg("--public")
        .arg("-S")
        .arg(src.as_ref())
        .arg(dst.as_ref())
        .stdin(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()?;
    let mut stdin = encfs.stdin.take().unwrap();
    let mut stderr = encfs.stderr.take().unwrap();
    stdin.write_all(password.as_bytes()).await?;
    stdin.flush().await?;
    stdin.shutdown().await?;
    drop(stdin);
    let mut err = String::new();
    stderr.read_to_string(&mut err).await?;
    if !encfs.wait().await?.success() {
        Err(Error::new(anyhow!("{}", err), crate::ErrorKind::Filesystem))
    } else {
        Ok(())
    }
}

pub async fn bind<P0: AsRef<Path>, P1: AsRef<Path>>(
    src: P0,
    dst: P1,
    read_only: bool,
) -> Result<(), Error> {
    log::info!(
        "Binding {} to {}",
        src.as_ref().display(),
        dst.as_ref().display()
    );
    let is_mountpoint = tokio::process::Command::new("mountpoint")
        .arg(dst.as_ref())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .await?;
    if is_mountpoint.success() {
        unmount(dst.as_ref()).await?;
    }
    tokio::fs::create_dir_all(&dst).await?;
    let mut mount_cmd = tokio::process::Command::new("mount");
    mount_cmd.arg("--bind");
    if read_only {
        mount_cmd.arg("-o").arg("ro");
    }
    mount_cmd
        .arg(src.as_ref())
        .arg(dst.as_ref())
        .invoke(crate::ErrorKind::Filesystem)
        .await
        .map_err(|e| {
            Error::new(
                e.source.context(format!(
                    "Binding {} to {}",
                    src.as_ref().display(),
                    dst.as_ref().display(),
                )),
                e.kind,
            )
        })?;
    Ok(())
}

pub async fn unmount<P: AsRef<Path>>(mount_point: P) -> Result<(), Error> {
    log::info!("Unmounting {}.", mount_point.as_ref().display());
    let umount_output = tokio::process::Command::new("umount")
        .arg(mount_point.as_ref())
        .output()
        .await?;
    crate::ensure_code!(
        umount_output.status.success(),
        crate::ErrorKind::Filesystem,
        "Error Unmounting Drive: {}: {}",
        mount_point.as_ref().display(),
        std::str::from_utf8(&umount_output.stderr).unwrap_or("Unknown Error")
    );
    tokio::fs::remove_dir_all(mount_point.as_ref())
        .await
        .with_ctx(|_| {
            (
                crate::ErrorKind::Filesystem,
                format!("rm {}", mount_point.as_ref().display()),
            )
        })?;
    Ok(())
}
