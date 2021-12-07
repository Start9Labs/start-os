use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use color_eyre::eyre::{self, eyre};
use futures::TryStreamExt;
use indexmap::IndexSet;
use nom::bytes::complete::{tag, take_till1};
use nom::character::complete::multispace1;
use nom::character::is_space;
use nom::combinator::{opt, rest};
use nom::sequence::{pair, preceded, terminated};
use nom::IResult;
use regex::Regex;
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use tokio::process::Command;
use tracing::instrument;

use super::mount::filesystem::block_dev::BlockDev;
use super::mount::guard::TmpMountGuard;
use super::quirks::{fetch_quirks, save_quirks, update_quirks};
use crate::util::io::from_yaml_async_reader;
use crate::util::serde::IoFormat;
use crate::util::{Invoke, Version};
use crate::{Error, ResultExt as _};

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct DiskInfo {
    pub logicalname: PathBuf,
    pub vendor: Option<String>,
    pub model: Option<String>,
    pub partitions: Vec<PartitionInfo>,
    pub capacity: u64,
    pub guid: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct PartitionInfo {
    pub logicalname: PathBuf,
    pub label: Option<String>,
    pub capacity: u64,
    pub used: Option<u64>,
    pub embassy_os: Option<EmbassyOsRecoveryInfo>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct EmbassyOsRecoveryInfo {
    pub version: Version,
    pub full: bool,
    pub password_hash: Option<String>,
    pub wrapped_key: Option<String>,
}

const DISK_PATH: &'static str = "/dev/disk/by-path";
const SYS_BLOCK_PATH: &'static str = "/sys/block";

lazy_static::lazy_static! {
    static ref PARTITION_REGEX: Regex = Regex::new("-part[0-9]+$").unwrap();
}

#[instrument(skip(path))]
pub async fn get_vendor<P: AsRef<Path>>(path: P) -> Result<Option<String>, Error> {
    let vendor = tokio::fs::read_to_string(
        Path::new(SYS_BLOCK_PATH)
            .join(path.as_ref().strip_prefix("/dev").map_err(|_| {
                Error::new(
                    eyre!("not a canonical block device"),
                    crate::ErrorKind::BlockDevice,
                )
            })?)
            .join("device")
            .join("vendor"),
    )
    .await?
    .trim()
    .to_owned();
    Ok(if vendor.is_empty() {
        None
    } else {
        Some(vendor)
    })
}

#[instrument(skip(path))]
pub async fn get_model<P: AsRef<Path>>(path: P) -> Result<Option<String>, Error> {
    let model = tokio::fs::read_to_string(
        Path::new(SYS_BLOCK_PATH)
            .join(path.as_ref().strip_prefix("/dev").map_err(|_| {
                Error::new(
                    eyre!("not a canonical block device"),
                    crate::ErrorKind::BlockDevice,
                )
            })?)
            .join("device")
            .join("model"),
    )
    .await?
    .trim()
    .to_owned();
    Ok(if model.is_empty() { None } else { Some(model) })
}

#[instrument(skip(path))]
pub async fn get_capacity<P: AsRef<Path>>(path: P) -> Result<u64, Error> {
    Ok(String::from_utf8(
        Command::new("blockdev")
            .arg("--getsize64")
            .arg(path.as_ref())
            .invoke(crate::ErrorKind::BlockDevice)
            .await?,
    )?
    .trim()
    .parse::<u64>()?)
}

#[instrument(skip(path))]
pub async fn get_label<P: AsRef<Path>>(path: P) -> Result<Option<String>, Error> {
    let label = String::from_utf8(
        Command::new("lsblk")
            .arg("-no")
            .arg("label")
            .arg(path.as_ref())
            .invoke(crate::ErrorKind::BlockDevice)
            .await?,
    )?
    .trim()
    .to_owned();
    Ok(if label.is_empty() { None } else { Some(label) })
}

#[instrument(skip(path))]
pub async fn get_used<P: AsRef<Path>>(path: P) -> Result<u64, Error> {
    Ok(String::from_utf8(
        Command::new("df")
            .arg("--output=used")
            .arg("--block-size=1")
            .arg(path.as_ref())
            .invoke(crate::ErrorKind::Filesystem)
            .await?,
    )?
    .lines()
    .skip(1)
    .next()
    .unwrap_or_default()
    .trim()
    .parse::<u64>()?)
}

#[instrument(skip(path))]
pub async fn get_available<P: AsRef<Path>>(path: P) -> Result<u64, Error> {
    Ok(String::from_utf8(
        Command::new("df")
            .arg("--output=avail")
            .arg("--block-size=1")
            .arg(path.as_ref())
            .invoke(crate::ErrorKind::Filesystem)
            .await?,
    )?
    .lines()
    .skip(1)
    .next()
    .unwrap_or_default()
    .trim()
    .parse::<u64>()?)
}

#[instrument(skip(path))]
pub async fn get_percentage<P: AsRef<Path>>(path: P) -> Result<u64, Error> {
    Ok(String::from_utf8(
        Command::new("df")
            .arg("--output=pcent")
            .arg(path.as_ref())
            .invoke(crate::ErrorKind::Filesystem)
            .await?,
    )?
    .lines()
    .skip(1)
    .next()
    .unwrap_or_default()
    .trim()
    .strip_suffix("%")
    .unwrap()
    .parse::<u64>()?)
}

#[instrument]
pub async fn pvscan() -> Result<BTreeMap<PathBuf, Option<String>>, Error> {
    let pvscan_out = Command::new("pvscan")
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    let pvscan_out_str = std::str::from_utf8(&pvscan_out)?;
    Ok(parse_pvscan_output(pvscan_out_str))
}

pub async fn recovery_info(
    mountpoint: impl AsRef<Path>,
) -> Result<Option<EmbassyOsRecoveryInfo>, Error> {
    let backup_unencrypted_metadata_path = mountpoint
        .as_ref()
        .join("EmbassyBackups/unencrypted-metadata.cbor");
    if tokio::fs::metadata(&backup_unencrypted_metadata_path)
        .await
        .is_ok()
    {
        Ok(Some(
            IoFormat::Cbor.from_slice(
                &tokio::fs::read(&backup_unencrypted_metadata_path)
                    .await
                    .with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            backup_unencrypted_metadata_path.display().to_string(),
                        )
                    })?,
            )?,
        ))
    } else {
        Ok(None)
    }
}

#[instrument]
pub async fn list() -> Result<Vec<DiskInfo>, Error> {
    let mut quirks = fetch_quirks().await?;
    update_quirks(&mut quirks).await?;
    save_quirks(&mut quirks).await?;
    let disk_guids = pvscan().await?;
    let disks = tokio_stream::wrappers::ReadDirStream::new(
        tokio::fs::read_dir(DISK_PATH)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, DISK_PATH))?,
    )
    .map_err(|e| {
        Error::new(
            eyre::Error::from(e).wrap_err(DISK_PATH),
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
            if &*disk == Path::new("/dev/mmcblk0") {
                return Ok(disks);
            }
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
        let mut guid: Option<String> = None;
        let mut partitions = Vec::with_capacity(parts.len());
        let vendor = get_vendor(&disk)
            .await
            .map_err(|e| tracing::warn!("Could not get vendor of {}: {}", disk.display(), e.source))
            .unwrap_or_default();
        let model = get_model(&disk)
            .await
            .map_err(|e| tracing::warn!("Could not get model of {}: {}", disk.display(), e.source))
            .unwrap_or_default();
        let capacity = get_capacity(&disk)
            .await
            .map_err(|e| {
                tracing::warn!("Could not get capacity of {}: {}", disk.display(), e.source)
            })
            .unwrap_or_default();
        if let Some(g) = disk_guids.get(&disk) {
            guid = g.clone();
        } else {
            for part in parts {
                let mut embassy_os = None;
                let label = get_label(&part).await?;
                let capacity = get_capacity(&part)
                    .await
                    .map_err(|e| {
                        tracing::warn!("Could not get capacity of {}: {}", part.display(), e.source)
                    })
                    .unwrap_or_default();
                let mut used = None;

                match TmpMountGuard::mount(&BlockDev::new(&part)).await {
                    Err(e) => tracing::warn!("Could not collect usage information: {}", e.source),
                    Ok(mount_guard) => {
                        used = get_used(&mount_guard)
                            .await
                            .map_err(|e| {
                                tracing::warn!(
                                    "Could not get usage of {}: {}",
                                    part.display(),
                                    e.source
                                )
                            })
                            .ok();
                        if let Some(recovery_info) = match recovery_info(&mount_guard).await {
                            Ok(a) => a,
                            Err(e) => {
                                tracing::error!(
                                    "Error fetching unencrypted backup metadata: {}",
                                    e
                                );
                                None
                            }
                        } {
                            embassy_os = Some(recovery_info)
                        } else if label.as_deref() == Some("rootfs") {
                            let version_path = mount_guard.as_ref().join("root/appmgr/version");
                            if tokio::fs::metadata(&version_path).await.is_ok() {
                                embassy_os = Some(EmbassyOsRecoveryInfo {
                                    version: from_yaml_async_reader(
                                        File::open(&version_path).await?,
                                    )
                                    .await?,
                                    full: true,
                                    password_hash: None,
                                    wrapped_key: None,
                                });
                            }
                        }
                        mount_guard.unmount().await?;
                    }
                }

                partitions.push(PartitionInfo {
                    logicalname: part,
                    label,
                    capacity,
                    used,
                    embassy_os,
                });
            }
        }
        res.push(DiskInfo {
            logicalname: disk,
            vendor,
            model,
            partitions,
            capacity,
            guid,
        })
    }

    Ok(res)
}

fn parse_pvscan_output(pvscan_output: &str) -> BTreeMap<PathBuf, Option<String>> {
    fn parse_line(line: &str) -> IResult<&str, (&str, Option<&str>)> {
        let pv_parse = preceded(
            tag("  PV "),
            terminated(take_till1(|c| is_space(c as u8)), multispace1),
        );
        let vg_parse = preceded(
            tag("VG "),
            terminated(take_till1(|c| is_space(c as u8)), multispace1),
        );
        let mut parser = terminated(pair(pv_parse, opt(vg_parse)), rest);
        parser(line)
    }
    let lines = pvscan_output.lines();
    let n = lines.clone().count();
    let entries = lines.take(n.saturating_sub(1));
    let mut ret = BTreeMap::new();
    for entry in entries {
        match parse_line(entry) {
            Ok((_, (pv, vg))) => {
                ret.insert(PathBuf::from(pv), vg.map(|s| s.to_owned()));
            }
            Err(_) => {
                tracing::warn!("Failed to parse pvscan output line: {}", entry);
            }
        }
    }
    ret
}

#[test]
fn test_pvscan_parser() {
    let s1 = r#"  PV /dev/mapper/cryptdata   VG data            lvm2 [1.81 TiB / 0    free]
  PV /dev/sdb                                   lvm2 [931.51 GiB]
  Total: 2 [2.72 TiB] / in use: 1 [1.81 TiB] / in no VG: 1 [931.51 GiB]
"#;
    let s2 = r#"  PV /dev/sdb   VG EMBASSY_LZHJAENWGPCJJL6C6AXOD7OOOIJG7HFBV4GYRJH6HADXUCN4BRWQ   lvm2 [931.51 GiB / 0    free]
  Total: 1 [931.51 GiB] / in use: 1 [931.51 GiB] / in no VG: 0 [0   ]
"#;
    let s3 = r#"  PV /dev/mapper/cryptdata   VG data            lvm2 [1.81 TiB / 0    free]
  Total: 1 [1.81 TiB] / in use: 1 [1.81 TiB] / in no VG: 0 [0   ]
"#;
    println!("{:?}", parse_pvscan_output(s1));
    println!("{:?}", parse_pvscan_output(s2));
    println!("{:?}", parse_pvscan_output(s3));
}
