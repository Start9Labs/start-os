use std::collections::BTreeMap;
use std::os::unix::prelude::OsStrExt;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Weak};

use color_eyre::eyre::{self, eyre};
use digest::Digest;
use futures::TryStreamExt;
use indexmap::IndexSet;
use lazy_static::lazy_static;
use nom::bytes::complete::{tag, take_till1};
use nom::character::complete::multispace1;
use nom::character::is_space;
use nom::combinator::{opt, rest};
use nom::sequence::{pair, preceded, terminated};
use nom::IResult;
use regex::Regex;
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::process::Command;
use tokio::sync::Mutex;
use tracing::instrument;

use super::BackupInfo;
use crate::auth::check_password;
use crate::middleware::encrypt::{decrypt_slice, encrypt_slice};
use crate::s9pk::manifest::PackageId;
use crate::util::io::from_yaml_async_reader;
use crate::util::{AtomicFile, FileLock, Invoke, IoFormat, Version};
use crate::volume::BACKUP_DIR;
use crate::{Error, ResultExt as _};

pub const TMP_MOUNTPOINT: &'static str = "/media/embassy-os/tmp";

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
    .await?;
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
    .await?;
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

#[instrument]
pub async fn list() -> Result<Vec<DiskInfo>, Error> {
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

                match TmpMountGuard::mount(&part, None).await {
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
                        let backup_unencrypted_metadata_path = mount_guard
                            .as_ref()
                            .join("EmbassyBackups/unencrypted-metadata.cbor");
                        if tokio::fs::metadata(&backup_unencrypted_metadata_path)
                            .await
                            .is_ok()
                        {
                            embassy_os = match (|| async {
                                IoFormat::Cbor.from_slice(
                                    &tokio::fs::read(&backup_unencrypted_metadata_path)
                                        .await
                                        .with_ctx(|_| {
                                            (
                                                crate::ErrorKind::Filesystem,
                                                backup_unencrypted_metadata_path
                                                    .display()
                                                    .to_string(),
                                            )
                                        })?,
                                )
                            })()
                            .await
                            {
                                Ok(a) => Some(a),
                                Err(e) => {
                                    tracing::error!(
                                        "Error fetching unencrypted backup metadata: {}",
                                        e
                                    );
                                    None
                                }
                            };
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

#[instrument(skip(logicalname, mountpoint))]
pub async fn mount(
    logicalname: impl AsRef<Path>,
    mountpoint: impl AsRef<Path>,
) -> Result<(), Error> {
    let is_mountpoint = tokio::process::Command::new("mountpoint")
        .arg(mountpoint.as_ref())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .await?;
    if is_mountpoint.success() {
        unmount(mountpoint.as_ref()).await?;
    }
    tokio::fs::create_dir_all(mountpoint.as_ref()).await?;
    let mount_output = tokio::process::Command::new("mount")
        .arg(logicalname.as_ref())
        .arg(mountpoint.as_ref())
        .output()
        .await?;
    crate::ensure_code!(
        mount_output.status.success(),
        crate::ErrorKind::Filesystem,
        "Error Mounting {} to {}: {}",
        logicalname.as_ref().display(),
        mountpoint.as_ref().display(),
        std::str::from_utf8(&mount_output.stderr).unwrap_or("Unknown Error")
    );
    Ok(())
}

#[instrument(skip(src, dst, key))]
pub async fn mount_ecryptfs<P0: AsRef<Path>, P1: AsRef<Path>>(
    src: P0,
    dst: P1,
    key: &str,
) -> Result<(), Error> {
    let is_mountpoint = tokio::process::Command::new("mountpoint")
        .arg(dst.as_ref())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .await?;
    if is_mountpoint.success() {
        unmount(dst.as_ref()).await?;
    }
    tokio::fs::create_dir_all(dst.as_ref()).await?;
    let mut ecryptfs = tokio::process::Command::new("mount")
        .arg("-t")
        .arg("ecryptfs")
        .arg(src.as_ref())
        .arg(dst.as_ref())
        .arg("-o")
        .arg(format!("key=passphrase,passwd={},ecryptfs_cipher=aes,ecryptfs_key_bytes=32,ecryptfs_passthrough=n,ecryptfs_enable_filename_crypto=y", key))
        .stdin(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()?;
    let mut stdin = ecryptfs.stdin.take().unwrap();
    let mut stderr = ecryptfs.stderr.take().unwrap();
    stdin.write_all(b"\nyes\nno").await?;
    stdin.flush().await?;
    stdin.shutdown().await?;
    drop(stdin);
    let mut err = String::new();
    stderr.read_to_string(&mut err).await?;
    if !ecryptfs.wait().await?.success() {
        Err(Error::new(eyre!("{}", err), crate::ErrorKind::Filesystem))
    } else {
        Ok(())
    }
}

#[instrument(skip(src, dst))]
pub async fn bind<P0: AsRef<Path>, P1: AsRef<Path>>(
    src: P0,
    dst: P1,
    read_only: bool,
) -> Result<(), Error> {
    tracing::info!(
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
    tokio::fs::create_dir_all(&src).await?;
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
        .await?;
    Ok(())
}

#[instrument(skip(mountpoint))]
pub async fn unmount<P: AsRef<Path>>(mountpoint: P) -> Result<(), Error> {
    tracing::debug!("Unmounting {}.", mountpoint.as_ref().display());
    let umount_output = tokio::process::Command::new("umount")
        .arg("-l")
        .arg(mountpoint.as_ref())
        .output()
        .await?;
    crate::ensure_code!(
        umount_output.status.success(),
        crate::ErrorKind::Filesystem,
        "Error Unmounting Drive: {}: {}",
        mountpoint.as_ref().display(),
        std::str::from_utf8(&umount_output.stderr).unwrap_or("Unknown Error")
    );
    tokio::fs::remove_dir_all(mountpoint.as_ref())
        .await
        .with_ctx(|_| {
            (
                crate::ErrorKind::Filesystem,
                format!("rm {}", mountpoint.as_ref().display()),
            )
        })?;
    Ok(())
}

#[async_trait::async_trait]
pub trait GenericMountGuard: AsRef<Path> + std::fmt::Debug + Send + Sync + 'static {
    async fn unmount(mut self) -> Result<(), Error>;
}

#[derive(Debug)]
pub struct MountGuard {
    mountpoint: PathBuf,
    mounted: bool,
}
impl MountGuard {
    pub async fn mount(
        logicalname: impl AsRef<Path>,
        mountpoint: impl AsRef<Path>,
        encryption_key: Option<&str>,
    ) -> Result<Self, Error> {
        let mountpoint = mountpoint.as_ref().to_owned();
        if let Some(key) = encryption_key {
            mount_ecryptfs(logicalname, &mountpoint, key).await?;
        } else {
            mount(logicalname, &mountpoint).await?;
        }
        Ok(MountGuard {
            mountpoint,
            mounted: true,
        })
    }
    pub async fn unmount(mut self) -> Result<(), Error> {
        if self.mounted {
            unmount(&self.mountpoint).await?;
            self.mounted = false;
        }
        Ok(())
    }
}
impl AsRef<Path> for MountGuard {
    fn as_ref(&self) -> &Path {
        &self.mountpoint
    }
}
impl Drop for MountGuard {
    fn drop(&mut self) {
        if self.mounted {
            let mountpoint = std::mem::take(&mut self.mountpoint);
            tokio::spawn(async move { unmount(mountpoint).await.unwrap() });
        }
    }
}
#[async_trait::async_trait]
impl GenericMountGuard for MountGuard {
    async fn unmount(mut self) -> Result<(), Error> {
        MountGuard::unmount(self).await
    }
}

async fn tmp_mountpoint(source: impl AsRef<Path>) -> Result<PathBuf, Error> {
    Ok(Path::new(TMP_MOUNTPOINT).join(base32::encode(
        base32::Alphabet::RFC4648 { padding: false },
        &sha2::Sha256::digest(
            tokio::fs::canonicalize(&source)
                .await
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        source.as_ref().display().to_string(),
                    )
                })?
                .as_os_str()
                .as_bytes(),
        ),
    )))
}

lazy_static! {
    static ref TMP_MOUNTS: Mutex<BTreeMap<PathBuf, Weak<MountGuard>>> = Mutex::new(BTreeMap::new());
}

#[derive(Debug)]
pub struct TmpMountGuard {
    guard: Arc<MountGuard>,
}
impl TmpMountGuard {
    #[instrument(skip(logicalname, encryption_key))]
    pub async fn mount(
        logicalname: impl AsRef<Path>,
        encryption_key: Option<&str>,
    ) -> Result<Self, Error> {
        let mountpoint = tmp_mountpoint(&logicalname).await?;
        let mut tmp_mounts = TMP_MOUNTS.lock().await;
        if !tmp_mounts.contains_key(&mountpoint) {
            tmp_mounts.insert(mountpoint.clone(), Weak::new());
        }
        let weak_slot = tmp_mounts.get_mut(&mountpoint).unwrap();
        if let Some(guard) = weak_slot.upgrade() {
            Ok(TmpMountGuard { guard })
        } else {
            let guard =
                Arc::new(MountGuard::mount(logicalname, &mountpoint, encryption_key).await?);
            *weak_slot = Arc::downgrade(&guard);
            Ok(TmpMountGuard { guard })
        }
    }
    pub async fn unmount(self) -> Result<(), Error> {
        if let Ok(guard) = Arc::try_unwrap(self.guard) {
            guard.unmount().await?;
        }
        Ok(())
    }
}
impl AsRef<Path> for TmpMountGuard {
    fn as_ref(&self) -> &Path {
        (&*self.guard).as_ref()
    }
}
#[async_trait::async_trait]
impl GenericMountGuard for TmpMountGuard {
    async fn unmount(mut self) -> Result<(), Error> {
        TmpMountGuard::unmount(self).await
    }
}

pub struct BackupMountGuard<G: GenericMountGuard> {
    backup_disk_mount_guard: Option<G>,
    encrypted_guard: Option<TmpMountGuard>,
    enc_key: String,
    pub unencrypted_metadata: EmbassyOsRecoveryInfo,
    pub metadata: BackupInfo,
}
impl<G: GenericMountGuard> BackupMountGuard<G> {
    fn backup_disk_path(&self) -> &Path {
        if let Some(guard) = &self.backup_disk_mount_guard {
            guard.as_ref()
        } else {
            unreachable!()
        }
    }

    #[instrument(skip(password))]
    pub async fn mount(backup_disk_mount_guard: G, password: &str) -> Result<Self, Error> {
        let backup_disk_path = backup_disk_mount_guard.as_ref();
        let unencrypted_metadata_path =
            backup_disk_path.join("EmbassyBackups/unencrypted-metadata.cbor");
        let mut unencrypted_metadata: EmbassyOsRecoveryInfo =
            if tokio::fs::metadata(&unencrypted_metadata_path)
                .await
                .is_ok()
            {
                IoFormat::Cbor.from_slice(
                    &tokio::fs::read(&unencrypted_metadata_path)
                        .await
                        .with_ctx(|_| {
                            (
                                crate::ErrorKind::Filesystem,
                                unencrypted_metadata_path.display().to_string(),
                            )
                        })?,
                )?
            } else {
                Default::default()
            };
        let enc_key = if let (Some(hash), Some(wrapped_key)) = (
            unencrypted_metadata.password_hash.as_ref(),
            unencrypted_metadata.wrapped_key.as_ref(),
        ) {
            let wrapped_key =
                base32::decode(base32::Alphabet::RFC4648 { padding: true }, wrapped_key)
                    .ok_or_else(|| {
                        Error::new(
                            eyre!("failed to decode wrapped key"),
                            crate::ErrorKind::Backup,
                        )
                    })?;
            check_password(hash, password)?;
            String::from_utf8(decrypt_slice(wrapped_key, password))?
        } else {
            base32::encode(
                base32::Alphabet::RFC4648 { padding: false },
                &rand::random::<[u8; 32]>()[..],
            )
        };

        if unencrypted_metadata.password_hash.is_none() {
            unencrypted_metadata.password_hash = Some(
                argon2::hash_encoded(
                    password.as_bytes(),
                    &rand::random::<[u8; 16]>()[..],
                    &argon2::Config::default(),
                )
                .with_kind(crate::ErrorKind::PasswordHashGeneration)?,
            );
        }
        if unencrypted_metadata.wrapped_key.is_none() {
            unencrypted_metadata.wrapped_key = Some(base32::encode(
                base32::Alphabet::RFC4648 { padding: true },
                &encrypt_slice(&enc_key, password),
            ));
        }

        let crypt_path = backup_disk_path.join("EmbassyBackups/crypt");
        if tokio::fs::metadata(&crypt_path).await.is_err() {
            tokio::fs::create_dir_all(&crypt_path).await.with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    crypt_path.display().to_string(),
                )
            })?;
        }
        let encrypted_guard = TmpMountGuard::mount(&crypt_path, Some(&enc_key)).await?;

        let metadata_path = encrypted_guard.as_ref().join("metadata.cbor");
        let metadata: BackupInfo = if tokio::fs::metadata(&metadata_path).await.is_ok() {
            IoFormat::Cbor.from_slice(&tokio::fs::read(&metadata_path).await.with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    metadata_path.display().to_string(),
                )
            })?)?
        } else {
            Default::default()
        };

        Ok(Self {
            backup_disk_mount_guard: Some(backup_disk_mount_guard),
            encrypted_guard: Some(encrypted_guard),
            enc_key,
            unencrypted_metadata,
            metadata,
        })
    }

    pub fn change_password(&mut self, new_password: &str) -> Result<(), Error> {
        self.unencrypted_metadata.password_hash = Some(
            argon2::hash_encoded(
                new_password.as_bytes(),
                &rand::random::<[u8; 16]>()[..],
                &argon2::Config::default(),
            )
            .with_kind(crate::ErrorKind::PasswordHashGeneration)?,
        );
        self.unencrypted_metadata.wrapped_key = Some(base32::encode(
            base32::Alphabet::RFC4648 { padding: false },
            &encrypt_slice(&self.enc_key, new_password),
        ));
        Ok(())
    }

    #[instrument(skip(self))]
    pub async fn mount_package_backup(
        &self,
        id: &PackageId,
    ) -> Result<PackageBackupMountGuard, Error> {
        let lock = FileLock::new(Path::new(BACKUP_DIR).join(format!("{}.lock", id)), false).await?;
        let mountpoint = Path::new(BACKUP_DIR).join(id);
        bind(self.as_ref().join(id), &mountpoint, false).await?;
        Ok(PackageBackupMountGuard {
            mountpoint: Some(mountpoint),
            lock: Some(lock),
        })
    }

    #[instrument(skip(self))]
    pub async fn save(&self) -> Result<(), Error> {
        let metadata_path = self.as_ref().join("metadata.cbor");
        let backup_disk_path = self.backup_disk_path();
        let mut file = AtomicFile::new(&metadata_path).await?;
        file.write_all(&IoFormat::Cbor.to_vec(&self.metadata)?)
            .await?;
        file.save().await?;
        let unencrypted_metadata_path =
            backup_disk_path.join("EmbassyBackups/unencrypted-metadata.cbor");
        let mut file = AtomicFile::new(&unencrypted_metadata_path).await?;
        file.write_all(&IoFormat::Cbor.to_vec(&self.unencrypted_metadata)?)
            .await?;
        file.save().await?;
        Ok(())
    }

    #[instrument(skip(self))]
    pub async fn unmount(mut self) -> Result<(), Error> {
        if let Some(guard) = self.encrypted_guard.take() {
            guard.unmount().await?;
        }
        if let Some(guard) = self.backup_disk_mount_guard.take() {
            guard.unmount().await?;
        }
        Ok(())
    }

    #[instrument(skip(self))]
    pub async fn save_and_unmount(self) -> Result<(), Error> {
        self.save().await?;
        self.unmount().await?;
        Ok(())
    }
}
impl<G: GenericMountGuard> AsRef<Path> for BackupMountGuard<G> {
    fn as_ref(&self) -> &Path {
        if let Some(guard) = &self.encrypted_guard {
            guard.as_ref()
        } else {
            unreachable!()
        }
    }
}
impl<G: GenericMountGuard> Drop for BackupMountGuard<G> {
    fn drop(&mut self) {
        let first = self.encrypted_guard.take();
        let second = self.backup_disk_mount_guard.take();
        tokio::spawn(async move {
            if let Some(guard) = first {
                guard.unmount().await.unwrap();
            }
            if let Some(guard) = second {
                guard.unmount().await.unwrap();
            }
        });
    }
}

pub struct PackageBackupMountGuard {
    mountpoint: Option<PathBuf>,
    lock: Option<FileLock>,
}
impl PackageBackupMountGuard {
    pub async fn unmount(mut self) -> Result<(), Error> {
        if let Some(mountpoint) = self.mountpoint.take() {
            unmount(&mountpoint).await?;
        }
        if let Some(lock) = self.lock.take() {
            lock.unlock().await?;
        }
        Ok(())
    }
}
impl AsRef<Path> for PackageBackupMountGuard {
    fn as_ref(&self) -> &Path {
        if let Some(mountpoint) = &self.mountpoint {
            mountpoint
        } else {
            unreachable!()
        }
    }
}
impl Drop for PackageBackupMountGuard {
    fn drop(&mut self) {
        let mountpoint = self.mountpoint.take();
        let lock = self.lock.take();
        tokio::spawn(async move {
            if let Some(mountpoint) = mountpoint {
                unmount(&mountpoint).await.unwrap();
            }
            if let Some(lock) = lock {
                lock.unlock().await.unwrap();
            }
        });
    }
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
