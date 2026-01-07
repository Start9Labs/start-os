use std::path::{Path, PathBuf};

use clap::Parser;
use color_eyre::eyre::eyre;
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use ts_rs::TS;

use crate::context::SetupContext;
use crate::context::config::ServerConfig;
use crate::disk::OsPartitionInfo;
use crate::disk::mount::filesystem::bind::Bind;
use crate::disk::mount::filesystem::block_dev::BlockDev;
use crate::disk::mount::filesystem::efivarfs::EfiVarFs;
use crate::disk::mount::filesystem::overlayfs::OverlayFs;
use crate::disk::mount::filesystem::{MountType, ReadWrite};
use crate::disk::mount::guard::{GenericMountGuard, MountGuard, TmpMountGuard};
use crate::disk::util::{DiskInfo, PartitionTable};
use crate::prelude::*;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::setup::SetupInfo;
use crate::util::Invoke;
use crate::util::io::{TmpDir, delete_file, open_file, write_file_atomic};
use crate::util::serde::IoFormat;
use crate::{ARCH, Error};

mod gpt;
mod mbr;

pub fn partition_for(disk: impl AsRef<Path>, idx: u32) -> PathBuf {
    let disk_path = disk.as_ref();
    let (root, leaf) = if let (Some(root), Some(leaf)) = (
        disk_path.parent(),
        disk_path.file_name().and_then(|s| s.to_str()),
    ) {
        (root, leaf)
    } else {
        return Default::default();
    };
    if leaf.ends_with(|c: char| c.is_ascii_digit()) {
        root.join(format!("{}p{}", leaf, idx))
    } else {
        root.join(format!("{}{}", leaf, idx))
    }
}

async fn partition(disk: &mut DiskInfo, overwrite: bool) -> Result<OsPartitionInfo, Error> {
    let partition_type = match (overwrite, disk.partition_table) {
        (true, _) | (_, None) => PartitionTable::Gpt,
        (_, Some(t)) => t,
    };
    disk.partition_table = Some(partition_type);
    match partition_type {
        PartitionTable::Gpt => gpt::partition(disk, overwrite).await,
        PartitionTable::Mbr => mbr::partition(disk, overwrite).await,
    }
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct InstallOsParams {
    os_drive: PathBuf,
    #[command(flatten)]
    data_drive: Option<DataDrive>,
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
struct DataDrive {
    #[arg(long = "data-drive")]
    logicalname: PathBuf,
    #[arg(long)]
    wipe: bool,
}

pub async fn install_os(
    ctx: SetupContext,
    InstallOsParams {
        os_drive,
        data_drive,
    }: InstallOsParams,
) -> Result<SetupInfo, Error> {
    let mut disks = crate::disk::util::list(&Default::default()).await?;
    let disk = disks
        .iter_mut()
        .find(|d| &d.logicalname == &os_drive)
        .ok_or_else(|| {
            Error::new(
                eyre!("Unknown disk {}", os_drive.display()),
                crate::ErrorKind::DiskManagement,
            )
        })?;

    let overwrite = if let Some(data_drive) = &data_drive {
        data_drive.wipe
            || ((disk.guid.is_none() || disk.logicalname != data_drive.logicalname)
                && disk
                    .partitions
                    .iter()
                    .all(|p| p.guid.is_none() || p.logicalname != data_drive.logicalname))
    } else {
        true
    };

    let part_info = partition(disk, overwrite).await?;

    if let Some(efi) = &part_info.efi {
        Command::new("mkfs.vfat")
            .arg(efi)
            .invoke(crate::ErrorKind::DiskManagement)
            .await?;
        Command::new("fatlabel")
            .arg(efi)
            .arg("efi")
            .invoke(crate::ErrorKind::DiskManagement)
            .await?;
    }

    Command::new("mkfs.vfat")
        .arg(&part_info.boot)
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    Command::new("fatlabel")
        .arg(&part_info.boot)
        .arg("boot")
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;

    if !overwrite {
        if let Ok(guard) =
            TmpMountGuard::mount(&BlockDev::new(part_info.root.clone()), MountType::ReadWrite).await
        {
            if let Err(e) = async {
                // cp -r ${guard}/config /tmp/config
                delete_file(guard.path().join("config/upgrade")).await?;
                delete_file(guard.path().join("config/overlay/etc/hostname")).await?;
                delete_file(guard.path().join("config/disk.guid")).await?;
                Command::new("cp")
                    .arg("-r")
                    .arg(guard.path().join("config"))
                    .arg("/tmp/config.bak")
                    .invoke(crate::ErrorKind::Filesystem)
                    .await?;
                Ok::<_, Error>(())
            }
            .await
            {
                tracing::error!("Error recovering previous config: {e}");
                tracing::debug!("{e:?}");
            }
            guard.unmount().await?;
        }
    }

    Command::new("mkfs.btrfs")
        .arg("-f")
        .arg(&part_info.root)
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    Command::new("btrfs")
        .arg("property")
        .arg("set")
        .arg(&part_info.root)
        .arg("label")
        .arg("rootfs")
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    let rootfs = TmpMountGuard::mount(&BlockDev::new(&part_info.root), ReadWrite).await?;

    let config_path = rootfs.path().join("config");

    if tokio::fs::metadata("/tmp/config.bak").await.is_ok() {
        if tokio::fs::metadata(&config_path).await.is_ok() {
            tokio::fs::remove_dir_all(&config_path).await?;
        }
        Command::new("cp")
            .arg("-r")
            .arg("/tmp/config.bak")
            .arg(&config_path)
            .invoke(crate::ErrorKind::Filesystem)
            .await?;
    } else {
        tokio::fs::create_dir_all(&config_path).await?;
    }

    let images_path = rootfs.path().join("images");
    tokio::fs::create_dir_all(&images_path).await?;
    let image_path = images_path
        .join(hex::encode(
            &MultiCursorFile::from(open_file("/run/live/medium/live/filesystem.squashfs").await?)
                .blake3_mmap()
                .await?
                .as_bytes()[..16],
        ))
        .with_extension("rootfs");
    tokio::fs::copy("/run/live/medium/live/filesystem.squashfs", &image_path).await?;
    // TODO: check hash of fs
    let unsquash_target = TmpDir::new().await?;
    let bootfs = MountGuard::mount(
        &BlockDev::new(&part_info.boot),
        unsquash_target.join("boot"),
        ReadWrite,
    )
    .await?;
    Command::new("unsquashfs")
        .arg("-n")
        .arg("-f")
        .arg("-d")
        .arg(&*unsquash_target)
        .arg("/run/live/medium/live/filesystem.squashfs")
        .arg("boot")
        .invoke(crate::ErrorKind::Filesystem)
        .await?;
    bootfs.unmount(true).await?;
    unsquash_target.delete().await?;
    Command::new("ln")
        .arg("-rsf")
        .arg(&image_path)
        .arg(config_path.join("current.rootfs"))
        .invoke(ErrorKind::DiskManagement)
        .await?;

    tokio::fs::write(
        rootfs.path().join("config/config.yaml"),
        IoFormat::Yaml.to_vec(&ServerConfig {
            os_partitions: Some(part_info.clone()),
            ..Default::default()
        })?,
    )
    .await?;
    ctx.config
        .mutate(|c| c.os_partitions = Some(part_info.clone()));

    let lower = TmpMountGuard::mount(&BlockDev::new(&image_path), MountType::ReadOnly).await?;
    let work = config_path.join("work");
    let upper = config_path.join("overlay");
    let overlay = TmpMountGuard::mount(
        &OverlayFs::new(vec![lower.path()], &upper, &work),
        ReadWrite,
    )
    .await?;

    let boot = MountGuard::mount(
        &BlockDev::new(&part_info.boot),
        overlay.path().join("boot"),
        ReadWrite,
    )
    .await?;
    let efi = if let Some(efi) = &part_info.efi {
        Some(
            MountGuard::mount(
                &BlockDev::new(efi),
                overlay.path().join("boot/efi"),
                ReadWrite,
            )
            .await?,
        )
    } else {
        None
    };
    let start_os_fs = MountGuard::mount(
        &Bind::new(rootfs.path()),
        overlay.path().join("media/startos/root"),
        MountType::ReadOnly,
    )
    .await?;
    let dev = MountGuard::mount(&Bind::new("/dev"), overlay.path().join("dev"), ReadWrite).await?;
    let proc =
        MountGuard::mount(&Bind::new("/proc"), overlay.path().join("proc"), ReadWrite).await?;
    let sys = MountGuard::mount(&Bind::new("/sys"), overlay.path().join("sys"), ReadWrite).await?;
    let efivarfs = if tokio::fs::metadata("/sys/firmware/efi").await.is_ok() {
        Some(
            MountGuard::mount(
                &EfiVarFs,
                overlay.path().join("sys/firmware/efi/efivars"),
                ReadWrite,
            )
            .await?,
        )
    } else {
        None
    };

    tokio::fs::write(
        overlay.path().join("etc/fstab"),
        format!(
            include_str!("fstab.template"),
            boot = part_info.boot.display(),
            efi = part_info
                .efi
                .as_ref()
                .map(|p| p.display().to_string())
                .unwrap_or_else(|| "# N/A".to_owned()),
            root = part_info.root.display(),
        ),
    )
    .await?;

    Command::new("chroot")
        .arg(overlay.path())
        .arg("systemd-machine-id-setup")
        .invoke(crate::ErrorKind::Systemd)
        .await?;

    Command::new("chroot")
        .arg(overlay.path())
        .arg("ssh-keygen")
        .arg("-A")
        .invoke(crate::ErrorKind::OpenSsh)
        .await?;

    let mut install = Command::new("chroot");
    install.arg(overlay.path()).arg("grub-install");
    if tokio::fs::metadata("/sys/firmware/efi").await.is_err() {
        match ARCH {
            "x86_64" => install.arg("--target=i386-pc"),
            _ => &mut install,
        };
    } else {
        match ARCH {
            "x86_64" => install.arg("--target=x86_64-efi"),
            "aarch64" => install.arg("--target=arm64-efi"),
            "riscv64" => install.arg("--target=riscv64-efi"),
            _ => &mut install,
        };
    }
    install
        .arg(&disk.logicalname)
        .invoke(crate::ErrorKind::Grub)
        .await?;

    Command::new("chroot")
        .arg(overlay.path())
        .arg("update-grub")
        .invoke(crate::ErrorKind::Grub)
        .await?;
    dev.unmount(false).await?;
    if let Some(efivarfs) = efivarfs {
        efivarfs.unmount(false).await?;
    }
    sys.unmount(false).await?;
    proc.unmount(false).await?;
    start_os_fs.unmount(false).await?;
    if let Some(efi) = efi {
        efi.unmount(false).await?;
    }
    boot.unmount(false).await?;

    overlay.unmount().await?;
    tokio::fs::remove_dir_all(&work).await?;
    lower.unmount().await?;

    let mut setup_info = SetupInfo::default();

    if let Some(data_drive) = data_drive {
        let mut logicalname = &*data_drive.logicalname;
        if logicalname == &os_drive {
            logicalname = part_info.data.as_deref().ok_or_else(|| {
                Error::new(
                    eyre!("not enough room on OS drive for data"),
                    ErrorKind::InvalidRequest,
                )
            })?;
        }
        if let Some(guid) = disks.iter().find_map(|d| {
            d.guid
                .as_ref()
                .filter(|_| &d.logicalname == logicalname)
                .cloned()
                .or_else(|| {
                    d.partitions.iter().find_map(|p| {
                        p.guid
                            .as_ref()
                            .filter(|_| &p.logicalname == logicalname)
                            .cloned()
                    })
                })
        }) {
            setup_info.guid = Some(guid);
            setup_info.attach = true;
        } else {
            let guid = crate::setup::setup_data_drive(&ctx, logicalname).await?;
            setup_info.guid = Some(guid);
        }
    }

    write_file_atomic(
        rootfs.path().join("config/setup.json"),
        IoFormat::JsonPretty.to_vec(&setup_info)?,
    )
    .await?;

    ctx.install_rootfs.replace(Some(rootfs));

    Ok(setup_info)
}
