use std::path::{Path, PathBuf};

use clap::Parser;
use color_eyre::eyre::eyre;
use models::Error;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use ts_rs::TS;

use crate::context::config::ServerConfig;
use crate::context::{CliContext, InstallContext};
use crate::disk::mount::filesystem::bind::Bind;
use crate::disk::mount::filesystem::block_dev::BlockDev;
use crate::disk::mount::filesystem::efivarfs::EfiVarFs;
use crate::disk::mount::filesystem::overlayfs::OverlayFs;
use crate::disk::mount::filesystem::{MountType, ReadWrite};
use crate::disk::mount::guard::{GenericMountGuard, MountGuard, TmpMountGuard};
use crate::disk::util::{DiskInfo, PartitionTable};
use crate::disk::OsPartitionInfo;
use crate::net::utils::find_eth_iface;
use crate::prelude::*;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::util::io::{open_file, TmpDir};
use crate::util::serde::IoFormat;
use crate::util::Invoke;
use crate::ARCH;

mod gpt;
mod mbr;

pub fn install<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("disk", disk::<C>())
        .subcommand(
            "execute",
            from_fn_async(execute::<InstallContext>)
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "reboot",
            from_fn_async(reboot)
                .no_display()
                .with_call_remote::<CliContext>(),
        )
}

pub fn disk<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand(
        "list",
        from_fn_async(list)
            .no_display()
            .with_call_remote::<CliContext>(),
    )
}

pub async fn list(_: InstallContext) -> Result<Vec<DiskInfo>, Error> {
    let skip = match async {
        Ok::<_, Error>(
            Path::new(
                &String::from_utf8(
                    Command::new("grub-probe-default")
                        .arg("-t")
                        .arg("disk")
                        .arg("/run/live/medium")
                        .invoke(crate::ErrorKind::Grub)
                        .await?,
                )?
                .trim(),
            )
            .to_owned(),
        )
    }
    .await
    {
        Ok(a) => Some(a),
        Err(e) => {
            tracing::error!("Could not determine live usb device: {}", e);
            tracing::debug!("{:?}", e);
            None
        }
    };
    Ok(crate::disk::util::list(&Default::default())
        .await?
        .into_iter()
        .filter(|i| Some(&*i.logicalname) != skip.as_deref())
        .collect())
}

pub fn partition_for(disk: impl AsRef<Path>, idx: usize) -> PathBuf {
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
pub struct ExecuteParams {
    logicalname: PathBuf,
    #[arg(short = 'o')]
    overwrite: bool,
}

pub async fn execute<C: Context>(
    _: C,
    ExecuteParams {
        logicalname,
        mut overwrite,
    }: ExecuteParams,
) -> Result<(), Error> {
    let mut disk = crate::disk::util::list(&Default::default())
        .await?
        .into_iter()
        .find(|d| &d.logicalname == &logicalname)
        .ok_or_else(|| {
            Error::new(
                eyre!("Unknown disk {}", logicalname.display()),
                crate::ErrorKind::DiskManagement,
            )
        })?;
    let eth_iface = find_eth_iface().await?;

    overwrite |= disk.guid.is_none() && disk.partitions.iter().all(|p| p.guid.is_none());

    if !overwrite
        && (disk
            .guid
            .as_ref()
            .map_or(false, |g| g.starts_with("EMBASSY_"))
            || disk
                .partitions
                .iter()
                .flat_map(|p| p.guid.as_ref())
                .any(|g| g.starts_with("EMBASSY_")))
    {
        return Err(Error::new(
            eyre!("installing over versions before 0.3.6 is unsupported"),
            ErrorKind::InvalidRequest,
        ));
    }

    let part_info = partition(&mut disk, overwrite).await?;

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
                if tokio::fs::metadata(guard.path().join("config/upgrade"))
                    .await
                    .is_ok()
                {
                    tokio::fs::remove_file(guard.path().join("config/upgrade")).await?;
                }
                if tokio::fs::metadata(guard.path().join("config/disk.guid"))
                    .await
                    .is_ok()
                {
                    tokio::fs::remove_file(guard.path().join("config/disk.guid")).await?;
                }
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
            ethernet_interface: Some(eth_iface),
            ..Default::default()
        })?,
    )
    .await?;

    let lower = TmpMountGuard::mount(&BlockDev::new(&image_path), MountType::ReadOnly).await?;
    let work = config_path.join("work");
    let upper = config_path.join("overlay");
    let overlay =
        TmpMountGuard::mount(&OverlayFs::new(&lower.path(), &upper, &work), ReadWrite).await?;

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
        install.arg("--target=i386-pc");
    } else {
        match ARCH {
            "x86_64" => install.arg("--target=x86_64-efi"),
            "aarch64" => install.arg("--target=arm64-efi"),
            _ => &mut install,
        };
    }
    install
        .arg(&disk.logicalname)
        .invoke(crate::ErrorKind::Grub)
        .await?;

    Command::new("chroot")
        .arg(overlay.path())
        .arg("update-grub2")
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

    rootfs.unmount().await?;

    Ok(())
}

pub async fn reboot(ctx: InstallContext) -> Result<(), Error> {
    Command::new("sync")
        .invoke(crate::ErrorKind::Filesystem)
        .await?;
    ctx.shutdown.send(()).unwrap();
    Ok(())
}
