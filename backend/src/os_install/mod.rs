use std::path::{Path, PathBuf};

use color_eyre::eyre::eyre;
use models::Error;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tokio::process::Command;

use crate::context::InstallContext;
use crate::disk::mount::filesystem::bind::Bind;
use crate::disk::mount::filesystem::block_dev::BlockDev;
use crate::disk::mount::filesystem::efivarfs::EfiVarFs;
use crate::disk::mount::filesystem::{MountType, ReadWrite};
use crate::disk::mount::guard::{MountGuard, TmpMountGuard};
use crate::disk::util::{DiskInfo, PartitionTable};
use crate::disk::OsPartitionInfo;
use crate::net::utils::{find_eth_iface, find_wifi_iface};
use crate::util::serde::IoFormat;
use crate::util::{display_none, Invoke};
use crate::ARCH;

mod gpt;
mod mbr;

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct PostInstallConfig {
    os_partitions: OsPartitionInfo,
    ethernet_interface: String,
    wifi_interface: Option<String>,
}

#[command(subcommands(disk, execute, reboot))]
pub fn install() -> Result<(), Error> {
    Ok(())
}

#[command(subcommands(list))]
pub fn disk() -> Result<(), Error> {
    Ok(())
}

#[command(display(display_none))]
pub async fn list() -> Result<Vec<DiskInfo>, Error> {
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

#[command(display(display_none))]
pub async fn execute(
    #[arg] logicalname: PathBuf,
    #[arg(short = 'o')] mut overwrite: bool,
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
    let wifi_iface = find_wifi_iface().await?;

    overwrite |= disk.guid.is_none() && disk.partitions.iter().all(|p| p.guid.is_none());

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
            TmpMountGuard::mount(&BlockDev::new(part_info.root.clone()), MountType::ReadOnly).await
        {
            if let Err(e) = async {
                // cp -r ${guard}/config /tmp/config
                Command::new("cp")
                    .arg("-r")
                    .arg(guard.as_ref().join("config"))
                    .arg("/tmp/config.bak")
                    .invoke(crate::ErrorKind::Filesystem)
                    .await?;
                if tokio::fs::metadata(guard.as_ref().join("config/upgrade"))
                    .await
                    .is_ok()
                {
                    tokio::fs::remove_file(guard.as_ref().join("config/upgrade")).await?;
                }
                guard.unmount().await
            }
            .await
            {
                tracing::error!("Error recovering previous config: {e}");
                tracing::debug!("{e:?}");
            }
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
    if tokio::fs::metadata("/tmp/config.bak").await.is_ok() {
        Command::new("cp")
            .arg("-r")
            .arg("/tmp/config.bak")
            .arg(rootfs.as_ref().join("config"))
            .invoke(crate::ErrorKind::Filesystem)
            .await?;
    } else {
        tokio::fs::create_dir(rootfs.as_ref().join("config")).await?;
    }
    tokio::fs::create_dir(rootfs.as_ref().join("next")).await?;
    let current = rootfs.as_ref().join("current");
    tokio::fs::create_dir(&current).await?;

    tokio::fs::create_dir(current.join("boot")).await?;
    let boot = MountGuard::mount(
        &BlockDev::new(&part_info.boot),
        current.join("boot"),
        ReadWrite,
    )
    .await?;

    let efi = if let Some(efi) = &part_info.efi {
        Some(MountGuard::mount(&BlockDev::new(efi), current.join("boot/efi"), ReadWrite).await?)
    } else {
        None
    };

    Command::new("unsquashfs")
        .arg("-n")
        .arg("-f")
        .arg("-d")
        .arg(&current)
        .arg("/run/live/medium/live/filesystem.squashfs")
        .invoke(crate::ErrorKind::Filesystem)
        .await?;

    tokio::fs::write(
        rootfs.as_ref().join("config/config.yaml"),
        IoFormat::Yaml.to_vec(&PostInstallConfig {
            os_partitions: part_info.clone(),
            ethernet_interface: eth_iface,
            wifi_interface: wifi_iface,
        })?,
    )
    .await?;

    tokio::fs::write(
        current.join("etc/fstab"),
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
        .arg(&current)
        .arg("systemd-machine-id-setup")
        .invoke(crate::ErrorKind::Systemd)
        .await?;

    Command::new("chroot")
        .arg(&current)
        .arg("ssh-keygen")
        .arg("-A")
        .invoke(crate::ErrorKind::OpenSsh)
        .await?;

    let dev = MountGuard::mount(
        &Bind::new(rootfs.as_ref()),
        current.join("media/embassy/embassyfs"),
        MountType::ReadOnly,
    )
    .await?;
    let dev = MountGuard::mount(&Bind::new("/dev"), current.join("dev"), ReadWrite).await?;
    let proc = MountGuard::mount(&Bind::new("/proc"), current.join("proc"), ReadWrite).await?;
    let sys = MountGuard::mount(&Bind::new("/sys"), current.join("sys"), ReadWrite).await?;
    let efivarfs = if tokio::fs::metadata("/sys/firmware/efi").await.is_ok() {
        Some(
            MountGuard::mount(
                &EfiVarFs,
                current.join("sys/firmware/efi/efivars"),
                ReadWrite,
            )
            .await?,
        )
    } else {
        None
    };

    let mut install = Command::new("chroot");
    install.arg(&current).arg("grub-install");
    if tokio::fs::metadata("/sys/firmware/efi").await.is_err() {
        install.arg("--target=i386-pc");
    } else {
        match *ARCH {
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
        .arg(&current)
        .arg("update-grub2")
        .invoke(crate::ErrorKind::Grub)
        .await?;

    dev.unmount(false).await?;
    if let Some(efivarfs) = efivarfs {
        efivarfs.unmount(false).await?;
    }
    sys.unmount(false).await?;
    proc.unmount(false).await?;
    if let Some(efi) = efi {
        efi.unmount(false).await?;
    }
    boot.unmount(false).await?;
    rootfs.unmount().await?;

    Ok(())
}

#[command(display(display_none))]
pub async fn reboot(#[context] ctx: InstallContext) -> Result<(), Error> {
    Command::new("sync")
        .invoke(crate::ErrorKind::Filesystem)
        .await?;
    ctx.shutdown.send(()).unwrap();
    Ok(())
}
