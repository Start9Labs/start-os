use std::path::{Path, PathBuf};

use color_eyre::eyre::eyre;
use mbrman::{MBRPartitionEntry, CHS, MBR};
use models::Error;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tokio::process::Command;

use crate::context::InstallContext;
use crate::disk::mount::filesystem::bind::Bind;
use crate::disk::mount::filesystem::block_dev::BlockDev;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::{MountGuard, TmpMountGuard};
use crate::disk::util::DiskInfo;
use crate::disk::OsPartitionInfo;
use crate::util::serde::IoFormat;
use crate::util::{display_none, Invoke};

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
    crate::disk::util::list(&Default::default()).await
}

pub async fn find_wifi_iface() -> Result<Option<String>, Error> {
    let mut ifaces = tokio::fs::read_dir("/sys/class/net").await?;
    while let Some(iface) = ifaces.next_entry().await? {
        if tokio::fs::metadata(iface.path().join("wireless"))
            .await
            .is_ok()
        {
            if let Some(iface) = iface.file_name().into_string().ok() {
                return Ok(Some(iface));
            }
        }
    }

    Ok(None)
}

pub async fn find_eth_iface() -> Result<String, Error> {
    let mut ifaces = tokio::fs::read_dir("/sys/class/net").await?;
    while let Some(iface) = ifaces.next_entry().await? {
        if tokio::fs::metadata(iface.path().join("wireless"))
            .await
            .is_err()
            && tokio::fs::metadata(iface.path().join("device"))
                .await
                .is_ok()
        {
            if let Some(iface) = iface.file_name().into_string().ok() {
                return Ok(iface);
            }
        }
    }
    Err(Error::new(
        eyre!("Could not detect ethernet interface"),
        crate::ErrorKind::Network,
    ))
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

#[command(display(display_none))]
pub async fn execute(
    #[arg] logicalname: PathBuf,
    #[arg(short = 'o')] mut overwrite: bool,
) -> Result<(), Error> {
    let disk = crate::disk::util::list(&Default::default())
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
    let sectors = (disk.capacity / 512) as u32;
    tokio::task::spawn_blocking(move || {
        let mut file = std::fs::File::options()
            .read(true)
            .write(true)
            .open(&logicalname)?;
        let (mut mbr, guid_part) = if overwrite {
            (MBR::new_from(&mut file, 512, rand::random())?, None)
        } else {
            let mut mbr = MBR::read_from(&mut file, 512)?;
            let mut guid_part = None;
            for (idx, part_info) in disk
                .partitions
                .iter()
                .enumerate()
                .map(|(idx, x)| (idx + 1, x))
            {
                if let Some(entry) = mbr.get_mut(idx) {
                    if entry.starting_lba >= 33556480 {
                        if idx < 3 {
                            guid_part = Some(std::mem::replace(entry, MBRPartitionEntry::empty()))
                        }
                        break;
                    }
                    if part_info.guid.is_some() {
                        return Err(Error::new(
                            eyre!("Not enough space before embassy data"),
                            crate::ErrorKind::InvalidRequest,
                        ));
                    }
                    *entry = MBRPartitionEntry::empty();
                }
            }
            (mbr, guid_part)
        };

        mbr[1] = MBRPartitionEntry {
            boot: 0x80,
            first_chs: CHS::empty(),
            sys: 0x0b,
            last_chs: CHS::empty(),
            starting_lba: 2048,
            sectors: 2099200 - 2048,
        };
        mbr[2] = MBRPartitionEntry {
            boot: 0,
            first_chs: CHS::empty(),
            sys: 0x83,
            last_chs: CHS::empty(),
            starting_lba: 2099200,
            sectors: 33556480 - 2099200,
        };

        if overwrite {
            mbr[3] = MBRPartitionEntry {
                boot: 0,
                first_chs: CHS::empty(),
                sys: 0x8e,
                last_chs: CHS::empty(),
                starting_lba: 33556480,
                sectors: sectors - 33556480,
            }
        } else if let Some(guid_part) = guid_part {
            mbr[3] = guid_part;
        }
        mbr.write_into(&mut file)?;

        Ok(())
    })
    .await
    .unwrap()?;

    let boot_part = partition_for(&disk.logicalname, 1);
    let root_part = partition_for(&disk.logicalname, 2);

    Command::new("mkfs.vfat")
        .arg(&boot_part)
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    Command::new("fatlabel")
        .arg(&boot_part)
        .arg("boot")
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;

    Command::new("mkfs.ext4")
        .arg(&root_part)
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    Command::new("e2label")
        .arg(&root_part)
        .arg("rootfs")
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;

    let rootfs = TmpMountGuard::mount(&BlockDev::new(&root_part), ReadWrite).await?;
    tokio::fs::create_dir(rootfs.as_ref().join("config")).await?;
    tokio::fs::create_dir(rootfs.as_ref().join("next")).await?;
    let current = rootfs.as_ref().join("current");
    tokio::fs::create_dir(&current).await?;

    tokio::fs::create_dir(current.join("boot")).await?;
    let boot =
        MountGuard::mount(&BlockDev::new(&boot_part), current.join("boot"), ReadWrite).await?;

    Command::new("unsquashfs")
        .arg("-n")
        .arg("-f")
        .arg("-d")
        .arg(&current)
        .arg("/cdrom/casper/filesystem.squashfs")
        .invoke(crate::ErrorKind::Filesystem)
        .await?;

    tokio::fs::write(
        rootfs.as_ref().join("config/config.yaml"),
        IoFormat::Yaml.to_vec(&PostInstallConfig {
            os_partitions: OsPartitionInfo {
                boot: boot_part.clone(),
                root: root_part.clone(),
            },
            ethernet_interface: eth_iface,
            wifi_interface: wifi_iface,
        })?,
    )
    .await?;

    tokio::fs::write(
        current.join("etc/fstab"),
        format!(
            include_str!("fstab.template"),
            boot = boot_part.display(),
            root = root_part.display()
        ),
    )
    .await?;

    Command::new("chroot")
        .arg(&current)
        .arg("systemd-machine-id-setup")
        .invoke(crate::ErrorKind::Unknown) // TODO systemd
        .await?;

    Command::new("chroot")
        .arg(&current)
        .arg("ssh-keygen")
        .arg("-A")
        .invoke(crate::ErrorKind::Unknown) // TODO ssh
        .await?;

    let dev = MountGuard::mount(&Bind::new("/dev"), current.join("dev"), ReadWrite).await?;
    let sys = MountGuard::mount(&Bind::new("/sys"), current.join("sys"), ReadWrite).await?;
    let proc = MountGuard::mount(&Bind::new("/proc"), current.join("proc"), ReadWrite).await?;

    Command::new("chroot")
        .arg(&current)
        .arg("update-grub")
        .invoke(crate::ErrorKind::Unknown) // TODO grub
        .await?;
    Command::new("chroot")
        .arg(&current)
        .arg("grub-install")
        .arg(&disk.logicalname)
        .invoke(crate::ErrorKind::Unknown) // TODO grub
        .await?;

    dev.unmount().await?;
    sys.unmount().await?;
    proc.unmount().await?;
    boot.unmount().await?;
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
