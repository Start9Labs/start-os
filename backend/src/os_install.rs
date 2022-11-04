use std::path::{Path, PathBuf};

use color_eyre::eyre::eyre;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWriteExt;
use tokio::process::Command;

use crate::disk::mount::filesystem::bind::Bind;
use crate::disk::mount::filesystem::block_dev::BlockDev;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::{MountGuard, TmpMountGuard};
use crate::disk::OsPartitionInfo;
use crate::util::serde::IoFormat;
use crate::util::{display_none, Invoke};
use crate::Error;

const OS_PARTITION_SCHEME: &'static str = concat!(
    "n\n",
    "p\n",
    "1\n",
    "2048\n",
    "2099199\n",
    "n\n",
    "p\n",
    "2\n",
    "2099200\n",
    "33556479\n",
    "t\n",
    "1\n",
    "b\n",
    "a\n",
    "1\n",
);
const DATA_PARTITION_SCHEME: &'static str =
    concat!("n\n", "p\n", "3\n", "\n", "\n", "t\n", "3\n", "8e\n");

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct PostInstallConfig {
    os_partitions: OsPartitionInfo,
    ethernet_interface: String,
    wifi_interface: Option<String>,
}

#[command(subcommands(status, execute))]
pub fn install() -> Result<(), Error> {
    Ok(())
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct InstallTarget {
    logicalname: PathBuf,
    embassy_data: bool,
}

#[command(display(display_none))]
pub async fn status() -> Result<Vec<InstallTarget>, Error> {
    let disks = crate::disk::util::list(&Default::default()).await?;
    Ok(disks
        .into_iter()
        .map(|d| InstallTarget {
            logicalname: d.logicalname,
            embassy_data: d.guid.is_some() || d.partitions.into_iter().any(|p| p.guid.is_some()),
        })
        .collect())
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
    let mut fdisk = Command::new("fdisk")
        .arg(&logicalname)
        .stdin(std::process::Stdio::piped())
        .spawn()?;
    let mut fdisk_stdin = fdisk.stdin.take().unwrap();
    overwrite |= disk.guid.is_none() && disk.partitions.iter().all(|p| p.guid.is_none());
    if overwrite {
        fdisk_stdin.write_all(b"o\n").await?; // NEW MBR
    } else {
        for (idx, part_info) in disk.partitions.iter().enumerate() {
            if part_info.guid.is_none() {
                fdisk_stdin
                    .write_all(format!("d\n{}\n", idx + 1).as_bytes())
                    .await?; // delete partition (partition indexes start at 1 not 0)
            }
        }
    }
    fdisk_stdin
        .write_all(OS_PARTITION_SCHEME.as_bytes())
        .await?;
    if overwrite {
        fdisk_stdin
            .write_all(DATA_PARTITION_SCHEME.as_bytes())
            .await?;
    }
    fdisk_stdin.write_all(b"w\n").await?;
    if !fdisk.wait().await?.success() {
        return Err(Error::new(
            eyre!("fdisk failed"),
            crate::ErrorKind::DiskManagement,
        ));
    }

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
        .invoke(crate::ErrorKind::Unknown) // TODO grub
        .await?;

    dev.unmount().await?;
    sys.unmount().await?;
    proc.unmount().await?;
    boot.unmount().await?;
    rootfs.unmount().await?;

    Ok(())
}
