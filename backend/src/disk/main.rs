use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use color_eyre::eyre::eyre;
use tokio::process::Command;
use tracing::instrument;

use super::fsck::{RepairStrategy, RequiresReboot};
use super::util::pvscan;
use crate::disk::mount::filesystem::block_dev::mount;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::util::unmount;
use crate::util::Invoke;
use crate::{Error, ResultExt};

pub const PASSWORD_PATH: &'static str = "/etc/embassy/password";
pub const DEFAULT_PASSWORD: &'static str = "password";
pub const MAIN_FS_SIZE: FsSize = FsSize::Gigabytes(8);

#[instrument(skip(disks, datadir, password))]
pub async fn create<I, P>(
    disks: &I,
    pvscan: &BTreeMap<PathBuf, Option<String>>,
    datadir: impl AsRef<Path>,
    password: &str,
) -> Result<String, Error>
where
    for<'a> &'a I: IntoIterator<Item = &'a P>,
    P: AsRef<Path>,
{
    let guid = create_pool(disks, pvscan).await?;
    create_all_fs(&guid, &datadir, password).await?;
    export(&guid, datadir).await?;
    Ok(guid)
}

#[instrument(skip(disks))]
pub async fn create_pool<I, P>(
    disks: &I,
    pvscan: &BTreeMap<PathBuf, Option<String>>,
) -> Result<String, Error>
where
    for<'a> &'a I: IntoIterator<Item = &'a P>,
    P: AsRef<Path>,
{
    Command::new("dmsetup")
        .arg("remove_all") // TODO: find a higher finesse way to do this for portability reasons
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    for disk in disks {
        if pvscan.contains_key(disk.as_ref()) {
            Command::new("pvremove")
                .arg("-yff")
                .arg(disk.as_ref())
                .invoke(crate::ErrorKind::DiskManagement)
                .await?;
        }
        tokio::fs::write(disk.as_ref(), &[0; 2048]).await?; // wipe partition table
        Command::new("pvcreate")
            .arg("-yff")
            .arg(disk.as_ref())
            .invoke(crate::ErrorKind::DiskManagement)
            .await?;
    }
    let guid = format!(
        "EMBASSY_{}",
        base32::encode(
            base32::Alphabet::RFC4648 { padding: false },
            &rand::random::<[u8; 32]>(),
        )
    );
    let mut cmd = Command::new("vgcreate");
    cmd.arg("-y").arg(&guid);
    for disk in disks {
        cmd.arg(disk.as_ref());
    }
    cmd.invoke(crate::ErrorKind::DiskManagement).await?;
    Ok(guid)
}

#[derive(Debug, Clone, Copy)]
pub enum FsSize {
    Gigabytes(usize),
    FreePercentage(usize),
}

#[instrument(skip(datadir, password))]
pub async fn create_fs<P: AsRef<Path>>(
    guid: &str,
    datadir: P,
    name: &str,
    size: FsSize,
    password: &str,
) -> Result<(), Error> {
    tokio::fs::write(PASSWORD_PATH, password)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, PASSWORD_PATH))?;
    let mut cmd = Command::new("lvcreate");
    match size {
        FsSize::Gigabytes(a) => cmd.arg("-L").arg(format!("{}G", a)),
        FsSize::FreePercentage(a) => cmd.arg("-l").arg(format!("{}%FREE", a)),
    };
    cmd.arg("-y")
        .arg("-n")
        .arg(name)
        .arg(guid)
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    Command::new("cryptsetup")
        .arg("-q")
        .arg("luksFormat")
        .arg(format!("--key-file={}", PASSWORD_PATH))
        .arg(format!("--keyfile-size={}", password.len()))
        .arg(Path::new("/dev").join(guid).join(name))
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    Command::new("cryptsetup")
        .arg("-q")
        .arg("luksOpen")
        .arg(format!("--key-file={}", PASSWORD_PATH))
        .arg(format!("--keyfile-size={}", password.len()))
        .arg(Path::new("/dev").join(guid).join(name))
        .arg(format!("{}_{}", guid, name))
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    Command::new("mkfs.ext4")
        .arg(Path::new("/dev/mapper").join(format!("{}_{}", guid, name)))
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    mount(
        Path::new("/dev/mapper").join(format!("{}_{}", guid, name)),
        datadir.as_ref().join(name),
        ReadWrite,
    )
    .await?;
    tokio::fs::remove_file(PASSWORD_PATH)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, PASSWORD_PATH))?;
    Ok(())
}

#[instrument(skip(datadir, password))]
pub async fn create_all_fs<P: AsRef<Path>>(
    guid: &str,
    datadir: P,
    password: &str,
) -> Result<(), Error> {
    create_fs(guid, &datadir, "main", MAIN_FS_SIZE, password).await?;
    create_fs(
        guid,
        &datadir,
        "package-data",
        FsSize::FreePercentage(100),
        password,
    )
    .await?;
    Ok(())
}

#[instrument(skip(datadir))]
pub async fn unmount_fs<P: AsRef<Path>>(guid: &str, datadir: P, name: &str) -> Result<(), Error> {
    unmount(datadir.as_ref().join(name)).await?;
    Command::new("cryptsetup")
        .arg("-q")
        .arg("luksClose")
        .arg(format!("{}_{}", guid, name))
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;

    Ok(())
}

#[instrument(skip(datadir))]
pub async fn unmount_all_fs<P: AsRef<Path>>(guid: &str, datadir: P) -> Result<(), Error> {
    unmount_fs(guid, &datadir, "main").await?;
    unmount_fs(guid, &datadir, "package-data").await?;
    Command::new("dmsetup")
        .arg("remove_all") // TODO: find a higher finesse way to do this for portability reasons
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    Ok(())
}

#[instrument(skip(datadir))]
pub async fn export<P: AsRef<Path>>(guid: &str, datadir: P) -> Result<(), Error> {
    unmount_all_fs(guid, datadir).await?;
    Command::new("vgchange")
        .arg("-an")
        .arg(guid)
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    Command::new("vgexport")
        .arg(guid)
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    Ok(())
}

#[instrument(skip(datadir, password))]
pub async fn import<P: AsRef<Path>>(
    guid: &str,
    datadir: P,
    repair: RepairStrategy,
    password: &str,
) -> Result<(), Error> {
    let scan = pvscan().await?;
    if scan
        .values()
        .filter_map(|a| a.as_ref())
        .filter(|a| a.starts_with("EMBASSY_"))
        .next()
        .is_none()
    {
        return Err(Error::new(
            eyre!("Embassy disk not found."),
            crate::ErrorKind::DiskNotAvailable,
        ));
    }
    if !scan
        .values()
        .filter_map(|a| a.as_ref())
        .any(|id| id == guid)
    {
        return Err(Error::new(
            eyre!("An Embassy disk was found, but it is not the correct disk for this device."),
            crate::ErrorKind::IncorrectDisk,
        ));
    }
    Command::new("dmsetup")
        .arg("remove_all") // TODO: find a higher finesse way to do this for portability reasons
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    match Command::new("vgimport")
        .arg(guid)
        .invoke(crate::ErrorKind::DiskManagement)
        .await
    {
        Ok(_) => Ok(()),
        Err(e)
            if format!("{}", e.source)
                .lines()
                .any(|l| l.trim() == format!("Volume group \"{}\" is not exported", guid)) =>
        {
            Ok(())
        }
        Err(e) => Err(e),
    }?;
    Command::new("vgchange")
        .arg("-ay")
        .arg(guid)
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    mount_all_fs(guid, datadir, repair, password).await?;
    Ok(())
}

#[instrument(skip(datadir, password))]
pub async fn mount_fs<P: AsRef<Path>>(
    guid: &str,
    datadir: P,
    name: &str,
    repair: RepairStrategy,
    password: &str,
) -> Result<RequiresReboot, Error> {
    tokio::fs::write(PASSWORD_PATH, password)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, PASSWORD_PATH))?;
    Command::new("cryptsetup")
        .arg("-q")
        .arg("luksOpen")
        .arg(format!("--key-file={}", PASSWORD_PATH))
        .arg(format!("--keyfile-size={}", password.len()))
        .arg(Path::new("/dev").join(guid).join(name))
        .arg(format!("{}_{}", guid, name))
        .invoke(crate::ErrorKind::DiskManagement)
        .await?;
    let mapper_path = Path::new("/dev/mapper").join(format!("{}_{}", guid, name));
    let reboot = repair.e2fsck(&mapper_path).await?;
    mount(&mapper_path, datadir.as_ref().join(name), ReadWrite).await?;

    tokio::fs::remove_file(PASSWORD_PATH)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, PASSWORD_PATH))?;

    Ok(reboot)
}

#[instrument(skip(datadir, password))]
pub async fn mount_all_fs<P: AsRef<Path>>(
    guid: &str,
    datadir: P,
    repair: RepairStrategy,
    password: &str,
) -> Result<RequiresReboot, Error> {
    let mut reboot = RequiresReboot(false);
    reboot |= mount_fs(guid, &datadir, "main", repair, password).await?;
    reboot |= mount_fs(guid, &datadir, "package-data", repair, password).await?;
    Ok(reboot)
}
