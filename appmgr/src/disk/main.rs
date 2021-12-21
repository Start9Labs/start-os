use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use tokio::process::Command;
use tracing::instrument;

use crate::disk::mount::filesystem::block_dev::mount;
use crate::disk::mount::util::unmount;
use crate::util::Invoke;
use crate::{Error, ResultExt};

pub const PASSWORD_PATH: &'static str = "/etc/embassy/password";
pub const DEFAULT_PASSWORD: &'static str = "password";
pub const MAIN_FS_SIZE: FsSize = FsSize::Gigabytes(8);
pub const SWAP_SIZE: FsSize = FsSize::Gigabytes(8);

// TODO: use IncorrectDisk / DiskNotAvailable / DiskCorrupted

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
    swap: bool,
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
    if swap {
        Command::new("mkswap")
            .arg("-f")
            .arg(Path::new("/dev/mapper").join(format!("{}_{}", guid, name)))
            .invoke(crate::ErrorKind::DiskManagement)
            .await?;
        // Command::new("swapon")
        //     .arg(Path::new("/dev/mapper").join(format!("{}_{}", guid, name)))
        //     .invoke(crate::ErrorKind::DiskManagement)
        //     .await?;
    } else {
        Command::new("mkfs.ext4")
            .arg(Path::new("/dev/mapper").join(format!("{}_{}", guid, name)))
            .invoke(crate::ErrorKind::DiskManagement)
            .await?;
        mount(
            Path::new("/dev/mapper").join(format!("{}_{}", guid, name)),
            datadir.as_ref().join(name),
        )
        .await?;
    }
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
    create_fs(guid, &datadir, "main", MAIN_FS_SIZE, false, password).await?;
    create_fs(guid, &datadir, "swap", SWAP_SIZE, true, password).await?;
    create_fs(
        guid,
        &datadir,
        "package-data",
        FsSize::FreePercentage(100),
        false,
        password,
    )
    .await?;
    Ok(())
}

#[instrument(skip(datadir))]
pub async fn unmount_fs<P: AsRef<Path>>(
    guid: &str,
    datadir: P,
    name: &str,
    swap: bool,
) -> Result<(), Error> {
    if swap {
        // Command::new("swapoff")
        //     .arg(Path::new("/dev/mapper").join(format!("{}_{}", guid, name)))
        //     .invoke(crate::ErrorKind::DiskManagement)
        //     .await?;
    } else {
        unmount(datadir.as_ref().join(name)).await?;
    }
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
    unmount_fs(guid, &datadir, "main", false).await?;
    unmount_fs(guid, &datadir, "swap", true).await?;
    unmount_fs(guid, &datadir, "package-data", false).await?;
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
pub async fn import<P: AsRef<Path>>(guid: &str, datadir: P, password: &str) -> Result<(), Error> {
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
    mount_all_fs(guid, datadir, password).await?;
    Ok(())
}

#[instrument(skip(datadir, password))]
pub async fn mount_fs<P: AsRef<Path>>(
    guid: &str,
    datadir: P,
    name: &str,
    swap: bool,
    password: &str,
) -> Result<(), Error> {
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
    if swap {
        // Command::new("swapon")
        //     .arg(Path::new("/dev/mapper").join(format!("{}_{}", guid, name)))
        //     .invoke(crate::ErrorKind::DiskManagement)
        //     .await?;
    } else {
        mount(
            Path::new("/dev/mapper").join(format!("{}_{}", guid, name)),
            datadir.as_ref().join(name),
        )
        .await?;
    }

    tokio::fs::remove_file(PASSWORD_PATH)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, PASSWORD_PATH))?;

    Ok(())
}

#[instrument(skip(datadir, password))]
pub async fn mount_all_fs<P: AsRef<Path>>(
    guid: &str,
    datadir: P,
    password: &str,
) -> Result<(), Error> {
    mount_fs(guid, &datadir, "main", false, password).await?;
    mount_fs(guid, &datadir, "swap", true, password).await?;
    mount_fs(guid, &datadir, "package-data", false, password).await?;
    Ok(())
}
