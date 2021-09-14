use std::path::Path;

use anyhow::anyhow;
use tokio::process::Command;

use crate::util::Invoke;
use crate::{Error, ResultExt};

pub const PASSWORD_PATH: &'static str = "/etc/embassy/password";
pub const DEFAULT_PASSWORD: &'static str = "password";

pub async fn create<I: IntoIterator<Item = P>, P: AsRef<Path>>(
    pool_name: &str,
    disks: I,
    password: &str,
) -> Result<String, Error> {
    let guid = create_pool(pool_name, disks).await?;
    create_fs(pool_name, password).await?;
    export(pool_name).await?;
    Ok(guid)
}

pub async fn load<P: AsRef<Path>>(
    guid: &str,
    pool_name: &str,
    datadir: P,
    password: &str,
) -> Result<(), Error> {
    import(guid).await?;
    mount(pool_name, datadir, password).await?;
    Ok(())
}

pub async fn create_pool<I: IntoIterator<Item = P>, P: AsRef<Path>>(
    pool_name: &str,
    disks: I,
) -> Result<String, Error> {
    let mut cmd = Command::new("zpool");
    cmd.arg("create").arg(pool_name);
    for disk in disks {
        cmd.arg(disk.as_ref());
    }
    cmd.invoke(crate::ErrorKind::Zfs).await?;
    Ok(String::from_utf8(
        Command::new("zpool")
            .arg("get")
            .arg("-H")
            .arg("-ovalue")
            .arg("guid")
            .arg(pool_name)
            .invoke(crate::ErrorKind::Zfs)
            .await?,
    )?)
}

pub async fn create_fs(pool_name: &str, password: &str) -> Result<(), Error> {
    tokio::fs::write(PASSWORD_PATH, password)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, PASSWORD_PATH))?;
    Command::new("zfs")
        .arg("create")
        .arg("-o")
        .arg("reservation=5G")
        .arg("-o")
        .arg("encryption=on")
        .arg("-o")
        .arg("keylocation=file:///etc/embassy/password")
        .arg("-o")
        .arg("keyformat=passphrase")
        .arg(format!("{}/main", pool_name))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Command::new("zfs")
        .arg("create")
        .arg("-o")
        .arg("reservation=5G")
        .arg(format!("{}/updates", pool_name))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Command::new("zfs")
        .arg("create")
        .arg("-o")
        .arg("encryption=on")
        .arg("-o")
        .arg("keylocation=file:///etc/embassy/password")
        .arg("-o")
        .arg("keyformat=passphrase")
        .arg(format!("{}/package-data", pool_name))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Command::new("zfs")
        .arg("create")
        .arg("-o")
        .arg("encryption=on")
        .arg("-o")
        .arg("keylocation=file:///etc/embassy/password")
        .arg("-o")
        .arg("keyformat=passphrase")
        .arg(format!("{}/tmp", pool_name))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    tokio::fs::remove_file(PASSWORD_PATH)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, PASSWORD_PATH))?;
    Ok(())
}

pub async fn create_swap(pool_name: &str) -> Result<(), Error> {
    let pagesize = String::from_utf8(
        Command::new("getconf")
            .arg("PAGESIZE")
            .invoke(crate::ErrorKind::Zfs)
            .await?,
    )?;
    Command::new("zfs")
        .arg("create")
        .arg("-V8G")
        .arg("-b")
        .arg(pagesize)
        .arg("-o")
        .arg("logbias=throughput")
        .arg("-o")
        .arg("sync=always")
        .arg("-o")
        .arg("primarycache=metadata")
        .arg("-o")
        .arg("com.sun:auto-snapshot=false")
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Command::new("mkswap")
        .arg("-f")
        .arg(Path::new("/dev/zvol").join(pool_name).join("swap"))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Ok(())
}

pub async fn use_swap(pool_name: &str) -> Result<(), Error> {
    Command::new("swapon")
        .arg(Path::new("/dev/zvol").join(pool_name).join("swap"))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Ok(())
}

pub async fn export(pool_name: &str) -> Result<(), Error> {
    Command::new("zpool")
        .arg("export")
        .arg(pool_name)
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Ok(())
}

pub async fn import(guid: &str) -> Result<(), Error> {
    Command::new("zpool")
        .arg("import")
        .arg("-f")
        .arg(guid)
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Ok(())
}

pub async fn mount<P: AsRef<Path>>(
    pool_name: &str,
    datadir: P,
    password: &str,
) -> Result<(), Error> {
    let mountpoint = String::from_utf8(
        Command::new("zfs")
            .arg("get")
            .arg("-H")
            .arg("-ovalue")
            .arg("mountpoint")
            .arg(pool_name)
            .invoke(crate::ErrorKind::Zfs)
            .await?,
    )?;
    if Path::new(mountpoint.trim()) != datadir.as_ref() {
        Command::new("zfs")
            .arg("set")
            .arg(format!("mountpoint={}", datadir.as_ref().display()))
            .arg(pool_name)
            .invoke(crate::ErrorKind::Zfs)
            .await?;
    }

    tokio::fs::write(PASSWORD_PATH, password)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, PASSWORD_PATH))?;
    Command::new("zfs")
        .arg("load-key")
        .arg(format!("{}/main", pool_name))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Command::new("zfs")
        .arg("load-key")
        .arg(format!("{}/package-data", pool_name))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Command::new("zfs")
        .arg("load-key")
        .arg(format!("{}/tmp", pool_name))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    tokio::fs::remove_file(PASSWORD_PATH)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, PASSWORD_PATH))?;

    Command::new("zfs")
        .arg("mount")
        .arg(format!("{}/main", pool_name))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Command::new("zfs")
        .arg("mount")
        .arg(format!("{}/package-data", pool_name))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Command::new("zfs")
        .arg("mount")
        .arg(format!("{}/tmp", pool_name))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Ok(())
}
