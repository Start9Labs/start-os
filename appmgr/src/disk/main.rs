use std::path::Path;

use anyhow::anyhow;
use tokio::process::Command;

use crate::context::rpc::RpcContextConfig;
use crate::util::Invoke;
use crate::{Error, ResultExt};

pub const PASSWORD_PATH: &'static str = "/etc/embassy/password";

pub async fn create(
    cfg: &RpcContextConfig,
    disks: &[&str],
    password: &str,
) -> Result<String, Error> {
    let guid = create_pool(cfg, disks).await?;
    create_fs(cfg, password).await?;
    export(cfg).await?;
    Ok(guid)
}

pub async fn load(cfg: &RpcContextConfig, guid: &str, password: &str) -> Result<(), Error> {
    import(guid).await?;
    mount(cfg, password).await?;
    Ok(())
}

pub async fn create_pool(cfg: &RpcContextConfig, disks: &[&str]) -> Result<String, Error> {
    Command::new("zpool")
        .arg("create")
        .arg(cfg.zfs_pool_name())
        .args(disks)
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Ok(String::from_utf8(
        Command::new("zpool")
            .arg("get")
            .arg("-H")
            .arg("-ovalue")
            .arg("guid")
            .arg(cfg.zfs_pool_name())
            .invoke(crate::ErrorKind::Zfs)
            .await?,
    )?)
}

pub async fn create_fs(cfg: &RpcContextConfig, password: &str) -> Result<(), Error> {
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
        .arg(format!("{}/main", cfg.zfs_pool_name()))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Command::new("zfs")
        .arg("create")
        .arg("-o")
        .arg("reservation=5G")
        .arg(format!("{}/updates", cfg.zfs_pool_name()))
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
        .arg(format!("{}/package-data", cfg.zfs_pool_name()))
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
        .arg(format!("{}/tmp", cfg.zfs_pool_name()))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    tokio::fs::remove_file(PASSWORD_PATH)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, PASSWORD_PATH))?;
    Ok(())
}

pub async fn create_swap(cfg: &RpcContextConfig) -> Result<(), Error> {
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
        .arg(
            Path::new("/dev/zvol")
                .join(cfg.zfs_pool_name())
                .join("swap"),
        )
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Ok(())
}

pub async fn use_swap(cfg: &RpcContextConfig) -> Result<(), Error> {
    Command::new("swapon")
        .arg(
            Path::new("/dev/zvol")
                .join(cfg.zfs_pool_name())
                .join("swap"),
        )
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Ok(())
}

pub async fn export(cfg: &RpcContextConfig) -> Result<(), Error> {
    Command::new("zpool")
        .arg("export")
        .arg(cfg.zfs_pool_name())
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Ok(())
}

/// BLOCKING
pub fn export_blocking(pool: &str) -> Result<(), Error> {
    let output = std::process::Command::new("zpool")
        .arg("export")
        .arg(pool)
        .output()?;
    if !output.status.success() {
        Err(Error::new(
            anyhow!("{}", String::from_utf8(output.stderr)?),
            crate::ErrorKind::Zfs,
        ))
    } else {
        Ok(())
    }
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

pub async fn mount(cfg: &RpcContextConfig, password: &str) -> Result<(), Error> {
    let mountpoint = String::from_utf8(
        Command::new("zfs")
            .arg("get")
            .arg("-H")
            .arg("-ovalue")
            .arg("mountpoint")
            .arg(cfg.zfs_pool_name())
            .invoke(crate::ErrorKind::Zfs)
            .await?,
    )?;
    if Path::new(mountpoint.trim()) != &cfg.datadir() {
        Command::new("zfs")
            .arg("set")
            .arg(format!("mountpoint={}", cfg.datadir().display()))
            .arg(cfg.zfs_pool_name())
            .invoke(crate::ErrorKind::Zfs)
            .await?;
    }

    tokio::fs::write(PASSWORD_PATH, password)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, PASSWORD_PATH))?;
    Command::new("zfs")
        .arg("load-key")
        .arg(format!("{}/main", cfg.zfs_pool_name()))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Command::new("zfs")
        .arg("load-key")
        .arg(format!("{}/package-data", cfg.zfs_pool_name()))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Command::new("zfs")
        .arg("load-key")
        .arg(format!("{}/tmp", cfg.zfs_pool_name()))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    tokio::fs::remove_file(PASSWORD_PATH)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, PASSWORD_PATH))?;

    Command::new("zfs")
        .arg("mount")
        .arg(format!("{}/main", cfg.zfs_pool_name()))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Command::new("zfs")
        .arg("mount")
        .arg(format!("{}/package-data", cfg.zfs_pool_name()))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Command::new("zfs")
        .arg("mount")
        .arg(format!("{}/tmp", cfg.zfs_pool_name()))
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Ok(())
}
