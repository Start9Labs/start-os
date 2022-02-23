use std::net::IpAddr;
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};

use async_trait::async_trait;
use digest::generic_array::GenericArray;
use digest::Digest;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use tokio::process::Command;
use tracing::instrument;

use super::{FileSystem, MountType, ReadOnly};
use crate::disk::mount::guard::TmpMountGuard;
use crate::util::Invoke;
use crate::Error;

async fn resolve_hostname(hostname: &str) -> Result<IpAddr, Error> {
    #[cfg(feature = "avahi")]
    if hostname.ends_with(".local") {
        return Ok(crate::net::mdns::resolve_mdns(hostname).await?);
    }
    Ok(String::from_utf8(
        Command::new("nmblookup")
            .arg(hostname)
            .invoke(crate::ErrorKind::Network)
            .await?,
    )?
    .split(" ")
    .next()
    .unwrap()
    .trim()
    .parse()?)
}

#[instrument(skip(path, password, mountpoint))]
pub async fn mount_cifs(
    hostname: &str,
    path: impl AsRef<Path>,
    username: &str,
    password: Option<&str>,
    mountpoint: impl AsRef<Path>,
    mount_type: MountType,
) -> Result<(), Error> {
    tokio::fs::create_dir_all(mountpoint.as_ref()).await?;
    let ip: IpAddr = resolve_hostname(hostname).await?;
    let absolute_path = Path::new("/").join(path.as_ref());
    let mut cmd = Command::new("mount");
    cmd.arg("-t")
        .arg("cifs")
        .env("USER", username)
        .env("PASSWD", password.unwrap_or_default())
        .arg(format!("//{}{}", ip, absolute_path.display()))
        .arg(mountpoint.as_ref());
    if mount_type == ReadOnly {
        cmd.arg("-o").arg("ro,noserverino");
    } else {
        cmd.arg("-o").arg("noserverino");
    }
    cmd.invoke(crate::ErrorKind::Filesystem).await?;
    Ok(())
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Cifs {
    pub hostname: String,
    pub path: PathBuf,
    pub username: String,
    pub password: Option<String>,
}
impl Cifs {
    pub async fn mountable(&self) -> Result<(), Error> {
        let guard = TmpMountGuard::mount(self, ReadOnly).await?;
        guard.unmount().await?;
        Ok(())
    }
}
#[async_trait]
impl FileSystem for Cifs {
    async fn mount<P: AsRef<std::path::Path> + Send + Sync>(
        &self,
        mountpoint: P,
        mount_type: MountType,
    ) -> Result<(), Error> {
        mount_cifs(
            &self.hostname,
            &self.path,
            &self.username,
            self.password.as_ref().map(|p| p.as_str()),
            mountpoint,
            mount_type,
        )
        .await
    }
    async fn source_hash(&self) -> Result<GenericArray<u8, <Sha256 as Digest>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("Cifs");
        sha.update(self.hostname.as_bytes());
        sha.update(self.path.as_os_str().as_bytes());
        Ok(sha.finalize())
    }
}
