use std::net::IpAddr;
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};

use async_trait::async_trait;
use color_eyre::eyre::eyre;
use digest::generic_array::GenericArray;
use digest::Digest;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use tokio::process::Command;
use tracing::instrument;

use super::FileSystem;
use crate::disk::mount::guard::TmpMountGuard;
use crate::net::mdns::resolve_mdns;
use crate::util::Invoke;
use crate::Error;

#[instrument(skip(path, password, mountpoint))]
pub async fn mount_cifs(
    hostname: &str,
    path: impl AsRef<Path>,
    username: &str,
    password: Option<&str>,
    mountpoint: impl AsRef<Path>,
) -> Result<(), Error> {
    tokio::fs::create_dir_all(mountpoint.as_ref()).await?;
    let ip: IpAddr = if hostname.ends_with(".local") {
        resolve_mdns(hostname).await?
    } else {
        String::from_utf8(
            Command::new("nmblookup")
                .arg(hostname)
                .invoke(crate::ErrorKind::Network)
                .await?,
        )?
        .split(" ")
        .next()
        .unwrap()
        .trim()
        .parse()?
    };
    let absolute_path = Path::new("/").join(path.as_ref());
    Command::new("mount")
        .arg("-t")
        .arg("cifs")
        .arg("-o")
        .arg(format!(
            "username={}{}",
            username,
            password
                .map(|p| format!(",password={}", p))
                .unwrap_or_default()
        ))
        .arg(format!("//{}{}", ip, absolute_path.display()))
        .arg(mountpoint.as_ref())
        .invoke(crate::ErrorKind::Filesystem)
        .await?;
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
        let guard = TmpMountGuard::mount(self).await?;
        guard.unmount().await?;
        Ok(())
    }
}
#[async_trait]
impl FileSystem for Cifs {
    async fn mount<P: AsRef<std::path::Path> + Send + Sync>(
        &self,
        mountpoint: P,
    ) -> Result<(), Error> {
        mount_cifs(
            &self.hostname,
            &self.path,
            &self.username,
            self.password.as_ref().map(|p| p.as_str()),
            mountpoint,
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
