use std::path::Path;

use async_trait::async_trait;
use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use reqwest::Url;
use serde::{Deserialize, Serialize};
use sha2::Sha256;

use super::{FileSystem, MountType};
use crate::util::Invoke;
use crate::Error;

pub async fn mount_httpdirfs(url: &Url, mountpoint: impl AsRef<Path>) -> Result<(), Error> {
    tokio::fs::create_dir_all(mountpoint.as_ref()).await?;
    let mut cmd = tokio::process::Command::new("httpdirfs");
    cmd.arg("--cache")
        .arg("--single-file-mode")
        .arg(url.as_str())
        .arg(mountpoint.as_ref());
    cmd.invoke(crate::ErrorKind::Filesystem).await?;
    Ok(())
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct HttpDirFS {
    url: Url,
}
impl HttpDirFS {
    pub fn new(url: Url) -> Self {
        HttpDirFS { url }
    }
}
#[async_trait]
impl FileSystem for HttpDirFS {
    async fn mount<P: AsRef<Path> + Send + Sync>(
        &self,
        mountpoint: P,
        _mount_type: MountType,
    ) -> Result<(), Error> {
        mount_httpdirfs(&self.url, mountpoint).await
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("HttpDirFS");
        sha.update(self.url.as_str());
        Ok(sha.finalize())
    }
}
