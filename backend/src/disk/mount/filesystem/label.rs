use std::path::Path;

use async_trait::async_trait;
use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use sha2::Sha256;

use super::{FileSystem, MountType, ReadOnly};
use crate::util::Invoke;
use crate::Error;

pub async fn mount_label(
    label: &str,
    mountpoint: impl AsRef<Path>,
    mount_type: MountType,
) -> Result<(), Error> {
    tokio::fs::create_dir_all(mountpoint.as_ref()).await?;
    let mut cmd = tokio::process::Command::new("mount");
    cmd.arg("-L").arg(label).arg(mountpoint.as_ref());
    if mount_type == ReadOnly {
        cmd.arg("-o").arg("ro");
    }
    cmd.invoke(crate::ErrorKind::Filesystem).await?;
    Ok(())
}

pub struct Label<S: AsRef<str>> {
    label: S,
}
impl<S: AsRef<str>> Label<S> {
    pub fn new(label: S) -> Self {
        Label { label }
    }
}
#[async_trait]
impl<S: AsRef<str> + Send + Sync> FileSystem for Label<S> {
    async fn mount<P: AsRef<Path> + Send + Sync>(
        &self,
        mountpoint: P,
        mount_type: MountType,
    ) -> Result<(), Error> {
        mount_label(self.label.as_ref(), mountpoint, mount_type).await
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("Label");
        sha.update(self.label.as_ref().as_bytes());
        Ok(sha.finalize())
    }
}
