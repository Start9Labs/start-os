use std::os::unix::ffi::OsStrExt;
use std::path::Path;

use async_trait::async_trait;
use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use serde::{Deserialize, Serialize};
use sha2::Sha256;

use super::{FileSystem, MountType, ReadOnly};
use crate::util::Invoke;
use crate::{Error, ResultExt};

pub async fn mount(
    logicalname: impl AsRef<Path>,
    offset: u64,
    mountpoint: impl AsRef<Path>,
    mount_type: MountType,
) -> Result<(), Error> {
    tokio::fs::create_dir_all(mountpoint.as_ref()).await?;
    let mut cmd = tokio::process::Command::new("mount");
    cmd.arg(logicalname.as_ref())
        .arg(mountpoint.as_ref())
        .arg("-o");
    if mount_type == ReadOnly {
        cmd.arg(format!("loop,offset={offset},ro"));
    } else {
        cmd.arg(format!("loop,offset={offset}"));
    }
    cmd.invoke(crate::ErrorKind::Filesystem).await?;
    Ok(())
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Offset<LogicalName: AsRef<Path>> {
    logicalname: LogicalName,
    offset: u64,
}
impl<LogicalName: AsRef<Path>> Offset<LogicalName> {
    pub fn new(logicalname: LogicalName, offset: u64) -> Self {
        Self {
            logicalname,
            offset,
        }
    }
}
#[async_trait]
impl<LogicalName: AsRef<Path> + Send + Sync> FileSystem for Offset<LogicalName> {
    async fn mount<P: AsRef<Path> + Send + Sync>(
        &self,
        mountpoint: P,
        mount_type: MountType,
    ) -> Result<(), Error> {
        mount(
            self.logicalname.as_ref(),
            self.offset,
            mountpoint,
            mount_type,
        )
        .await
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("Offset");
        sha.update(
            tokio::fs::canonicalize(self.logicalname.as_ref())
                .await
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        self.logicalname.as_ref().display().to_string(),
                    )
                })?
                .as_os_str()
                .as_bytes(),
        );
        sha.update(&u64::to_be_bytes(self.offset)[..]);
        Ok(sha.finalize())
    }
}
