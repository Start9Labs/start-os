use std::os::unix::ffi::OsStrExt;
use std::path::Path;

use async_trait::async_trait;
use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use sha2::Sha256;

use super::{FileSystem, MountType, ReadOnly};
use crate::prelude::*;
use crate::util::Invoke;

pub struct EfiVarFs;
#[async_trait]
impl FileSystem for EfiVarFs {
    async fn mount<P: AsRef<Path> + Send + Sync>(
        &self,
        mountpoint: P,
        mount_type: MountType,
    ) -> Result<(), Error> {
        tokio::fs::create_dir_all(mountpoint.as_ref()).await?;
        let mut cmd = tokio::process::Command::new("mount");
        cmd.arg("-t")
            .arg("efivarfs")
            .arg("efivarfs")
            .arg(mountpoint.as_ref());
        if mount_type == ReadOnly {
            cmd.arg("-o").arg("ro");
        }
        cmd.invoke(ErrorKind::Filesystem).await?;
        Ok(())
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("EfiVarFs");
        Ok(sha.finalize())
    }
}
