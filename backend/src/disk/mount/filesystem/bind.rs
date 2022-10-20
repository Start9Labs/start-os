use std::os::unix::ffi::OsStrExt;
use std::path::Path;

use async_trait::async_trait;
use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use sha2::Sha256;

use super::{FileSystem, MountType, ReadOnly};
use crate::disk::mount::util::bind;
use crate::{Error, ResultExt};

pub struct Bind<SrcDir: AsRef<Path>> {
    src_dir: SrcDir,
}
impl<SrcDir: AsRef<Path>> Bind<SrcDir> {
    pub fn new(src_dir: SrcDir) -> Self {
        Self { src_dir }
    }
}
#[async_trait]
impl<SrcDir: AsRef<Path> + Send + Sync> FileSystem for Bind<SrcDir> {
    async fn mount<P: AsRef<Path> + Send + Sync>(
        &self,
        mountpoint: P,
        mount_type: MountType,
    ) -> Result<(), Error> {
        bind(
            self.src_dir.as_ref(),
            mountpoint,
            matches!(mount_type, ReadOnly),
        )
        .await
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("Bind");
        sha.update(
            tokio::fs::canonicalize(self.src_dir.as_ref())
                .await
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        self.src_dir.as_ref().display().to_string(),
                    )
                })?
                .as_os_str()
                .as_bytes(),
        );
        Ok(sha.finalize())
    }
}
