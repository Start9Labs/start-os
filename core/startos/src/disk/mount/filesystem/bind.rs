use std::os::unix::ffi::OsStrExt;
use std::path::Path;

use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use sha2::Sha256;

use super::FileSystem;
use crate::prelude::*;

pub struct Bind<SrcDir: AsRef<Path>> {
    src_dir: SrcDir,
}
impl<SrcDir: AsRef<Path>> Bind<SrcDir> {
    pub fn new(src_dir: SrcDir) -> Self {
        Self { src_dir }
    }
}
impl<SrcDir: AsRef<Path> + Send + Sync> FileSystem for Bind<SrcDir> {
    async fn source(&self) -> Result<Option<impl AsRef<Path>>, Error> {
        Ok(Some(&self.src_dir))
    }
    fn extra_args(&self) -> impl IntoIterator<Item = impl AsRef<std::ffi::OsStr>> {
        ["--bind"]
    }
    async fn pre_mount(&self) -> Result<(), Error> {
        tokio::fs::create_dir_all(self.src_dir.as_ref()).await?;
        Ok(())
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
