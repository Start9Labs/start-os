use std::os::unix::ffi::OsStrExt;
use std::path::Path;

use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use ts_rs::TS;

use super::FileSystem;
use crate::prelude::*;
use crate::util::io::create_file;

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "kebab-case")]
pub enum FileType {
    File,
    Directory,
    Infer,
}

pub struct Bind<Src: AsRef<Path>> {
    src: Src,
    filetype: FileType,
}
impl<Src: AsRef<Path>> Bind<Src> {
    pub fn new(src: Src) -> Self {
        Self {
            src,
            filetype: FileType::Directory,
        }
    }
    pub fn with_type(mut self, filetype: FileType) -> Self {
        self.filetype = filetype;
        self
    }
}
impl<Src: AsRef<Path> + Send + Sync> FileSystem for Bind<Src> {
    async fn source(&self) -> Result<Option<impl AsRef<Path>>, Error> {
        Ok(Some(&self.src))
    }
    fn extra_args(&self) -> impl IntoIterator<Item = impl AsRef<std::ffi::OsStr>> {
        ["--bind"]
    }
    async fn pre_mount(&self, mountpoint: &Path) -> Result<(), Error> {
        let from_meta = tokio::fs::metadata(&self.src).await.ok();
        let to_meta = tokio::fs::metadata(&mountpoint).await.ok();
        if matches!(self.filetype, FileType::File)
            || (matches!(self.filetype, FileType::Infer)
                && from_meta.as_ref().map_or(false, |m| m.is_file()))
        {
            if to_meta.as_ref().map_or(false, |m| m.is_dir()) {
                tokio::fs::remove_dir(mountpoint).await?;
            }
            if from_meta.is_none() {
                create_file(self.src.as_ref()).await?.sync_all().await?;
            }
            if to_meta.is_none() {
                create_file(mountpoint).await?.sync_all().await?;
            }
        } else {
            if to_meta.as_ref().map_or(false, |m| m.is_file()) {
                tokio::fs::remove_file(mountpoint).await?;
            }
            if from_meta.is_none() {
                tokio::fs::create_dir_all(self.src.as_ref()).await?;
            }
            if to_meta.is_none() {
                tokio::fs::create_dir_all(mountpoint).await?;
            }
        }
        Ok(())
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("Bind");
        sha.update(
            tokio::fs::canonicalize(self.src.as_ref())
                .await
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        self.src.as_ref().display().to_string(),
                    )
                })?
                .as_os_str()
                .as_bytes(),
        );
        Ok(sha.finalize())
    }
}
