use std::ffi::OsStr;
use std::fmt::Display;
use std::path::Path;

use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use tokio::process::Command;

use super::{FileSystem, MountType};
use crate::disk::mount::filesystem::default_mount_command;
use crate::prelude::*;
use crate::util::Invoke;

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct IdMapped<Fs: FileSystem> {
    filesystem: Fs,
    from_id: u32,
    to_id: u32,
    range: u32,
}
impl<Fs: FileSystem> IdMapped<Fs> {
    pub fn new(filesystem: Fs, from_id: u32, to_id: u32, range: u32) -> Self {
        Self {
            filesystem,
            from_id,
            to_id,
            range,
        }
    }
}
impl<Fs: FileSystem> FileSystem for IdMapped<Fs> {
    fn mount_type(&self) -> Option<impl AsRef<str>> {
        self.filesystem.mount_type()
    }
    fn extra_args(&self) -> impl IntoIterator<Item = impl AsRef<OsStr>> {
        self.filesystem.extra_args()
    }
    fn mount_options(&self) -> impl IntoIterator<Item = impl Display> {
        self.filesystem
            .mount_options()
            .into_iter()
            .map(|a| Box::new(a) as Box<dyn Display>)
            .chain(std::iter::once(Box::new(lazy_format!(
                "X-mount.idmapped=b:{}:{}:{}",
                self.from_id,
                self.to_id,
                self.range,
            )) as Box<dyn Display>))
    }
    fn source(&self) -> Option<impl AsRef<Path>> {
        self.filesystem.source()
    }
    async fn pre_mount(&self) -> Result<(), Error> {
        self.filesystem.pre_mount().await
    }
    async fn mount<P: AsRef<Path> + Send>(
        &self,
        mountpoint: P,
        mount_type: MountType,
    ) -> Result<(), Error> {
        self.pre_mount().await?;
        tokio::fs::create_dir_all(mountpoint.as_ref()).await?;
        Command::new("mount.next")
            .args(default_mount_command(self, mountpoint, mount_type).get_args())
            .invoke(ErrorKind::Filesystem)
            .await?;

        Ok(())
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("IdMapped");
        sha.update(self.filesystem.source_hash().await?);
        sha.update(u32::to_be_bytes(self.from_id));
        sha.update(u32::to_be_bytes(self.to_id));
        sha.update(u32::to_be_bytes(self.range));
        Ok(sha.finalize())
    }
}
