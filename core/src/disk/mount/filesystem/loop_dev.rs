use std::os::fd::AsFd;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;

use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use serde::{Deserialize, Serialize};
use sha2::Sha256;

use super::FileSystem;
use crate::disk::mount::filesystem::MountType;
use crate::disk::mount::filesystem::syscall::{
    DetachedMount, fsconfig_create, fsconfig_set_string, fsmount, fsopen, loop_attach,
};
use crate::prelude::*;

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct LoopDev<LogicalName: AsRef<Path>> {
    logicalname: LogicalName,
    offset: u64,
    size: u64,
    #[serde(default, skip)]
    fs_type: Option<&'static str>,
}
impl<LogicalName: AsRef<Path>> LoopDev<LogicalName> {
    pub fn new(logicalname: LogicalName, offset: u64, size: u64) -> Self {
        Self {
            logicalname,
            offset,
            size,
            fs_type: None,
        }
    }
    /// Opt into the syscall path. Without a type the mount goes through
    /// `mount(8)`'s `-o loop,offset=,sizelimit=` for autodetect.
    pub fn with_type(mut self, ty: &'static str) -> Self {
        self.fs_type = Some(ty);
        self
    }
}
impl<LogicalName: AsRef<Path> + Send + Sync> FileSystem for LoopDev<LogicalName> {
    async fn mount<P: AsRef<Path> + Send>(
        &self,
        mountpoint: P,
        mount_type: MountType,
    ) -> Result<(), Error> {
        let mp = mountpoint.as_ref();
        let Some(ty) = self.fs_type else {
            // Fallback: let mount(8) drive losetup + autodetect.
            let opts = [
                "loop".to_owned(),
                format!("offset={}", self.offset),
                format!("sizelimit={}", self.size),
            ];
            return super::mount_via_cli(
                None,
                [] as [&str; 0],
                opts,
                Some(self.logicalname.as_ref()),
                mp,
                mount_type,
            )
            .await;
        };
        tokio::fs::create_dir_all(mp).await?;

        let read_only = matches!(mount_type, MountType::ReadOnly);
        let (loop_fd, loop_path) =
            loop_attach(self.logicalname.as_ref(), self.offset, self.size, read_only).await?;

        let fs = fsopen(ty)?;
        fsconfig_set_string(fs.as_fd(), "source", &loop_path.display().to_string())?;
        fsconfig_create(fs.as_fd())?;
        let detached = DetachedMount::from_fd(fsmount(fs.as_fd(), 0)?);
        if read_only {
            detached.set_readonly(true)?;
        }
        detached.attach(mp)?;
        // The kernel keeps the loop dev alive as long as the filesystem
        // built on it is mounted; LO_FLAGS_AUTOCLEAR will tear it down
        // when the last opener (this fd + the mount) goes away.
        drop(loop_fd);
        Ok(())
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("LoopDev");
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
