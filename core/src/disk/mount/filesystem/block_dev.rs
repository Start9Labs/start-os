use std::os::fd::AsFd;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;

use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use ts_rs::TS;

use super::FileSystem;
use crate::disk::mount::filesystem::MountType;
use crate::disk::mount::filesystem::syscall::{
    DetachedMount, fsconfig_create, fsconfig_set_string, fsmount, fsopen,
};
use crate::prelude::*;

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(concrete(LogicalName = std::path::PathBuf))]
pub struct BlockDev<LogicalName: AsRef<Path>> {
    logicalname: LogicalName,
    #[serde(default, skip)]
    #[ts(skip)]
    fs_type: Option<&'static str>,
}
impl<LogicalName: AsRef<Path>> BlockDev<LogicalName> {
    pub fn new(logicalname: LogicalName) -> Self {
        BlockDev {
            logicalname,
            fs_type: None,
        }
    }
    /// Opt into the syscall-based mount path by declaring the filesystem
    /// type. `mount(2)` requires an explicit type — only callers that
    /// genuinely need autodetection (e.g. unknown-disk probing) should
    /// leave `fs_type = None`, which falls back to `mount(8)` for the
    /// type-sniffing it does for us.
    pub fn with_type(mut self, ty: &'static str) -> Self {
        self.fs_type = Some(ty);
        self
    }
}
impl<LogicalName: AsRef<Path> + Send + Sync> FileSystem for BlockDev<LogicalName> {
    async fn mount<P: AsRef<Path> + Send>(
        &self,
        mountpoint: P,
        mount_type: MountType,
    ) -> Result<(), Error> {
        let mp = mountpoint.as_ref();
        let Some(ty) = self.fs_type else {
            return super::mount_via_cli(
                None,
                [] as [&str; 0],
                [] as [&str; 0],
                Some(self.logicalname.as_ref()),
                mp,
                mount_type,
            )
            .await;
        };
        tokio::fs::create_dir_all(mp).await?;
        let fs = fsopen(ty)?;
        fsconfig_set_string(
            fs.as_fd(),
            "source",
            &self.logicalname.as_ref().display().to_string(),
        )?;
        fsconfig_create(fs.as_fd())?;
        let detached = DetachedMount::from_fd(fsmount(fs.as_fd(), 0)?);
        if matches!(mount_type, MountType::ReadOnly) {
            detached.set_readonly(true)?;
        }
        detached.attach(mp)?;
        Ok(())
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("BlockDev");
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
        Ok(sha.finalize())
    }
}
