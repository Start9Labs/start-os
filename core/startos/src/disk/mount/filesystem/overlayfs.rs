use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};

use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use serde::de;
use sha2::Sha256;
use tokio::process::Command;

use crate::disk::mount::filesystem::{FileSystem, MountType, ReadOnly, ReadWrite};
use crate::disk::mount::guard::{self, GenericMountGuard, MountGuard, TmpMountGuard};
use crate::disk::mount::util::unmount;
use crate::prelude::*;
use crate::util::io::TmpDir;
use crate::util::Invoke;

pub async fn mount(
    lower: impl AsRef<Path>,
    upper: impl AsRef<Path>,
    mountpoint: impl AsRef<Path>,
    mount_type: MountType,
) -> Result<(), Error> {
    tokio::fs::create_dir_all(mountpoint.as_ref()).await?;
    let work = upper.as_ref().join("work");
    tokio::fs::create_dir_all(&work).await?;
    let upper = upper.as_ref().join("upper");
    tokio::fs::create_dir_all(&upper).await?;
    let mut cmd = Command::new("mount");
    if mount_type == ReadOnly {
        cmd.arg("-o").arg("ro");
    }
    cmd.arg("-t")
        .arg("overlay")
        .arg(format!(
            "-olowerdir={},upperdir={},workdir={}",
            lower.as_ref().display(),
            upper.display(),
            work.display()
        ))
        .arg("overlay")
        .arg(mountpoint.as_ref())
        .invoke(ErrorKind::Filesystem)
        .await?;
    Ok(())
}

struct OverlayFs<P0: AsRef<Path>, P1: AsRef<Path>> {
    lower: P0,
    upper: P1,
}
impl<P0: AsRef<Path>, P1: AsRef<Path>> OverlayFs<P0, P1> {
    pub fn new(lower: P0, upper: P1) -> Self {
        Self { lower, upper }
    }
}
#[async_trait::async_trait]
impl<P0: AsRef<Path> + Send + Sync, P1: AsRef<Path> + Send + Sync> FileSystem
    for OverlayFs<P0, P1>
{
    async fn mount<P: AsRef<Path> + Send + Sync>(
        &self,
        mountpoint: P,
        mount_type: MountType,
    ) -> Result<(), Error> {
        mount(
            self.lower.as_ref(),
            self.upper.as_ref(),
            mountpoint,
            mount_type,
        )
        .await
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("OverlayFs");
        sha.update(
            tokio::fs::canonicalize(self.lower.as_ref())
                .await
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        self.lower.as_ref().display().to_string(),
                    )
                })?
                .as_os_str()
                .as_bytes(),
        );
        sha.update(
            tokio::fs::canonicalize(self.upper.as_ref())
                .await
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        self.upper.as_ref().display().to_string(),
                    )
                })?
                .as_os_str()
                .as_bytes(),
        );
        Ok(sha.finalize())
    }
}

#[derive(Debug)]
pub struct OverlayGuard {
    lower: Option<TmpMountGuard>,
    upper: Option<TmpDir>,
    inner_guard: MountGuard,
}
impl OverlayGuard {
    pub async fn mount(
        base: &impl FileSystem,
        mountpoint: impl AsRef<Path>,
    ) -> Result<Self, Error> {
        let lower = TmpMountGuard::mount(base, ReadOnly).await?;
        let upper = TmpDir::new().await?;
        let inner_guard = MountGuard::mount(
            &OverlayFs::new(lower.path(), upper.as_ref()),
            mountpoint,
            ReadWrite,
        )
        .await?;
        Ok(Self {
            lower: Some(lower),
            upper: Some(upper),
            inner_guard,
        })
    }
    pub async fn unmount(mut self, delete_mountpoint: bool) -> Result<(), Error> {
        self.inner_guard.take().unmount(delete_mountpoint).await?;
        if let Some(lower) = self.lower.take() {
            lower.unmount().await?;
        }
        if let Some(upper) = self.upper.take() {
            upper.delete().await?;
        }
        Ok(())
    }
}
#[async_trait::async_trait]
impl GenericMountGuard for OverlayGuard {
    fn path(&self) -> &Path {
        self.inner_guard.path()
    }
    async fn unmount(mut self) -> Result<(), Error> {
        self.unmount(false).await
    }
}
impl Drop for OverlayGuard {
    fn drop(&mut self) {
        let lower = self.lower.take();
        let upper = self.upper.take();
        let guard = self.inner_guard.take();
        tokio::spawn(async move {
            guard.unmount(false).await.unwrap();
            if let Some(lower) = lower {
                lower.unmount().await.unwrap();
            }
            if let Some(upper) = upper {
                upper.delete().await.unwrap();
            }
        });
    }
}
