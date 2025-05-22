use std::fmt::Display;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;

use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use sha2::Sha256;

use crate::disk::mount::filesystem::{FileSystem, ReadWrite};
use crate::disk::mount::guard::{GenericMountGuard, MountGuard};
use crate::prelude::*;
use crate::util::io::TmpDir;

pub struct OverlayFs<P0: AsRef<Path>, P1: AsRef<Path>, P2: AsRef<Path>> {
    lower: P0,
    upper: P1,
    work: P2,
}
impl<P0: AsRef<Path>, P1: AsRef<Path>, P2: AsRef<Path>> OverlayFs<P0, P1, P2> {
    pub fn new(lower: P0, upper: P1, work: P2) -> Self {
        Self { lower, upper, work }
    }
}
impl<
        P0: AsRef<Path> + Send + Sync,
        P1: AsRef<Path> + Send + Sync,
        P2: AsRef<Path> + Send + Sync,
    > FileSystem for OverlayFs<P0, P1, P2>
{
    fn mount_type(&self) -> Option<impl AsRef<str>> {
        Some("overlay")
    }
    async fn source(&self) -> Result<Option<impl AsRef<Path>>, Error> {
        Ok(Some("overlay"))
    }
    fn mount_options(&self) -> impl IntoIterator<Item = impl Display> {
        [
            Box::new(lazy_format!("lowerdir={}", self.lower.as_ref().display()))
                as Box<dyn Display>,
            Box::new(lazy_format!("upperdir={}", self.upper.as_ref().display())),
            Box::new(lazy_format!("workdir={}", self.work.as_ref().display())),
        ]
    }
    async fn pre_mount(&self, mountpoint: &Path) -> Result<(), Error> {
        tokio::fs::create_dir_all(self.upper.as_ref()).await?;
        tokio::fs::create_dir_all(self.work.as_ref()).await?;
        tokio::fs::create_dir_all(mountpoint).await?;
        Ok(())
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        tokio::fs::create_dir_all(self.upper.as_ref()).await?;
        tokio::fs::create_dir_all(self.work.as_ref()).await?;
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
        sha.update(
            tokio::fs::canonicalize(self.work.as_ref())
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
pub struct OverlayGuard<G: GenericMountGuard> {
    lower: Option<G>,
    upper: Option<TmpDir>,
    inner_guard: MountGuard,
}
impl<G: GenericMountGuard> OverlayGuard<G> {
    pub async fn mount(lower: G, mountpoint: impl AsRef<Path>) -> Result<Self, Error> {
        let upper = TmpDir::new().await?;
        let inner_guard = MountGuard::mount(
            &OverlayFs::new(
                lower.path(),
                upper.as_ref().join("upper"),
                upper.as_ref().join("work"),
            ),
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
    pub fn take(&mut self) -> Self {
        Self {
            lower: self.lower.take(),
            upper: self.upper.take(),
            inner_guard: self.inner_guard.take(),
        }
    }
}
impl<G: GenericMountGuard> GenericMountGuard for OverlayGuard<G> {
    fn path(&self) -> &Path {
        self.inner_guard.path()
    }
    async fn unmount(self) -> Result<(), Error> {
        self.unmount(false).await
    }
}
impl<G: GenericMountGuard> Drop for OverlayGuard<G> {
    fn drop(&mut self) {
        let lower = self.lower.take();
        let upper = self.upper.take();
        let guard = self.inner_guard.take();
        if lower.is_some() || upper.is_some() || guard.mounted {
            tokio::spawn(async move {
                guard.unmount(false).await.log_err();
                if let Some(lower) = lower {
                    lower.unmount().await.log_err();
                }
                if let Some(upper) = upper {
                    upper.delete().await.log_err();
                }
            });
        }
    }
}
