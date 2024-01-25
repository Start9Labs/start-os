use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Weak};

use lazy_static::lazy_static;
use models::ResultExt;
use tokio::sync::Mutex;
use tracing::instrument;

use super::filesystem::{FileSystem, MountType, ReadOnly, ReadWrite};
use super::util::unmount;
use crate::util::{Invoke, Never};
use crate::Error;

pub const TMP_MOUNTPOINT: &'static str = "/media/embassy/tmp";

#[async_trait::async_trait]
pub trait GenericMountGuard: std::fmt::Debug + Send + Sync + 'static {
    fn path(&self) -> &Path;
    async fn unmount(mut self) -> Result<(), Error>;
}

#[async_trait::async_trait]
impl GenericMountGuard for Never {
    fn path(&self) -> &Path {
        match *self {}
    }
    async fn unmount(mut self) -> Result<(), Error> {
        match self {}
    }
}

#[async_trait::async_trait]
impl<T> GenericMountGuard for Arc<T>
where
    T: GenericMountGuard,
{
    fn path(&self) -> &Path {
        (&**self).path()
    }
    async fn unmount(mut self) -> Result<(), Error> {
        if let Ok(guard) = Arc::try_unwrap(self) {
            guard.unmount().await?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct MountGuard {
    mountpoint: PathBuf,
    mounted: bool,
}
impl MountGuard {
    pub async fn mount(
        filesystem: &impl FileSystem,
        mountpoint: impl AsRef<Path>,
        mount_type: MountType,
    ) -> Result<Self, Error> {
        let mountpoint = mountpoint.as_ref().to_owned();
        filesystem.mount(&mountpoint, mount_type).await?;
        Ok(MountGuard {
            mountpoint,
            mounted: true,
        })
    }
    pub(super) fn take(&mut self) -> Self {
        Self {
            mountpoint: self.mountpoint.clone(),
            mounted: std::mem::replace(&mut self.mounted, false),
        }
    }
    pub async fn unmount(mut self, delete_mountpoint: bool) -> Result<(), Error> {
        if self.mounted {
            unmount(&self.mountpoint).await?;
            if delete_mountpoint {
                match tokio::fs::remove_dir(&self.mountpoint).await {
                    Err(e) if e.raw_os_error() == Some(39) => Ok(()), // directory not empty
                    a => a,
                }
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        format!("rm {}", self.mountpoint.display()),
                    )
                })?;
            }
            self.mounted = false;
        }
        Ok(())
    }
}
impl Drop for MountGuard {
    fn drop(&mut self) {
        if self.mounted {
            let mountpoint = std::mem::take(&mut self.mountpoint);
            tokio::spawn(async move { unmount(mountpoint).await.unwrap() });
        }
    }
}
#[async_trait::async_trait]
impl GenericMountGuard for MountGuard {
    fn path(&self) -> &Path {
        &self.mountpoint
    }
    async fn unmount(mut self) -> Result<(), Error> {
        MountGuard::unmount(self, false).await
    }
}

async fn tmp_mountpoint(source: &impl FileSystem) -> Result<PathBuf, Error> {
    Ok(Path::new(TMP_MOUNTPOINT).join(base32::encode(
        base32::Alphabet::RFC4648 { padding: false },
        &source.source_hash().await?,
    )))
}

lazy_static! {
    static ref TMP_MOUNTS: Mutex<BTreeMap<PathBuf, (MountType, Weak<MountGuard>)>> =
        Mutex::new(BTreeMap::new());
}

#[derive(Debug, Clone)]
pub struct TmpMountGuard {
    guard: Arc<MountGuard>,
}
impl TmpMountGuard {
    /// DRAGONS: if you try to mount something as ro and rw at the same time, the ro mount will be upgraded to rw.
    #[instrument(skip_all)]
    pub async fn mount(filesystem: &impl FileSystem, mount_type: MountType) -> Result<Self, Error> {
        let mountpoint = tmp_mountpoint(filesystem).await?;
        let mut tmp_mounts = TMP_MOUNTS.lock().await;
        if !tmp_mounts.contains_key(&mountpoint) {
            tmp_mounts.insert(mountpoint.clone(), (mount_type, Weak::new()));
        }
        let (prev_mt, weak_slot) = tmp_mounts.get_mut(&mountpoint).unwrap();
        if let Some(guard) = weak_slot.upgrade() {
            // upgrade to rw
            if *prev_mt == ReadOnly && mount_type == ReadWrite {
                tokio::process::Command::new("mount")
                    .arg("-o")
                    .arg("remount,rw")
                    .arg(&mountpoint)
                    .invoke(crate::ErrorKind::Filesystem)
                    .await?;
                *prev_mt = ReadWrite;
            }
            Ok(TmpMountGuard { guard })
        } else {
            let guard = Arc::new(MountGuard::mount(filesystem, &mountpoint, mount_type).await?);
            *weak_slot = Arc::downgrade(&guard);
            *prev_mt = mount_type;
            Ok(TmpMountGuard { guard })
        }
    }
}
#[async_trait::async_trait]
impl GenericMountGuard for TmpMountGuard {
    fn path(&self) -> &Path {
        self.guard.path()
    }
    async fn unmount(mut self) -> Result<(), Error> {
        self.guard.unmount().await
    }
}

#[derive(Debug)]
pub struct SubPath<G: GenericMountGuard> {
    guard: G,
    path: PathBuf,
}
impl<G: GenericMountGuard> SubPath<G> {
    pub fn new(guard: G, path: impl AsRef<Path>) -> Self {
        let path = path.as_ref();
        let path = guard.path().join(path.strip_prefix("/").unwrap_or(path));
        Self { guard, path }
    }
}
#[async_trait::async_trait]
impl<G: GenericMountGuard> GenericMountGuard for SubPath<G> {
    fn path(&self) -> &Path {
        self.path.as_path()
    }
    async fn unmount(mut self) -> Result<(), Error> {
        self.guard.unmount().await
    }
}
