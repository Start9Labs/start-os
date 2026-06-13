use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Weak};

use futures::Future;
use lazy_static::lazy_static;
use tokio::sync::Mutex;
use tracing::instrument;

use super::filesystem::{FileSystem, MountType, ReadOnly, ReadWrite};
use super::util::{is_mountpoint, unmount};
use crate::util::{Invoke, Never};
use crate::{Error, ResultExt};

pub const TMP_MOUNTPOINT: &'static str = "/media/startos/tmp";

pub trait GenericMountGuard: std::fmt::Debug + Send + Sync + 'static {
    fn path(&self) -> &Path;
    fn unmount(self) -> impl Future<Output = Result<(), Error>> + Send;
}

impl GenericMountGuard for Never {
    fn path(&self) -> &Path {
        match *self {}
    }
    async fn unmount(self) -> Result<(), Error> {
        match self {}
    }
}

impl<T> GenericMountGuard for Arc<T>
where
    T: GenericMountGuard,
{
    fn path(&self) -> &Path {
        (&**self).path()
    }
    async fn unmount(self) -> Result<(), Error> {
        if let Ok(guard) = Arc::try_unwrap(self) {
            guard.unmount().await?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct MountGuard {
    mountpoint: PathBuf,
    pub(super) mounted: bool,
}
impl MountGuard {
    pub async fn mount(
        filesystem: &impl FileSystem,
        mountpoint: impl AsRef<Path>,
        mount_type: MountType,
    ) -> Result<Self, Error> {
        let mountpoint = mountpoint.as_ref().to_owned();
        // A prior process lifetime (e.g. a hard kill, where Drop never ran) can
        // leave a stale kernel mount at this target; mounting onto it fails with
        // "already mounted". Reconcile by lazily unmounting first, mirroring the
        // `bind` helper. Within a live process this branch isn't reached for an
        // active mount — `TmpMountGuard` dedups via its `Weak<MountGuard>`.
        if is_mountpoint(&mountpoint).await? {
            unmount(&mountpoint, true).await?;
        }
        filesystem.mount(&mountpoint, mount_type).await?;
        Ok(MountGuard {
            mountpoint,
            mounted: true,
        })
    }
    fn as_unmounted(&self) -> Self {
        Self {
            mountpoint: self.mountpoint.clone(),
            mounted: false,
        }
    }
    pub fn take(&mut self) -> Self {
        let unmounted = self.as_unmounted();
        std::mem::replace(self, unmounted)
    }
    pub async fn unmount(mut self, delete_mountpoint: bool) -> Result<(), Error> {
        if self.mounted {
            unmount(&self.mountpoint, !cfg!(feature = "unstable")).await?;
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
            tokio::spawn(async move { unmount(mountpoint, true).await.log_err() });
        }
    }
}
impl GenericMountGuard for MountGuard {
    fn path(&self) -> &Path {
        &self.mountpoint
    }
    async fn unmount(self) -> Result<(), Error> {
        MountGuard::unmount(self, false).await
    }
}

async fn tmp_mountpoint(source: &impl FileSystem) -> Result<PathBuf, Error> {
    Ok(Path::new(TMP_MOUNTPOINT).join(base32::encode(
        base32::Alphabet::Rfc4648 { padding: false },
        &source.source_hash().await?[0..20],
    )))
}

lazy_static! {
    // Maps each tmp mountpoint to its own lock. The outer map lock is held only
    // while fetching/creating a slot — never across the mount itself.
    static ref TMP_MOUNTS: Mutex<BTreeMap<PathBuf, Arc<Mutex<(MountType, Weak<MountGuard>)>>>> =
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
        // Grab the per-mountpoint slot, then drop the outer map lock before
        // mounting. Holding the global lock across the mount self-deadlocks:
        // `IdMapped::mount` re-enters `TmpMountGuard::mount` to stage its inner
        // filesystem, which needs the same lock.
        let slot = TMP_MOUNTS
            .lock()
            .await
            .entry(mountpoint.clone())
            .or_insert_with(|| Arc::new(Mutex::new((mount_type, Weak::new()))))
            .clone();
        let mut slot = slot.lock().await;
        let (prev_mt, weak_slot) = &mut *slot;
        if let Some(guard) = weak_slot.upgrade() {
            // upgrade to rw
            if *prev_mt == ReadOnly && mount_type != ReadOnly {
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

    pub fn take(&mut self) -> Self {
        let unmounted = Self {
            guard: Arc::new(self.guard.as_unmounted()),
        };
        std::mem::replace(self, unmounted)
    }
}
impl GenericMountGuard for TmpMountGuard {
    fn path(&self) -> &Path {
        self.guard.path()
    }
    async fn unmount(self) -> Result<(), Error> {
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
impl<G: GenericMountGuard> GenericMountGuard for SubPath<G> {
    fn path(&self) -> &Path {
        self.path.as_path()
    }
    async fn unmount(self) -> Result<(), Error> {
        self.guard.unmount().await
    }
}
