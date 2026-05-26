use std::path::Path;

use tracing::instrument;

use crate::Error;
use crate::disk::mount::filesystem::syscall;
use crate::prelude::*;

pub use syscall::is_mountpoint;

#[instrument(skip_all)]
pub async fn bind<P0: AsRef<Path>, P1: AsRef<Path>>(
    src: P0,
    dst: P1,
    read_only: bool,
) -> Result<(), Error> {
    tracing::info!(
        "{}",
        t!(
            "disk.mount.binding",
            src = src.as_ref().display(),
            dst = dst.as_ref().display()
        )
    );
    if is_mountpoint(&dst).await? {
        unmount(dst.as_ref(), true).await?;
    }
    tokio::fs::create_dir_all(&src).await?;
    tokio::fs::create_dir_all(&dst).await?;

    let fd = syscall::open_tree_clone(src.as_ref(), false)?;
    let detached = syscall::DetachedMount::from_fd(fd);
    if read_only {
        detached.set_readonly(true)?;
    }
    detached.attach(dst.as_ref())?;
    Ok(())
}

/// Flush the filesystem containing `path` via `syncfs(2)`.
///
/// Use this before lazy-unmounting a layered fs whose front layer flushes
/// dirty data to its backing on `syncfs` (e.g. overlayfs). FUSE filesystems
/// that don't implement `FUSE_SYNCFS` won't see this signal — use
/// [`sync_directory`] for those.
#[instrument(skip_all)]
pub async fn sync_filesystem<P: AsRef<Path>>(path: P) -> Result<(), Error> {
    syscall::syncfs_at(path.as_ref()).await
}

/// Flush the directory at `path` via `fsync(2)`.
///
/// For FUSE-based filesystems (e.g. backup-fs) this routes to
/// `FUSE_FSYNCDIR`, which the daemon implements to drain its dirty cache —
/// `syncfs` is not enough because FUSE doesn't propagate it.
#[instrument(skip_all)]
pub async fn sync_directory<P: AsRef<Path>>(path: P) -> Result<(), Error> {
    let p = path.as_ref().to_owned();
    tokio::task::spawn_blocking(move || -> Result<(), Error> {
        let f = std::fs::File::open(&p).map_err(|e| Error::new(e, ErrorKind::Filesystem))?;
        f.sync_all().map_err(|e| Error::new(e, ErrorKind::Filesystem))
    })
    .await
    .with_kind(ErrorKind::Cancelled)?
}

#[instrument(skip_all)]
pub async fn unmount<P: AsRef<Path>>(mountpoint: P, lazy: bool) -> Result<(), Error> {
    tracing::debug!("Unmounting {}.", mountpoint.as_ref().display());
    syscall::umount2(mountpoint, lazy).await
}

/// Unmounts all mountpoints under (and including) the given path, in reverse
/// depth order so that nested mounts are unmounted before their parents.
#[instrument(skip_all)]
pub async fn unmount_all_under<P: AsRef<Path>>(path: P, lazy: bool) -> Result<(), Error> {
    let path = path.as_ref();
    let canonical_path = tokio::fs::canonicalize(path)
        .await
        .with_ctx(|_| (ErrorKind::Filesystem, lazy_format!("canonicalize {path:?}")))?;

    let mounts_content = tokio::fs::read_to_string("/proc/mounts")
        .await
        .with_ctx(|_| (ErrorKind::Filesystem, "read /proc/mounts"))?;

    let mut mountpoints: Vec<&str> = mounts_content
        .lines()
        .filter_map(|line| {
            let mountpoint = line.split_whitespace().nth(1)?;
            let mp_path = Path::new(mountpoint);
            if mp_path.starts_with(&canonical_path) {
                Some(mountpoint)
            } else {
                None
            }
        })
        .collect();

    mountpoints.sort_by(|a, b| b.len().cmp(&a.len()));

    for mountpoint in mountpoints {
        tracing::debug!("Unmounting nested mountpoint: {}", mountpoint);
        unmount(mountpoint, lazy).await?;
    }

    Ok(())
}
