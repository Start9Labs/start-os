//! Non-Linux stub for the [`syscall`] module.
//!
//! start-os's mount layer is Linux-only — the new mount API, the LXC
//! userns dance, and the `LOOP_CONFIGURE` ioctl don't exist on
//! macOS / *BSD. But `start-cli` is cross-built for macOS so users
//! can drive the daemon from their dev machine, which means the
//! whole `start-os` lib must still compile on non-Linux targets.
//!
//! These stubs provide signature-compatible no-ops: each entry point
//! returns an error so the wrong path can't accidentally succeed at
//! runtime on a non-Linux host. None of them should ever be called
//! from `start-cli`; if they are, that's a bug.

use std::os::fd::{BorrowedFd, OwnedFd};
use std::path::{Path, PathBuf};

use crate::prelude::*;

fn unsupported<T>() -> Result<T, Error> {
    Err(Error::new(
        eyre!("mount syscall API is only available on Linux"),
        ErrorKind::Filesystem,
    ))
}

pub fn fsopen(_fs_type: &str) -> Result<OwnedFd, Error> {
    unsupported()
}
pub fn fsconfig_set_string(
    _fs_fd: BorrowedFd,
    _key: &str,
    _value: &str,
) -> Result<(), Error> {
    unsupported()
}
pub fn fsconfig_set_path(
    _fs_fd: BorrowedFd,
    _key: &str,
    _value: &Path,
) -> Result<(), Error> {
    unsupported()
}
pub fn fsconfig_set_flag(_fs_fd: BorrowedFd, _key: &str) -> Result<(), Error> {
    unsupported()
}
pub fn fsconfig_set_fd(
    _fs_fd: BorrowedFd,
    _key: &str,
    _aux: BorrowedFd,
) -> Result<(), Error> {
    unsupported()
}
pub fn fsconfig_create(_fs_fd: BorrowedFd) -> Result<(), Error> {
    unsupported()
}
pub fn fsmount(_fs_fd: BorrowedFd, _attrs: u64) -> Result<OwnedFd, Error> {
    unsupported()
}
pub fn open_tree_clone(_path: &Path, _recursive: bool) -> Result<OwnedFd, Error> {
    unsupported()
}
pub fn open_tree_attr_idmap(
    _path: &Path,
    _recursive: bool,
    _userns: BorrowedFd,
) -> Result<OwnedFd, Error> {
    unsupported()
}
pub fn move_mount(_detached: BorrowedFd, _to: &Path) -> Result<(), Error> {
    unsupported()
}
pub fn mount_setattr_idmap(
    _fd: BorrowedFd,
    _userns: BorrowedFd,
    _recursive: bool,
) -> Result<(), Error> {
    unsupported()
}
pub fn mount_setattr_ro(_fd: BorrowedFd, _ro: bool) -> Result<(), Error> {
    unsupported()
}

/// Stub `DetachedMount` — owns nothing on non-Linux; every method errors.
pub struct DetachedMount;
impl DetachedMount {
    pub fn from_fd(_fd: OwnedFd) -> Self {
        Self
    }
    pub fn as_fd(&self) -> BorrowedFd<'_> {
        // No real fd to borrow on non-Linux. Anything calling this is a
        // runtime bug; a panic here flags it clearly.
        panic!("DetachedMount::as_fd called on non-Linux")
    }
    pub fn set_idmap(&self, _userns: BorrowedFd, _recursive: bool) -> Result<(), Error> {
        unsupported()
    }
    pub fn set_readonly(&self, _ro: bool) -> Result<(), Error> {
        unsupported()
    }
    pub fn attach(self, _mountpoint: &Path) -> Result<(), Error> {
        unsupported()
    }
}

pub async fn userns_fd_from_idmap(
    _idmap: &[crate::disk::mount::filesystem::idmapped::IdMap],
) -> Result<OwnedFd, Error> {
    unsupported()
}

pub fn unshare_userns_main() -> std::io::Result<()> {
    Err(std::io::Error::new(
        std::io::ErrorKind::Unsupported,
        "unshare-userns is Linux-only",
    ))
}

pub async fn loop_attach(
    _backing: impl AsRef<Path>,
    _offset: u64,
    _sizelimit: u64,
    _read_only: bool,
) -> Result<(OwnedFd, PathBuf), Error> {
    unsupported()
}

pub async fn umount2(_target: impl AsRef<Path>, _lazy: bool) -> Result<(), Error> {
    unsupported()
}

pub async fn syncfs_at(_path: impl AsRef<Path>) -> Result<(), Error> {
    unsupported()
}

pub async fn is_mountpoint(_path: impl AsRef<Path>) -> Result<bool, Error> {
    unsupported()
}

pub async fn remount_rw(_mountpoint: impl AsRef<Path>) -> Result<(), Error> {
    unsupported()
}

pub async fn make_rshared(_mountpoint: impl AsRef<Path>) -> Result<(), Error> {
    unsupported()
}
