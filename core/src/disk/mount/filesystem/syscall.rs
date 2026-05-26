//! Thin wrappers around the Linux new-mount API + a fork-helper for userns
//! fds suitable for `MOUNT_ATTR_IDMAP`.
//!
//! `nix` 0.30 (in this crate's deps) only exposes the classic `mount(2)` /
//! `umount(2)` interfaces, so the new-mount-API syscalls below go through
//! `libc::syscall` directly. The flag and syscall-number constants come from
//! `libc` (>=0.2.156); only the `FSCONFIG_*` command codes are defined
//! locally because libc doesn't ship them yet.
//!
//! Most callers should use this module via [`DetachedMount`], which RAII-owns
//! the detached mount fd and exposes `idmap`/`set_readonly`/`attach` builders.

use std::ffi::{CString, OsString};
use std::io;
use std::os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd, OwnedFd, RawFd};
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::time::Duration;

use libc::{
    AT_EMPTY_PATH, AT_FDCWD, AT_RECURSIVE, MOUNT_ATTR_IDMAP, MOUNT_ATTR_RDONLY,
    MOVE_MOUNT_F_EMPTY_PATH, OPEN_TREE_CLOEXEC, OPEN_TREE_CLONE,
};
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Command;

use crate::prelude::*;

// FSCONFIG_* command codes (linux/mount.h; not yet exported by libc).
const FSCONFIG_SET_FLAG: libc::c_uint = 0;
const FSCONFIG_SET_STRING: libc::c_uint = 1;
const FSCONFIG_SET_PATH: libc::c_uint = 3;
const FSCONFIG_SET_FD: libc::c_uint = 5;
const FSCONFIG_CMD_CREATE: libc::c_uint = 6;

const FSOPEN_CLOEXEC: libc::c_uint = 0x1;
const FSMOUNT_CLOEXEC: libc::c_uint = 0x1;

#[repr(C)]
#[derive(Default)]
struct MountAttr {
    attr_set: u64,
    attr_clr: u64,
    propagation: u64,
    userns_fd: u64,
}

fn cstr_path(p: &Path) -> Result<CString, Error> {
    CString::new(p.as_os_str().as_bytes())
        .with_kind(ErrorKind::Filesystem)
        .with_ctx(|_| (ErrorKind::Filesystem, format!("path contains NUL: {p:?}")))
}

fn cstr_str(s: &str) -> Result<CString, Error> {
    CString::new(s)
        .with_kind(ErrorKind::Filesystem)
        .with_ctx(|_| (ErrorKind::Filesystem, format!("string contains NUL: {s:?}")))
}

fn fd_from_raw(raw: i64, what: &'static str) -> Result<OwnedFd, Error> {
    if raw < 0 {
        Err(Error::new(io::Error::last_os_error(), ErrorKind::Filesystem))
            .with_ctx(|_| (ErrorKind::Filesystem, what))
    } else {
        Ok(unsafe { OwnedFd::from_raw_fd(raw as RawFd) })
    }
}

fn check(ret: i64, what: &'static str) -> Result<(), Error> {
    if ret < 0 {
        Err(Error::new(io::Error::last_os_error(), ErrorKind::Filesystem))
            .with_ctx(|_| (ErrorKind::Filesystem, what))
    } else {
        Ok(())
    }
}

/// `fsopen(fs_type, FSOPEN_CLOEXEC)` — start a new filesystem context.
pub fn fsopen(fs_type: &str) -> Result<OwnedFd, Error> {
    let ty = cstr_str(fs_type)?;
    let r = unsafe { libc::syscall(libc::SYS_fsopen, ty.as_ptr(), FSOPEN_CLOEXEC) };
    fd_from_raw(r, "fsopen")
}

/// `fsconfig(fs_fd, FSCONFIG_SET_STRING, key, value, 0)`.
pub fn fsconfig_set_string(fs_fd: BorrowedFd, key: &str, value: &str) -> Result<(), Error> {
    let k = cstr_str(key)?;
    let v = cstr_str(value)?;
    let r = unsafe {
        libc::syscall(
            libc::SYS_fsconfig,
            fs_fd.as_raw_fd(),
            FSCONFIG_SET_STRING,
            k.as_ptr(),
            v.as_ptr(),
            0,
        )
    };
    check(r, "fsconfig SET_STRING")
}

/// `fsconfig(fs_fd, FSCONFIG_SET_PATH, key, value, AT_FDCWD)`.
pub fn fsconfig_set_path(fs_fd: BorrowedFd, key: &str, value: &Path) -> Result<(), Error> {
    let k = cstr_str(key)?;
    let v = cstr_path(value)?;
    let r = unsafe {
        libc::syscall(
            libc::SYS_fsconfig,
            fs_fd.as_raw_fd(),
            FSCONFIG_SET_PATH,
            k.as_ptr(),
            v.as_ptr(),
            AT_FDCWD,
        )
    };
    check(r, "fsconfig SET_PATH")
}

/// `fsconfig(fs_fd, FSCONFIG_SET_FLAG, key, NULL, 0)`.
pub fn fsconfig_set_flag(fs_fd: BorrowedFd, key: &str) -> Result<(), Error> {
    let k = cstr_str(key)?;
    let r = unsafe {
        libc::syscall(
            libc::SYS_fsconfig,
            fs_fd.as_raw_fd(),
            FSCONFIG_SET_FLAG,
            k.as_ptr(),
            std::ptr::null::<libc::c_void>(),
            0,
        )
    };
    check(r, "fsconfig SET_FLAG")
}

/// `fsconfig(fs_fd, FSCONFIG_SET_FD, key, NULL, aux_fd)`.
pub fn fsconfig_set_fd(fs_fd: BorrowedFd, key: &str, aux: BorrowedFd) -> Result<(), Error> {
    let k = cstr_str(key)?;
    let r = unsafe {
        libc::syscall(
            libc::SYS_fsconfig,
            fs_fd.as_raw_fd(),
            FSCONFIG_SET_FD,
            k.as_ptr(),
            std::ptr::null::<libc::c_void>(),
            aux.as_raw_fd(),
        )
    };
    check(r, "fsconfig SET_FD")
}

/// `fsconfig(fs_fd, FSCONFIG_CMD_CREATE, NULL, NULL, 0)` — finalize the fs.
pub fn fsconfig_create(fs_fd: BorrowedFd) -> Result<(), Error> {
    let r = unsafe {
        libc::syscall(
            libc::SYS_fsconfig,
            fs_fd.as_raw_fd(),
            FSCONFIG_CMD_CREATE,
            std::ptr::null::<libc::c_void>(),
            std::ptr::null::<libc::c_void>(),
            0,
        )
    };
    check(r, "fsconfig CMD_CREATE")
}

/// `fsmount(fs_fd, FSMOUNT_CLOEXEC, attrs)` — produce a detached mount fd.
pub fn fsmount(fs_fd: BorrowedFd, attrs: u64) -> Result<OwnedFd, Error> {
    let r = unsafe { libc::syscall(libc::SYS_fsmount, fs_fd.as_raw_fd(), FSMOUNT_CLOEXEC, attrs) };
    fd_from_raw(r, "fsmount")
}

/// `open_tree(AT_FDCWD, path, OPEN_TREE_CLONE | OPEN_TREE_CLOEXEC | [AT_RECURSIVE])`.
///
/// Produces a detached clone of the mount at `path`, optionally including any
/// submounts when `recursive` is true (the `rbind` semantic).
pub fn open_tree_clone(path: &Path, recursive: bool) -> Result<OwnedFd, Error> {
    let p = cstr_path(path)?;
    let mut flags = OPEN_TREE_CLOEXEC | OPEN_TREE_CLONE;
    if recursive {
        flags |= AT_RECURSIVE as libc::c_uint;
    }
    let r = unsafe { libc::syscall(libc::SYS_open_tree, AT_FDCWD, p.as_ptr(), flags) };
    fd_from_raw(r, "open_tree")
}

/// `move_mount(detached, "", AT_FDCWD, to, MOVE_MOUNT_F_EMPTY_PATH)` — attach
/// a detached mount fd at a path.
pub fn move_mount(detached: BorrowedFd, to: &Path) -> Result<(), Error> {
    let empty = cstr_str("")?;
    let t = cstr_path(to)?;
    let r = unsafe {
        libc::syscall(
            libc::SYS_move_mount,
            detached.as_raw_fd(),
            empty.as_ptr(),
            AT_FDCWD,
            t.as_ptr(),
            MOVE_MOUNT_F_EMPTY_PATH,
        )
    };
    check(r, "move_mount")
}

/// `mount_setattr(fd, "", AT_EMPTY_PATH | [AT_RECURSIVE], attr, sizeof(attr))`
/// with `MOUNT_ATTR_IDMAP` set and `attr->userns_fd = userns`.
pub fn mount_setattr_idmap(
    fd: BorrowedFd,
    userns: BorrowedFd,
    recursive: bool,
) -> Result<(), Error> {
    let attr = MountAttr {
        attr_set: MOUNT_ATTR_IDMAP,
        attr_clr: 0,
        propagation: 0,
        userns_fd: userns.as_raw_fd() as u64,
    };
    let empty = cstr_str("")?;
    let mut flags = AT_EMPTY_PATH;
    if recursive {
        flags |= AT_RECURSIVE;
    }
    let r = unsafe {
        libc::syscall(
            libc::SYS_mount_setattr,
            fd.as_raw_fd(),
            empty.as_ptr(),
            flags,
            &attr as *const _ as *const libc::c_void,
            std::mem::size_of::<MountAttr>(),
        )
    };
    check(r, "mount_setattr IDMAP")
}

/// `mount_setattr(fd, "", AT_EMPTY_PATH, attr, sizeof(attr))` toggling
/// `MOUNT_ATTR_RDONLY`.
pub fn mount_setattr_ro(fd: BorrowedFd, ro: bool) -> Result<(), Error> {
    let attr = MountAttr {
        attr_set: if ro { MOUNT_ATTR_RDONLY } else { 0 },
        attr_clr: if ro { 0 } else { MOUNT_ATTR_RDONLY },
        propagation: 0,
        userns_fd: 0,
    };
    let empty = cstr_str("")?;
    let r = unsafe {
        libc::syscall(
            libc::SYS_mount_setattr,
            fd.as_raw_fd(),
            empty.as_ptr(),
            AT_EMPTY_PATH,
            &attr as *const _ as *const libc::c_void,
            std::mem::size_of::<MountAttr>(),
        )
    };
    check(r, "mount_setattr RDONLY")
}

/// A detached mount fd, produced by `open_tree(OPEN_TREE_CLONE)` or `fsmount`.
///
/// While in this state the mount isn't visible at any path. Attach it with
/// [`DetachedMount::attach`] or drop the fd to discard.
#[derive(Debug)]
pub struct DetachedMount {
    fd: OwnedFd,
}
impl DetachedMount {
    pub fn from_fd(fd: OwnedFd) -> Self {
        Self { fd }
    }
    pub fn as_fd(&self) -> BorrowedFd<'_> {
        self.fd.as_fd()
    }
    /// Apply an idmap to the detached mount via `mount_setattr`. `recursive`
    /// applies the idmap to submounts as well.
    pub fn set_idmap(&self, userns: BorrowedFd, recursive: bool) -> Result<(), Error> {
        mount_setattr_idmap(self.fd.as_fd(), userns, recursive)
    }
    /// Toggle `MOUNT_ATTR_RDONLY` on the detached mount.
    pub fn set_readonly(&self, ro: bool) -> Result<(), Error> {
        mount_setattr_ro(self.fd.as_fd(), ro)
    }
    /// `move_mount` the detached mount onto `mountpoint`. The mount becomes
    /// visible and this `DetachedMount` is consumed.
    pub fn attach(self, mountpoint: &Path) -> Result<(), Error> {
        move_mount(self.fd.as_fd(), mountpoint)
    }
}

/// Build a userns whose uid_map/gid_map describes `idmap`, and return an
/// owned fd referring to that userns.
///
/// Implementation: spawn the helper subcommand `startbox unshare-userns`,
/// which `unshare(CLONE_NEWUSER)`s itself and then pauses on stdin. The
/// parent reads the helper's `ready` line, writes `/proc/<pid>/uid_map` and
/// `/proc/<pid>/gid_map`, and opens `/proc/<pid>/ns/user`. Dropping stdin
/// signals the helper to exit; the userns fd keeps the namespace alive.
pub async fn userns_fd_from_idmap(idmap: &[crate::disk::mount::filesystem::idmapped::IdMap]) -> Result<OwnedFd, Error> {
    let uid_map = idmap
        .iter()
        .map(|i| format!("{} {} {}\n", i.to_id, i.from_id, i.range))
        .collect::<String>();
    let gid_map = uid_map.clone();

    let helper = which_self_exe()?;
    let mut child = Command::new(&helper)
        .arg("unshare-userns")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .with_kind(ErrorKind::Filesystem)?;

    let pid = child
        .id()
        .ok_or_else(|| Error::new(eyre!("helper exited before reporting pid"), ErrorKind::Filesystem))?;

    {
        let stdout = child
            .stdout
            .as_mut()
            .ok_or_else(|| Error::new(eyre!("no stdout"), ErrorKind::Filesystem))?;
        let mut reader = BufReader::new(stdout);
        let mut line = String::new();
        // bounded wait so a wedged helper doesn't hang us forever
        match tokio::time::timeout(Duration::from_secs(5), reader.read_line(&mut line)).await {
            Ok(Ok(_)) if line.trim() == "ready" => (),
            Ok(Ok(_)) => {
                let _ = child.kill().await;
                return Err(Error::new(
                    eyre!("unshare-userns helper said: {}", line.trim()),
                    ErrorKind::Filesystem,
                ));
            }
            Ok(Err(e)) => {
                let _ = child.kill().await;
                return Err(Error::new(e, ErrorKind::Filesystem));
            }
            Err(_) => {
                let _ = child.kill().await;
                return Err(Error::new(
                    eyre!("unshare-userns helper timed out"),
                    ErrorKind::Filesystem,
                ));
            }
        }
    }

    let uid_map_path = format!("/proc/{pid}/uid_map");
    let gid_map_path = format!("/proc/{pid}/gid_map");
    if let Err(e) = tokio::fs::write(&uid_map_path, &uid_map).await {
        let _ = child.kill().await;
        return Err(Error::new(e, ErrorKind::Filesystem))
            .with_ctx(|_| (ErrorKind::Filesystem, format!("write {uid_map_path}")));
    }
    if let Err(e) = tokio::fs::write(&gid_map_path, &gid_map).await {
        let _ = child.kill().await;
        return Err(Error::new(e, ErrorKind::Filesystem))
            .with_ctx(|_| (ErrorKind::Filesystem, format!("write {gid_map_path}")));
    }

    let ns_path = format!("/proc/{pid}/ns/user");
    let userns_fd = tokio::task::spawn_blocking({
        let ns_path = ns_path.clone();
        move || -> Result<OwnedFd, std::io::Error> {
            use std::os::fd::IntoRawFd;
            let f = std::fs::File::open(&ns_path)?;
            Ok(unsafe { OwnedFd::from_raw_fd(f.into_raw_fd()) })
        }
    })
    .await
    .with_kind(ErrorKind::Cancelled)?
    .map_err(|e| Error::new(e, ErrorKind::Filesystem))
    .with_ctx(|_| (ErrorKind::Filesystem, format!("open {ns_path}")))?;

    // signal helper to exit and reap it
    drop(child.stdin.take());
    let _ = child.wait().await;

    Ok(userns_fd)
}

fn which_self_exe() -> Result<OsString, Error> {
    std::env::current_exe()
        .map(|p| p.into_os_string())
        .map_err(|e| Error::new(e, ErrorKind::Filesystem))
        .with_ctx(|_| (ErrorKind::Filesystem, "readlink /proc/self/exe"))
}

/// Body of the `startbox unshare-userns` subcommand. `unshare(CLONE_NEWUSER)`s
/// self, prints `ready\n` to stdout, then waits for stdin to close.
pub fn unshare_userns_main() -> std::io::Result<()> {
    use std::io::{Read, Write};

    nix::sched::unshare(nix::sched::CloneFlags::CLONE_NEWUSER)
        .map_err(|e| std::io::Error::from_raw_os_error(e as i32))?;

    let stdout = std::io::stdout();
    let mut out = stdout.lock();
    out.write_all(b"ready\n")?;
    out.flush()?;
    drop(out);

    // Block until parent closes stdin. read returns 0 on EOF; any byte arriving
    // is also a signal to exit.
    let mut buf = [0u8; 1];
    let _ = std::io::stdin().read(&mut buf);
    Ok(())
}

// loop(4) constants — not exported by libc 0.2.

const LOOP_CTL_GET_FREE: libc::c_ulong = 0x4C82;
const LOOP_CONFIGURE: libc::c_ulong = 0x4C0A;
const LO_FLAGS_READ_ONLY: u32 = 1;
const LO_FLAGS_AUTOCLEAR: u32 = 4;

#[repr(C)]
struct LoopInfo64 {
    lo_device: u64,
    lo_inode: u64,
    lo_rdevice: u64,
    lo_offset: u64,
    lo_sizelimit: u64,
    lo_number: u32,
    lo_encrypt_type: u32,
    lo_encrypt_key_size: u32,
    lo_flags: u32,
    lo_file_name: [u8; 64],
    lo_crypt_name: [u8; 64],
    lo_encrypt_key: [u8; 32],
    lo_init: [u64; 2],
}
impl LoopInfo64 {
    fn zeroed() -> Self {
        // SAFETY: all fields are POD (integers / byte arrays); zeroed bit
        // pattern is a valid LoopInfo64.
        unsafe { std::mem::zeroed() }
    }
}

#[repr(C)]
struct LoopConfig {
    fd: u32,
    block_size: u32,
    info: LoopInfo64,
    __reserved: [u64; 8],
}

/// Allocate a loop device, attach `backing` to it (with `offset` /
/// `sizelimit`), and return both an owned fd referring to the loop device
/// and the `/dev/loopN` path.
///
/// `read_only` sets `LO_FLAGS_READ_ONLY`; the loop device is also marked
/// `LO_FLAGS_AUTOCLEAR` so that the kernel detaches the backing file once
/// the last open fd is closed (avoiding leaked loop devices on crash).
pub async fn loop_attach(
    backing: impl AsRef<Path>,
    offset: u64,
    sizelimit: u64,
    read_only: bool,
) -> Result<(OwnedFd, std::path::PathBuf), Error> {
    let backing = backing.as_ref().to_owned();
    tokio::task::spawn_blocking(move || -> Result<(OwnedFd, std::path::PathBuf), Error> {
        use std::os::fd::IntoRawFd;
        let mut open_opts = std::fs::OpenOptions::new();
        open_opts.read(true);
        if !read_only {
            open_opts.write(true);
        }
        let backing_file = open_opts
            .open(&backing)
            .map_err(|e| Error::new(e, ErrorKind::Filesystem))
            .with_ctx(|_| (ErrorKind::Filesystem, format!("open backing {}", backing.display())))?;

        let ctl = std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open("/dev/loop-control")
            .map_err(|e| Error::new(e, ErrorKind::Filesystem))
            .with_ctx(|_| (ErrorKind::Filesystem, "open /dev/loop-control"))?;
        let loop_nr = unsafe { libc::ioctl(ctl.as_raw_fd(), LOOP_CTL_GET_FREE) };
        if loop_nr < 0 {
            return Err(Error::new(
                io::Error::last_os_error(),
                ErrorKind::Filesystem,
            ))
            .with_ctx(|_| (ErrorKind::Filesystem, "LOOP_CTL_GET_FREE"));
        }
        let loop_path = std::path::PathBuf::from(format!("/dev/loop{}", loop_nr));

        let mut loop_open = std::fs::OpenOptions::new();
        loop_open.read(true);
        if !read_only {
            loop_open.write(true);
        }
        let loop_dev = loop_open
            .open(&loop_path)
            .map_err(|e| Error::new(e, ErrorKind::Filesystem))
            .with_ctx(|_| (ErrorKind::Filesystem, format!("open {}", loop_path.display())))?;

        let mut config = LoopConfig {
            fd: backing_file.as_raw_fd() as u32,
            block_size: 0,
            info: LoopInfo64::zeroed(),
            __reserved: [0; 8],
        };
        config.info.lo_offset = offset;
        config.info.lo_sizelimit = sizelimit;
        config.info.lo_flags = LO_FLAGS_AUTOCLEAR | if read_only { LO_FLAGS_READ_ONLY } else { 0 };

        let r = unsafe {
            libc::ioctl(loop_dev.as_raw_fd(), LOOP_CONFIGURE, &config as *const LoopConfig)
        };
        if r < 0 {
            return Err(Error::new(
                io::Error::last_os_error(),
                ErrorKind::Filesystem,
            ))
            .with_ctx(|_| (ErrorKind::Filesystem, format!("LOOP_CONFIGURE {}", loop_path.display())));
        }

        let owned = unsafe { OwnedFd::from_raw_fd(loop_dev.into_raw_fd()) };
        Ok((owned, loop_path))
    })
    .await
    .with_kind(ErrorKind::Cancelled)?
}

/// `umount2(target, flags)` — drop-in replacement for `umount(8)`. `lazy`
/// sets `MNT_DETACH`.
pub async fn umount2(target: impl AsRef<Path>, lazy: bool) -> Result<(), Error> {
    let p = target.as_ref().to_owned();
    tokio::task::spawn_blocking(move || {
        let mut flags = nix::mount::MntFlags::empty();
        if lazy {
            flags |= nix::mount::MntFlags::MNT_DETACH;
        }
        match nix::mount::umount2(&p, flags) {
            Ok(()) => Ok(()),
            Err(nix::errno::Errno::EINVAL) => Ok(()), // not mounted
            Err(e) => Err(Error::new(
                std::io::Error::from_raw_os_error(e as i32),
                ErrorKind::Filesystem,
            ))
            .with_ctx(|_| (ErrorKind::Filesystem, format!("umount {}", p.display()))),
        }
    })
    .await
    .with_kind(ErrorKind::Cancelled)?
}

/// `syncfs(open(path).fd)` — flush the filesystem containing `path` without
/// shelling out to `sync -f`.
pub async fn syncfs_at(path: impl AsRef<Path>) -> Result<(), Error> {
    let p = path.as_ref().to_owned();
    tokio::task::spawn_blocking(move || -> Result<(), Error> {
        let f = std::fs::File::open(&p).map_err(|e| Error::new(e, ErrorKind::Filesystem))?;
        let r = unsafe { libc::syscall(libc::SYS_syncfs, f.as_raw_fd()) };
        check(r, "syncfs")
    })
    .await
    .with_kind(ErrorKind::Cancelled)?
}

/// Check whether `path` is a mountpoint by comparing st_dev of the path
/// against its parent (the classic technique).
pub async fn is_mountpoint(path: impl AsRef<Path>) -> Result<bool, Error> {
    let p = path.as_ref().to_owned();
    tokio::task::spawn_blocking(move || -> Result<bool, Error> {
        let st = match std::fs::metadata(&p) {
            Ok(s) => s,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(false),
            Err(e) => return Err(Error::new(e, ErrorKind::Filesystem)),
        };
        let parent = match p.parent() {
            Some(parent) if !parent.as_os_str().is_empty() => parent.to_owned(),
            _ => return Ok(true), // root: trivially a mountpoint
        };
        let pst =
            std::fs::metadata(&parent).map_err(|e| Error::new(e, ErrorKind::Filesystem))?;
        use std::os::unix::fs::MetadataExt;
        Ok(st.dev() != pst.dev())
    })
    .await
    .with_kind(ErrorKind::Cancelled)?
}

/// Remount `mountpoint` read-write via `mount(2)` with `MS_REMOUNT`. The
/// `mount -o remount,rw` equivalent.
pub async fn remount_rw(mountpoint: impl AsRef<Path>) -> Result<(), Error> {
    let p = mountpoint.as_ref().to_owned();
    tokio::task::spawn_blocking(move || -> Result<(), Error> {
        use nix::mount::MsFlags;
        nix::mount::mount(
            None::<&Path>,
            &p,
            None::<&str>,
            MsFlags::MS_REMOUNT,
            None::<&str>,
        )
        .map_err(|e| Error::new(std::io::Error::from_raw_os_error(e as i32), ErrorKind::Filesystem))
        .with_ctx(|_| (ErrorKind::Filesystem, format!("remount,rw {}", p.display())))
    })
    .await
    .with_kind(ErrorKind::Cancelled)?
}

/// Mark a mountpoint as `MS_SHARED | MS_REC` (the `mount --make-rshared`
/// equivalent).
pub async fn make_rshared(mountpoint: impl AsRef<Path>) -> Result<(), Error> {
    let p = mountpoint.as_ref().to_owned();
    tokio::task::spawn_blocking(move || -> Result<(), Error> {
        use nix::mount::MsFlags;
        nix::mount::mount(
            None::<&Path>,
            &p,
            None::<&str>,
            MsFlags::MS_SHARED | MsFlags::MS_REC,
            None::<&str>,
        )
        .map_err(|e| Error::new(std::io::Error::from_raw_os_error(e as i32), ErrorKind::Filesystem))
        .with_ctx(|_| (ErrorKind::Filesystem, format!("make-rshared {}", p.display())))
    })
    .await
    .with_kind(ErrorKind::Cancelled)?
}

/// Allow other process-related helpers to import this module's private bits.
#[allow(unused_imports)]
pub(crate) use {move_mount as _, open_tree_clone as _};
