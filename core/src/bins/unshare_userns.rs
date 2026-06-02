//! `startbox unshare-userns` — internal helper for
//! [`crate::disk::mount::filesystem::syscall::userns_fd_from_idmap`].
//!
//! Unshares the user namespace, prints `ready\n` on stdout so the parent
//! knows it can write `/proc/<pid>/uid_map` / `gid_map`, then blocks on
//! stdin until the parent closes it. The userns fd opened by the parent
//! keeps the namespace alive after this process exits.

use std::collections::VecDeque;
use std::ffi::OsString;

use crate::disk::mount::filesystem::syscall::unshare_userns_main;

pub fn main(_args: VecDeque<OsString>) {
    if let Err(e) = unshare_userns_main() {
        eprintln!("unshare-userns: {e}");
        std::process::exit(1);
    }
}
