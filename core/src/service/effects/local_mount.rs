//! `start-container mount` — syscall-based bind mount with optional idmap.
//!
//! Used by container-runtime to set up volume/asset/dependency/backup mounts
//! from inside an LXC subcontainer. The CLI surface is intentionally narrow:
//! a source path, a target path, and a handful of optional knobs. Idmap
//! entries follow the standard `b:from:to:range` shorthand (range optional,
//! defaults to 1) so they can come straight from the SDK's
//! `SharedOptions.idmap` field.

use std::path::PathBuf;

use clap::Parser;
use rpc_toolkit::Context;
use serde::{Deserialize, Serialize};
use std::os::fd::AsFd;
use ts_rs::TS;

use crate::disk::mount::filesystem::idmapped::IdMap;
use crate::disk::mount::filesystem::syscall::{self, DetachedMount};
use crate::prelude::*;

#[derive(Debug, Clone, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
pub struct LocalMountParams {
    /// Source path. May be any path on the filesystem; the kernel produces
    /// a detached bind clone via `open_tree(OPEN_TREE_CLONE)`.
    #[arg(long)]
    pub source: PathBuf,
    /// Mountpoint to attach to. Created if it doesn't exist (as a directory,
    /// unless `--file` is set).
    #[arg(long)]
    pub target: PathBuf,
    /// Apply the bind recursively (the `--rbind` semantic).
    #[arg(long, default_value_t = false)]
    pub recursive: bool,
    /// Mark the mount read-only via `mount_setattr(MOUNT_ATTR_RDONLY)`.
    #[arg(long, default_value_t = false)]
    pub readonly: bool,
    /// Treat the target as a regular file rather than a directory when
    /// creating it.
    #[arg(long, default_value_t = false)]
    pub file: bool,
    /// Idmap entries; repeat the flag for each entry. Format
    /// `from:to[:range]` (range defaults to 1).
    #[arg(long, value_parser = <IdMap as clap::builder::ValueParserFactory>::value_parser())]
    pub idmap: Vec<IdMap>,
}

pub async fn local_mount<C: Context>(_: C, params: LocalMountParams) -> Result<(), Error> {
    let LocalMountParams {
        source,
        target,
        recursive,
        readonly,
        file,
        idmap,
    } = params;

    if file {
        if let Some(parent) = target.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }
        if tokio::fs::metadata(&target).await.is_err() {
            tokio::fs::File::create(&target).await?.sync_all().await?;
        }
    } else {
        tokio::fs::create_dir_all(&target).await?;
    }

    let fd = syscall::open_tree_clone(&source, recursive)?;
    let detached = DetachedMount::from_fd(fd);

    if !idmap.is_empty() {
        let userns = syscall::userns_fd_from_idmap(&idmap).await?;
        detached.set_idmap(userns.as_fd(), recursive)?;
        drop(userns);
    }
    if readonly {
        detached.set_readonly(true)?;
    }

    detached.attach(&target)?;
    Ok(())
}
