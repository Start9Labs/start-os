use std::ffi::OsStr;
use std::fmt::{Display, Write};
use std::path::Path;

use digest::generic_array::GenericArray;
use digest::OutputSizeUser;
use futures::Future;
use sha2::Sha256;
use tokio::process::Command;

use crate::prelude::*;
use crate::util::Invoke;

pub mod bind;
pub mod block_dev;
pub mod cifs;
pub mod ecryptfs;
pub mod efivarfs;
pub mod httpdirfs;
pub mod idmapped;
pub mod label;
pub mod loop_dev;
pub mod overlayfs;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MountType {
    ReadOnly,
    ReadWrite,
}

pub use MountType::*;

pub(self) fn default_mount_command(
    fs: &(impl FileSystem + ?Sized),
    mountpoint: impl AsRef<Path> + Send,
    mount_type: MountType,
) -> std::process::Command {
    let mut cmd = std::process::Command::new("mount");
    if mount_type == ReadOnly {
        cmd.arg("-r");
    }
    if let Some(ty) = fs.mount_type() {
        cmd.arg("-t").arg(ty.as_ref());
    }
    if let Some(options) = fs
        .mount_options()
        .into_iter()
        .fold(None, |acc: Option<String>, x| match acc {
            Some(mut s) => {
                write!(s, ",{}", x);
                Some(s)
            }
            None => Some(x.to_string()),
        })
    {
        cmd.arg("-o").arg(options);
    }
    cmd.args(fs.extra_args());
    if let Some(source) = fs.source() {
        cmd.arg(source.as_ref());
    }
    cmd.arg(mountpoint.as_ref());
    cmd
}

pub(self) async fn default_mount_impl(
    fs: &(impl FileSystem + ?Sized),
    mountpoint: impl AsRef<Path> + Send,
    mount_type: MountType,
) -> Result<(), Error> {
    fs.pre_mount().await?;
    tokio::fs::create_dir_all(mountpoint.as_ref()).await?;
    Command::from(default_mount_command(fs, mountpoint, mount_type))
        .invoke(ErrorKind::Filesystem)
        .await?;

    Ok(())
}

pub trait FileSystem: Send + Sync {
    fn mount_type(&self) -> Option<impl AsRef<str>> {
        None::<&str>
    }
    fn extra_args(&self) -> impl IntoIterator<Item = impl AsRef<OsStr>> {
        [] as [&str; 0]
    }
    fn mount_options(&self) -> impl IntoIterator<Item = impl Display> {
        [] as [&str; 0]
    }
    fn source(&self) -> Option<impl AsRef<Path>> {
        None::<&Path>
    }
    fn pre_mount(&self) -> impl Future<Output = Result<(), Error>> + Send {
        async { Ok(()) }
    }
    fn mount<P: AsRef<Path> + Send>(
        &self,
        mountpoint: P,
        mount_type: MountType,
    ) -> impl Future<Output = Result<(), Error>> + Send {
        default_mount_impl(self, mountpoint, mount_type)
    }
    fn source_hash(
        &self,
    ) -> impl Future<Output = Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error>>
           + Send;
}
