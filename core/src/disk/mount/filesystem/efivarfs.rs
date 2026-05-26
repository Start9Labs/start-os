use std::os::fd::AsFd;
use std::path::Path;

use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use sha2::Sha256;

use super::FileSystem;
use crate::disk::mount::filesystem::MountType;
use crate::disk::mount::filesystem::syscall::{
    DetachedMount, fsconfig_create, fsmount, fsopen,
};
use crate::prelude::*;

pub struct EfiVarFs;
impl FileSystem for EfiVarFs {
    async fn mount<P: AsRef<Path> + Send>(
        &self,
        mountpoint: P,
        mount_type: MountType,
    ) -> Result<(), Error> {
        let mp = mountpoint.as_ref();
        tokio::fs::create_dir_all(mp).await?;
        let fs = fsopen("efivarfs")?;
        fsconfig_create(fs.as_fd())?;
        let detached = DetachedMount::from_fd(fsmount(fs.as_fd(), 0)?);
        if matches!(mount_type, MountType::ReadOnly) {
            detached.set_readonly(true)?;
        }
        detached.attach(mp)?;
        Ok(())
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("EfiVarFs");
        Ok(sha.finalize())
    }
}
