use std::path::Path;

use async_trait::async_trait;
use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use sha2::Sha256;

use crate::Error;

pub mod block_dev;
pub mod cifs;
pub mod ecryptfs;
pub mod label;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MountType {
    ReadOnly,
    ReadWrite,
}

pub use MountType::*;

#[async_trait]
pub trait FileSystem {
    async fn mount<P: AsRef<Path> + Send + Sync>(
        &self,
        mountpoint: P,
        mount_type: MountType,
    ) -> Result<(), Error>;
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error>;
}
