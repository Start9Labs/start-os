use std::path::Path;

use async_trait::async_trait;
use digest::generic_array::GenericArray;
use digest::Digest;
use sha2::Sha256;

use crate::Error;

pub mod block_dev;
pub mod cifs;
pub mod ecryptfs;
pub mod label;

#[async_trait]
pub trait FileSystem {
    async fn mount<P: AsRef<Path> + Send + Sync>(&self, mountpoint: P) -> Result<(), Error>;
    async fn source_hash(&self) -> Result<GenericArray<u8, <Sha256 as Digest>::OutputSize>, Error>;
}
