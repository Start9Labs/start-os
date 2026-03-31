use std::path::Path;

use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use sha2::Sha256;

use super::FileSystem;
use crate::prelude::*;

pub struct EfiVarFs;
impl FileSystem for EfiVarFs {
    fn mount_type(&self) -> Option<impl AsRef<str>> {
        Some("efivarfs")
    }
    async fn source(&self) -> Result<Option<impl AsRef<Path>>, Error> {
        Ok(Some("efivarfs"))
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("EfiVarFs");
        Ok(sha.finalize())
    }
}
