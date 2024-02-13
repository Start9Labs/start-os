use std::os::unix::ffi::OsStrExt;
use std::path::Path;

use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use serde::{Deserialize, Serialize};
use sha2::Sha256;

use super::FileSystem;
use crate::prelude::*;

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct BlockDev<LogicalName: AsRef<Path>> {
    logicalname: LogicalName,
}
impl<LogicalName: AsRef<Path>> BlockDev<LogicalName> {
    pub fn new(logicalname: LogicalName) -> Self {
        BlockDev { logicalname }
    }
}
impl<LogicalName: AsRef<Path> + Send + Sync> FileSystem for BlockDev<LogicalName> {
    fn source(&self) -> Option<impl AsRef<Path>> {
        Some(&self.logicalname)
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("BlockDev");
        sha.update(
            tokio::fs::canonicalize(self.logicalname.as_ref())
                .await
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        self.logicalname.as_ref().display().to_string(),
                    )
                })?
                .as_os_str()
                .as_bytes(),
        );
        Ok(sha.finalize())
    }
}
