use std::borrow::Cow;
use std::fmt::{self, Display};
use std::os::unix::ffi::OsStrExt;
use std::path::Path;

use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use sha2::Sha256;

use super::FileSystem;
use crate::prelude::*;

pub struct BackupFS<DataDir: AsRef<Path>, Password: fmt::Display> {
    data_dir: DataDir,
    password: Password,
    idmapped_root: Vec<(u32, u32)>,
}
impl<DataDir: AsRef<Path>, Password: fmt::Display> BackupFS<DataDir, Password> {
    pub fn new(data_dir: DataDir, password: Password, idmapped_root: Vec<(u32, u32)>) -> Self {
        BackupFS {
            data_dir,
            password,
            idmapped_root,
        }
    }
}
impl<DataDir: AsRef<Path> + Send + Sync, Password: fmt::Display + Send + Sync> FileSystem
    for BackupFS<DataDir, Password>
{
    fn mount_type(&self) -> Option<impl AsRef<str>> {
        Some("backup-fs")
    }
    fn mount_options(&self) -> impl IntoIterator<Item = impl Display> {
        [
            Cow::Owned(format!("password={}", self.password)),
            Cow::Borrowed("file-size-padding=0.05"),
            Cow::Borrowed("allow_other"),
        ]
        .into_iter()
        .chain(
            self.idmapped_root
                .iter()
                .map(|(root, range)| Cow::Owned(format!("idmapped-root={root}:{range}"))),
        )
    }
    async fn source(&self) -> Result<Option<impl AsRef<Path>>, Error> {
        Ok(Some(&self.data_dir))
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("BackupFS");
        sha.update(
            tokio::fs::canonicalize(self.data_dir.as_ref())
                .await
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        self.data_dir.as_ref().display().to_string(),
                    )
                })?
                .as_os_str()
                .as_bytes(),
        );
        Ok(sha.finalize())
    }
}
