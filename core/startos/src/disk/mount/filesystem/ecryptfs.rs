use std::os::unix::ffi::OsStrExt;
use std::path::Path;

use async_trait::async_trait;
use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use sha2::Sha256;

use super::{FileSystem, MountType};
use crate::util::Invoke;
use crate::{Error, ResultExt};

pub async fn mount_ecryptfs<P0: AsRef<Path>, P1: AsRef<Path>>(
    src: P0,
    dst: P1,
    key: &str,
) -> Result<(), Error> {
    tokio::fs::create_dir_all(dst.as_ref()).await?;
    tokio::process::Command::new("mount")
        .arg("-t")
        .arg("ecryptfs")
        .arg(src.as_ref())
        .arg(dst.as_ref())
        .arg("-o")
        // for more information `man ecryptfs` 
        .arg(format!("key=passphrase:passphrase_passwd={},ecryptfs_cipher=aes,ecryptfs_key_bytes=32,ecryptfs_passthrough=n,ecryptfs_enable_filename_crypto=y,no_sig_cache", key))
        .input(Some(&mut std::io::Cursor::new(b"\n")))
        .invoke(crate::ErrorKind::Filesystem).await?;
    Ok(())
}

pub struct EcryptFS<EncryptedDir: AsRef<Path>, Key: AsRef<str>> {
    encrypted_dir: EncryptedDir,
    key: Key,
}
impl<EncryptedDir: AsRef<Path>, Key: AsRef<str>> EcryptFS<EncryptedDir, Key> {
    pub fn new(encrypted_dir: EncryptedDir, key: Key) -> Self {
        EcryptFS { encrypted_dir, key }
    }
}
#[async_trait]
impl<EncryptedDir: AsRef<Path> + Send + Sync, Key: AsRef<str> + Send + Sync> FileSystem
    for EcryptFS<EncryptedDir, Key>
{
    async fn mount<P: AsRef<Path> + Send + Sync>(
        &self,
        mountpoint: P,
        _mount_type: MountType, // ignored - inherited from parent fs
    ) -> Result<(), Error> {
        mount_ecryptfs(self.encrypted_dir.as_ref(), mountpoint, self.key.as_ref()).await
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("EcryptFS");
        sha.update(
            tokio::fs::canonicalize(self.encrypted_dir.as_ref())
                .await
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        self.encrypted_dir.as_ref().display().to_string(),
                    )
                })?
                .as_os_str()
                .as_bytes(),
        );
        Ok(sha.finalize())
    }
}
