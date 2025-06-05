use std::path::{Path, PathBuf};
use std::sync::Arc;

use color_eyre::eyre::eyre;
use helpers::AtomicFile;
use models::PackageId;
use tokio::io::AsyncWriteExt;
use tracing::instrument;

use super::guard::{GenericMountGuard, TmpMountGuard};
use crate::auth::check_password;
use crate::backup::target::BackupInfo;
use crate::disk::mount::filesystem::backupfs::BackupFS;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::SubPath;
use crate::disk::util::StartOsRecoveryInfo;
use crate::util::crypto::{decrypt_slice, encrypt_slice};
use crate::util::serde::IoFormat;
use crate::{Error, ErrorKind, ResultExt};

#[derive(Clone, Debug)]
pub struct BackupMountGuard<G: GenericMountGuard> {
    backup_disk_mount_guard: Option<G>,
    encrypted_guard: Option<TmpMountGuard>,
    enc_key: String,
    unencrypted_metadata_path: PathBuf,
    pub unencrypted_metadata: StartOsRecoveryInfo,
    pub metadata: BackupInfo,
}
impl<G: GenericMountGuard> BackupMountGuard<G> {
    #[instrument(skip_all)]
    pub async fn load_metadata(
        backup_disk_path: &Path,
        server_id: &str,
        password: &str,
    ) -> Result<(StartOsRecoveryInfo, String), Error> {
        let backup_dir = backup_disk_path.join("StartOSBackups").join(server_id);
        let unencrypted_metadata_path = backup_dir.join("unencrypted-metadata.json");
        let crypt_path = backup_dir.join("crypt");
        let mut unencrypted_metadata: StartOsRecoveryInfo =
            if tokio::fs::metadata(&unencrypted_metadata_path)
                .await
                .is_ok()
            {
                IoFormat::Json.from_slice(
                    &tokio::fs::read(&unencrypted_metadata_path)
                        .await
                        .with_ctx(|_| {
                            (
                                crate::ErrorKind::Filesystem,
                                unencrypted_metadata_path.display().to_string(),
                            )
                        })?,
                )?
            } else {
                if tokio::fs::metadata(&crypt_path).await.is_ok() {
                    tokio::fs::remove_dir_all(&crypt_path).await?;
                }
                Default::default()
            };
        let enc_key = if let (Some(hash), Some(wrapped_key)) = (
            unencrypted_metadata.password_hash.as_ref(),
            unencrypted_metadata.wrapped_key.as_ref(),
        ) {
            let wrapped_key =
                base32::decode(base32::Alphabet::Rfc4648 { padding: true }, wrapped_key)
                    .ok_or_else(|| {
                        Error::new(
                            eyre!("failed to decode wrapped key"),
                            crate::ErrorKind::Backup,
                        )
                    })?;
            check_password(hash, password)?;
            String::from_utf8(decrypt_slice(wrapped_key, password))?
        } else {
            base32::encode(
                base32::Alphabet::Rfc4648 { padding: false },
                &rand::random::<[u8; 32]>()[..],
            )
        };
        if unencrypted_metadata.password_hash.is_none() {
            unencrypted_metadata.password_hash = Some(
                argon2::hash_encoded(
                    password.as_bytes(),
                    &rand::random::<[u8; 16]>()[..],
                    &argon2::Config::rfc9106_low_mem(),
                )
                .with_kind(crate::ErrorKind::PasswordHashGeneration)?,
            );
        }
        if unencrypted_metadata.wrapped_key.is_none() {
            unencrypted_metadata.wrapped_key = Some(base32::encode(
                base32::Alphabet::Rfc4648 { padding: true },
                &encrypt_slice(&enc_key, password),
            ));
        }
        Ok((unencrypted_metadata, enc_key))
    }
    #[instrument(skip_all)]
    pub async fn mount(
        backup_disk_mount_guard: G,
        server_id: &str,
        password: &str,
    ) -> Result<Self, Error> {
        let backup_disk_path = backup_disk_mount_guard.path();
        let (unencrypted_metadata, enc_key) =
            Self::load_metadata(backup_disk_path, server_id, password).await?;
        let backup_dir = backup_disk_path.join("StartOSBackups").join(server_id);
        let unencrypted_metadata_path = backup_dir.join("unencrypted-metadata.json");
        let crypt_path = backup_dir.join("crypt");

        if tokio::fs::metadata(&crypt_path).await.is_err() {
            tokio::fs::create_dir_all(&crypt_path).await.with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    crypt_path.display().to_string(),
                )
            })?;
        }
        let encrypted_guard = TmpMountGuard::mount(
            &BackupFS::new(&crypt_path, &enc_key, vec![(100000, 65536)]),
            ReadWrite,
        )
        .await?;

        let metadata_path = encrypted_guard.path().join("metadata.json");
        let metadata: BackupInfo = if tokio::fs::metadata(&metadata_path).await.is_ok() {
            IoFormat::Json.from_slice(&tokio::fs::read(&metadata_path).await.with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    metadata_path.display().to_string(),
                )
            })?)?
        } else {
            Default::default()
        };

        Ok(Self {
            backup_disk_mount_guard: Some(backup_disk_mount_guard),
            encrypted_guard: Some(encrypted_guard),
            enc_key,
            unencrypted_metadata_path,
            unencrypted_metadata,
            metadata,
        })
    }

    pub fn change_password(&mut self, new_password: &str) -> Result<(), Error> {
        self.unencrypted_metadata.password_hash = Some(
            argon2::hash_encoded(
                new_password.as_bytes(),
                &rand::random::<[u8; 16]>()[..],
                &argon2::Config::rfc9106_low_mem(),
            )
            .with_kind(crate::ErrorKind::PasswordHashGeneration)?,
        );
        self.unencrypted_metadata.wrapped_key = Some(base32::encode(
            base32::Alphabet::Rfc4648 { padding: false },
            &encrypt_slice(&self.enc_key, new_password),
        ));
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn package_backup(
        self: &Arc<Self>,
        id: &PackageId,
    ) -> Result<SubPath<Arc<Self>>, Error> {
        let package_guard = SubPath::new(self.clone(), id);
        let package_path = package_guard.path();
        if tokio::fs::metadata(&package_path).await.is_err() {
            tokio::fs::create_dir_all(&package_path)
                .await
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        package_path.display().to_string(),
                    )
                })?;
        }
        Ok(package_guard)
    }

    #[instrument(skip_all)]
    pub async fn save(&self) -> Result<(), Error> {
        let metadata_path = self.path().join("metadata.json");
        let mut file = AtomicFile::new(&metadata_path, None::<PathBuf>)
            .await
            .with_kind(ErrorKind::Filesystem)?;
        file.write_all(&IoFormat::Json.to_vec(&self.metadata)?)
            .await?;
        file.save().await.with_kind(ErrorKind::Filesystem)?;
        let mut file = AtomicFile::new(&self.unencrypted_metadata_path, None::<PathBuf>)
            .await
            .with_kind(ErrorKind::Filesystem)?;
        file.write_all(&IoFormat::Json.to_vec(&self.unencrypted_metadata)?)
            .await?;
        file.save().await.with_kind(ErrorKind::Filesystem)?;
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn save_and_unmount(self) -> Result<(), Error> {
        self.save().await?;
        self.unmount().await?;
        Ok(())
    }
}
impl<G: GenericMountGuard> GenericMountGuard for BackupMountGuard<G> {
    fn path(&self) -> &Path {
        if let Some(guard) = &self.encrypted_guard {
            guard.path()
        } else {
            unreachable!()
        }
    }
    async fn unmount(mut self) -> Result<(), Error> {
        if let Some(guard) = self.encrypted_guard.take() {
            guard.unmount().await?;
        }
        if let Some(guard) = self.backup_disk_mount_guard.take() {
            guard.unmount().await?;
        }
        Ok(())
    }
}
impl<G: GenericMountGuard> Drop for BackupMountGuard<G> {
    fn drop(&mut self) {
        let first = self.encrypted_guard.take();
        let second = self.backup_disk_mount_guard.take();
        tokio::spawn(async move {
            if let Some(guard) = first {
                guard.unmount().await.log_err();
            }
            if let Some(guard) = second {
                guard.unmount().await.log_err();
            }
        });
    }
}
