use std::path::{Path, PathBuf};

use color_eyre::eyre::eyre;
use tokio::io::AsyncWriteExt;
use tracing::instrument;

use super::filesystem::ecryptfs::EcryptFS;
use super::guard::{GenericMountGuard, TmpMountGuard};
use super::util::{bind, unmount};
use crate::auth::check_password;
use crate::backup::target::BackupInfo;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::util::EmbassyOsRecoveryInfo;
use crate::middleware::encrypt::{decrypt_slice, encrypt_slice};
use crate::s9pk::manifest::PackageId;
use crate::util::serde::IoFormat;
use crate::util::{AtomicFile, FileLock};
use crate::volume::BACKUP_DIR;
use crate::{Error, ResultExt};

pub struct BackupMountGuard<G: GenericMountGuard> {
    backup_disk_mount_guard: Option<G>,
    encrypted_guard: Option<TmpMountGuard>,
    enc_key: String,
    pub unencrypted_metadata: EmbassyOsRecoveryInfo,
    pub metadata: BackupInfo,
}
impl<G: GenericMountGuard> BackupMountGuard<G> {
    fn backup_disk_path(&self) -> &Path {
        if let Some(guard) = &self.backup_disk_mount_guard {
            guard.as_ref()
        } else {
            unreachable!()
        }
    }

    #[instrument(skip(password))]
    pub async fn mount(backup_disk_mount_guard: G, password: &str) -> Result<Self, Error> {
        let backup_disk_path = backup_disk_mount_guard.as_ref();
        let unencrypted_metadata_path =
            backup_disk_path.join("EmbassyBackups/unencrypted-metadata.cbor");
        let mut unencrypted_metadata: EmbassyOsRecoveryInfo =
            if tokio::fs::metadata(&unencrypted_metadata_path)
                .await
                .is_ok()
            {
                IoFormat::Cbor.from_slice(
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
                Default::default()
            };
        let enc_key = if let (Some(hash), Some(wrapped_key)) = (
            unencrypted_metadata.password_hash.as_ref(),
            unencrypted_metadata.wrapped_key.as_ref(),
        ) {
            let wrapped_key =
                base32::decode(base32::Alphabet::RFC4648 { padding: true }, wrapped_key)
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
                base32::Alphabet::RFC4648 { padding: false },
                &rand::random::<[u8; 32]>()[..],
            )
        };

        if unencrypted_metadata.password_hash.is_none() {
            unencrypted_metadata.password_hash = Some(
                argon2::hash_encoded(
                    password.as_bytes(),
                    &rand::random::<[u8; 16]>()[..],
                    &argon2::Config::default(),
                )
                .with_kind(crate::ErrorKind::PasswordHashGeneration)?,
            );
        }
        if unencrypted_metadata.wrapped_key.is_none() {
            unencrypted_metadata.wrapped_key = Some(base32::encode(
                base32::Alphabet::RFC4648 { padding: true },
                &encrypt_slice(&enc_key, password),
            ));
        }

        let crypt_path = backup_disk_path.join("EmbassyBackups/crypt");
        if tokio::fs::metadata(&crypt_path).await.is_err() {
            tokio::fs::create_dir_all(&crypt_path).await.with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    crypt_path.display().to_string(),
                )
            })?;
        }
        let encrypted_guard =
            TmpMountGuard::mount(&EcryptFS::new(&crypt_path, &enc_key), ReadWrite).await?;

        let metadata_path = encrypted_guard.as_ref().join("metadata.cbor");
        let metadata: BackupInfo = if tokio::fs::metadata(&metadata_path).await.is_ok() {
            IoFormat::Cbor.from_slice(&tokio::fs::read(&metadata_path).await.with_ctx(|_| {
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
            unencrypted_metadata,
            metadata,
        })
    }

    pub fn change_password(&mut self, new_password: &str) -> Result<(), Error> {
        self.unencrypted_metadata.password_hash = Some(
            argon2::hash_encoded(
                new_password.as_bytes(),
                &rand::random::<[u8; 16]>()[..],
                &argon2::Config::default(),
            )
            .with_kind(crate::ErrorKind::PasswordHashGeneration)?,
        );
        self.unencrypted_metadata.wrapped_key = Some(base32::encode(
            base32::Alphabet::RFC4648 { padding: false },
            &encrypt_slice(&self.enc_key, new_password),
        ));
        Ok(())
    }

    #[instrument(skip(self))]
    pub async fn mount_package_backup(
        &self,
        id: &PackageId,
    ) -> Result<PackageBackupMountGuard, Error> {
        let lock = FileLock::new(Path::new(BACKUP_DIR).join(format!("{}.lock", id)), false).await?;
        let mountpoint = Path::new(BACKUP_DIR).join(id);
        bind(self.as_ref().join(id), &mountpoint, false).await?;
        Ok(PackageBackupMountGuard {
            mountpoint: Some(mountpoint),
            lock: Some(lock),
        })
    }

    #[instrument(skip(self))]
    pub async fn save(&self) -> Result<(), Error> {
        let metadata_path = self.as_ref().join("metadata.cbor");
        let backup_disk_path = self.backup_disk_path();
        let mut file = AtomicFile::new(&metadata_path).await?;
        file.write_all(&IoFormat::Cbor.to_vec(&self.metadata)?)
            .await?;
        file.save().await?;
        let unencrypted_metadata_path =
            backup_disk_path.join("EmbassyBackups/unencrypted-metadata.cbor");
        let mut file = AtomicFile::new(&unencrypted_metadata_path).await?;
        file.write_all(&IoFormat::Cbor.to_vec(&self.unencrypted_metadata)?)
            .await?;
        file.save().await?;
        Ok(())
    }

    #[instrument(skip(self))]
    pub async fn unmount(mut self) -> Result<(), Error> {
        if let Some(guard) = self.encrypted_guard.take() {
            guard.unmount().await?;
        }
        if let Some(guard) = self.backup_disk_mount_guard.take() {
            guard.unmount().await?;
        }
        Ok(())
    }

    #[instrument(skip(self))]
    pub async fn save_and_unmount(self) -> Result<(), Error> {
        self.save().await?;
        self.unmount().await?;
        Ok(())
    }
}
impl<G: GenericMountGuard> AsRef<Path> for BackupMountGuard<G> {
    fn as_ref(&self) -> &Path {
        if let Some(guard) = &self.encrypted_guard {
            guard.as_ref()
        } else {
            unreachable!()
        }
    }
}
impl<G: GenericMountGuard> Drop for BackupMountGuard<G> {
    fn drop(&mut self) {
        let first = self.encrypted_guard.take();
        let second = self.backup_disk_mount_guard.take();
        tokio::spawn(async move {
            if let Some(guard) = first {
                guard.unmount().await.unwrap();
            }
            if let Some(guard) = second {
                guard.unmount().await.unwrap();
            }
        });
    }
}

pub struct PackageBackupMountGuard {
    mountpoint: Option<PathBuf>,
    lock: Option<FileLock>,
}
impl PackageBackupMountGuard {
    pub async fn unmount(mut self) -> Result<(), Error> {
        if let Some(mountpoint) = self.mountpoint.take() {
            unmount(&mountpoint).await?;
        }
        if let Some(lock) = self.lock.take() {
            lock.unlock().await?;
        }
        Ok(())
    }
}
impl AsRef<Path> for PackageBackupMountGuard {
    fn as_ref(&self) -> &Path {
        if let Some(mountpoint) = &self.mountpoint {
            mountpoint
        } else {
            unreachable!()
        }
    }
}
impl Drop for PackageBackupMountGuard {
    fn drop(&mut self) {
        let mountpoint = self.mountpoint.take();
        let lock = self.lock.take();
        tokio::spawn(async move {
            if let Some(mountpoint) = mountpoint {
                unmount(&mountpoint).await.unwrap();
            }
            if let Some(lock) = lock {
                lock.unlock().await.unwrap();
            }
        });
    }
}
