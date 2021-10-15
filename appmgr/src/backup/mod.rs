use std::collections::BTreeMap;
use std::path::Path;

use chrono::{DateTime, Utc};
use color_eyre::eyre::eyre;
use itertools::Itertools;
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use patch_db::HasModel;
use rpc_toolkit::command;
use serde::ser::SerializeTuple;
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use tokio::io::AsyncWriteExt;
use torut::onion::TorSecretKeyV3;
use tracing::instrument;

use crate::action::{ActionImplementation, NoOutput};
use crate::context::RpcContext;
use crate::net::interface::{InterfaceId, Interfaces};
use crate::s9pk::manifest::PackageId;
use crate::util::{IoFormat, Version};
use crate::volume::{Volume, VolumeId, Volumes, BACKUP_DIR};
use crate::{Error, ResultExt};

mod backup_bulk;

#[derive(Debug, Deserialize, Serialize)]
pub struct BackupReport {
    server: ServerBackupReport,
    packages: BTreeMap<PackageId, PackageBackupReport>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ServerBackupReport {
    attempted: bool,
    error: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct PackageBackupReport {
    error: Option<String>,
}

#[command(subcommands(backup_bulk::backup_all))]
pub fn backup() -> Result<(), Error> {
    Ok(())
}

struct SerializableCertInfo((PKey<Private>, Vec<X509>));
impl Serialize for SerializableCertInfo {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut tup = serializer.serialize_tuple(2)?;
        tup.serialize_element(
            &(self.0)
                .0
                .private_key_to_der()
                .map_err(serde::ser::Error::custom)?,
        )?;
        tup.serialize_element(
            &(self.0)
                .1
                .iter()
                .map(|x| x.to_der())
                .collect::<Result<Vec<_>, _>>()
                .map_err(serde::ser::Error::custom)?,
        )?;
        tup.end()
    }
}
impl<'de> Deserialize<'de> for SerializableCertInfo {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        todo!()
    }
}

#[derive(Deserialize, Serialize)]
struct BackupMetadata {
    pub timestamp: DateTime<Utc>,
    pub tor_keys: BTreeMap<InterfaceId, TorSecretKeyV3>,
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
pub struct BackupActions {
    pub create: ActionImplementation,
    pub restore: ActionImplementation,
}
impl BackupActions {
    #[instrument(skip(ctx))]
    pub async fn create(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        interfaces: &Interfaces,
        volumes: &Volumes,
    ) -> Result<DateTime<Utc>, Error> {
        let mut volumes = volumes.to_readonly();
        volumes.insert(VolumeId::Backup, Volume::Backup { readonly: false });
        self.create
            .execute::<(), NoOutput>(
                ctx,
                pkg_id,
                pkg_version,
                Some("CreateBackup"),
                &volumes,
                None,
                false,
            )
            .await?
            .map_err(|e| eyre!("{}", e.1))
            .with_kind(crate::ErrorKind::Backup)?;
        let tor_keys = interfaces
            .tor_keys(&mut ctx.secret_store.acquire().await?, pkg_id)
            .await?;
        let mut certificates = BTreeMap::new();
        for (iface, key) in &tor_keys {
            certificates.insert(
                iface.clone(),
                ctx.net_controller
                    .certificate_for(
                        &key.public()
                            .get_onion_address()
                            .get_address_without_dot_onion(),
                    )
                    .await?,
            );
        }
        let timestamp = Utc::now();
        let tmp_path = Path::new(BACKUP_DIR).join(pkg_id).join("metadata.cbor.tmp");
        let mut outfile = File::create(&tmp_path).await?;
        outfile.write_all(&IoFormat::Cbor.to_vec(&BackupMetadata {
            timestamp,
            tor_keys,
        })?);
        outfile.flush().await?;
        outfile.shutdown().await?;
        outfile.sync_all().await?;
        tokio::fs::rename(
            &tmp_path,
            Path::new(BACKUP_DIR).join(pkg_id).join("metadata.cbor"),
        )
        .await?;
        Ok(timestamp)
    }

    pub async fn restore(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
    ) -> Result<NoOutput, Error> {
        let mut volumes = volumes.clone();
        volumes.insert(VolumeId::Backup, Volume::Backup { readonly: true });
        self.restore
            .execute(
                ctx,
                pkg_id,
                pkg_version,
                Some("RestoreBackup"),
                &volumes,
                None::<()>,
                false,
            )
            .await?
            .map_err(|e| eyre!("{}", e.1))
            .with_kind(crate::ErrorKind::Restore)?;
        Ok(NoOutput)
    }
}
