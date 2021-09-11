use anyhow::anyhow;
use patch_db::HasModel;
use regex::NoExpand;
use serde::{Deserialize, Serialize};

use crate::action::{ActionImplementation, NoOutput};
use crate::context::RpcContext;
use crate::s9pk::manifest::PackageId;
use crate::util::Version;
use crate::volume::{Volume, VolumeId, Volumes};
use crate::{Error, ResultExt};

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
pub struct BackupActions {
    pub create: ActionImplementation,
    pub restore: ActionImplementation,
}
impl BackupActions {
    pub async fn create(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
    ) -> Result<NoOutput, Error> {
        let mut volumes = volumes.to_readonly();
        volumes.insert(VolumeId::Backup, Volume::Backup { readonly: false });
        self.create
            .execute(
                ctx,
                pkg_id,
                pkg_version,
                Some("CreateBackup"),
                &volumes,
                None::<()>,
                false,
            )
            .await?
            .map_err(|e| anyhow!("{}", e.1))
            .with_kind(crate::ErrorKind::Backup)?;
        Ok(NoOutput)
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
            .map_err(|e| anyhow!("{}", e.1))
            .with_kind(crate::ErrorKind::Restore)?;
        Ok(NoOutput)
    }
}
