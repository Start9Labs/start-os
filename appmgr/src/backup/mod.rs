use anyhow::anyhow;
use patch_db::HasModel;
use serde::{Deserialize, Serialize};

use crate::action::ActionImplementation;
use crate::net::host::Hosts;
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
    pub async fn backup(
        &self,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
        hosts: &Hosts,
    ) -> Result<(), Error> {
        let mut volumes = volumes.to_readonly();
        volumes.insert(VolumeId::Backup, Volume::Backup { readonly: false });
        self.create
            .execute(pkg_id, pkg_version, &volumes, hosts, None::<()>, false)
            .await?
            .map_err(|e| anyhow!("{}", e.1))
            .with_kind(crate::ErrorKind::Backup)?;
        Ok(())
    }

    pub async fn restore(
        &self,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
        hosts: &Hosts,
    ) -> Result<(), Error> {
        let mut volumes = volumes.clone();
        volumes.insert(VolumeId::Backup, Volume::Backup { readonly: true });
        self.restore
            .execute(pkg_id, pkg_version, &volumes, hosts, None::<()>, false)
            .await?
            .map_err(|e| anyhow!("{}", e.1))
            .with_kind(crate::ErrorKind::Restore)?;
        Ok(())
    }
}
