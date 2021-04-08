use anyhow::anyhow;
use emver::VersionRange;
use indexmap::{IndexMap, IndexSet};
use patch_db::HasModel;
use serde::{Deserialize, Serialize};

use crate::action::ActionImplementation;
use crate::net::host::Hosts;
use crate::s9pk::manifest::PackageId;
use crate::status::health_check::HealthCheckId;
use crate::util::Version;
use crate::volume::Volumes;
use crate::Error;

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct Migrations {
    pub from: IndexMap<VersionRange, ActionImplementation>,
    pub to: IndexMap<VersionRange, ActionImplementation>,
}
impl Migrations {
    pub async fn from(
        &self,
        version: &Version,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
        hosts: &Hosts,
    ) -> Result<Option<MigrationRes>, Error> {
        Ok(
            if let Some((_, migration)) = self
                .from
                .iter()
                .find(|(range, _)| version.satisfies(*range))
            {
                Some(
                    migration
                        .execute(pkg_id, pkg_version, volumes, hosts, Some(version), false)
                        .await?
                        .map_err(|e| {
                            Error::new(anyhow!("{}", e.1), crate::ErrorKind::MigrationFailed)
                        })?,
                )
            } else {
                None
            },
        )
    }
    pub async fn to(
        &self,
        version: &Version,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
        hosts: &Hosts,
    ) -> Result<Option<MigrationRes>, Error> {
        Ok(
            if let Some((_, migration)) =
                self.to.iter().find(|(range, _)| version.satisfies(*range))
            {
                Some(
                    migration
                        .execute(pkg_id, pkg_version, volumes, hosts, Some(version), false)
                        .await?
                        .map_err(|e| {
                            Error::new(anyhow!("{}", e.1), crate::ErrorKind::MigrationFailed)
                        })?,
                )
            } else {
                None
            },
        )
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct MigrationRes {
    pub configured: bool,
}
