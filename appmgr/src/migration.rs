use anyhow::anyhow;
use emver::VersionRange;
use indexmap::IndexMap;
use patch_db::HasModel;
use serde::{Deserialize, Serialize};

use crate::action::ActionImplementation;
use crate::context::RpcContext;
use crate::s9pk::manifest::PackageId;
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
        ctx: &RpcContext,
        version: &Version,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
    ) -> Result<Option<MigrationRes>, Error> {
        Ok(
            if let Some((_, migration)) = self
                .from
                .iter()
                .find(|(range, _)| version.satisfies(*range))
            {
                Some(
                    migration
                        .execute(
                            ctx,
                            pkg_id,
                            pkg_version,
                            Some("Migration"), // Migrations cannot be executed concurrently
                            volumes,
                            Some(version),
                            false,
                        )
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
        ctx: &RpcContext,
        version: &Version,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
    ) -> Result<Option<MigrationRes>, Error> {
        Ok(
            if let Some((_, migration)) =
                self.to.iter().find(|(range, _)| version.satisfies(*range))
            {
                Some(
                    migration
                        .execute(
                            ctx,
                            pkg_id,
                            pkg_version,
                            Some("Migration"),
                            volumes,
                            Some(version),
                            false,
                        )
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
