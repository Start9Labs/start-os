use std::collections::BTreeSet;

use color_eyre::eyre::eyre;
use emver::VersionRange;
use futures::{Future, FutureExt};
use indexmap::IndexMap;
use patch_db::HasModel;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::id::ImageId;
use crate::procedure::{PackageProcedure, ProcedureName};
use crate::s9pk::manifest::PackageId;
use crate::util::Version;
use crate::volume::Volumes;
use crate::{context::RpcContext, procedure::docker::DockerContainer};
use crate::{Error, ResultExt};

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct Migrations {
    pub from: IndexMap<VersionRange, PackageProcedure>,
    pub to: IndexMap<VersionRange, PackageProcedure>,
}
impl Migrations {
    #[instrument]
    pub fn validate(
        &self,
        container: &Option<DockerContainer>,
        eos_version: &Version,
        volumes: &Volumes,
        image_ids: &BTreeSet<ImageId>,
    ) -> Result<(), Error> {
        for (version, migration) in &self.from {
            migration
                .validate(container, eos_version, volumes, image_ids, true)
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::ValidateS9pk,
                        format!("Migration from {}", version),
                    )
                })?;
        }
        for (version, migration) in &self.to {
            migration
                .validate(container, eos_version, volumes, image_ids, true)
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::ValidateS9pk,
                        format!("Migration to {}", version),
                    )
                })?;
        }
        Ok(())
    }

    #[instrument(skip(ctx))]
    pub fn from<'a>(
        &'a self,
        container: &'a Option<DockerContainer>,
        ctx: &'a RpcContext,
        version: &'a Version,
        pkg_id: &'a PackageId,
        pkg_version: &'a Version,
        volumes: &'a Volumes,
    ) -> Option<impl Future<Output = Result<MigrationRes, Error>> + 'a> {
        if let Some((_, migration)) = self
            .from
            .iter()
            .find(|(range, _)| version.satisfies(*range))
        {
            Some(async move {
                migration
                    .execute(
                        ctx,
                        container,
                        pkg_id,
                        pkg_version,
                        ProcedureName::Migration, // Migrations cannot be executed concurrently
                        volumes,
                        Some(version),
                        None,
                    )
                    .map(|r| {
                        r.and_then(|r| {
                            r.map_err(|e| {
                                Error::new(eyre!("{}", e.1), crate::ErrorKind::MigrationFailed)
                            })
                        })
                    })
                    .await
            })
        } else {
            None
        }
    }

    #[instrument(skip(ctx))]
    pub fn to<'a>(
        &'a self,
        container: &'a Option<DockerContainer>,
        ctx: &'a RpcContext,
        version: &'a Version,
        pkg_id: &'a PackageId,
        pkg_version: &'a Version,
        volumes: &'a Volumes,
    ) -> Option<impl Future<Output = Result<MigrationRes, Error>> + 'a> {
        if let Some((_, migration)) = self.to.iter().find(|(range, _)| version.satisfies(*range)) {
            Some(async move {
                migration
                    .execute(
                        ctx,
                        container,
                        pkg_id,
                        pkg_version,
                        ProcedureName::Migration,
                        volumes,
                        Some(version),
                        None,
                    )
                    .map(|r| {
                        r.and_then(|r| {
                            r.map_err(|e| {
                                Error::new(eyre!("{}", e.1), crate::ErrorKind::MigrationFailed)
                            })
                        })
                    })
                    .await
            })
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct MigrationRes {
    pub configured: bool,
}
