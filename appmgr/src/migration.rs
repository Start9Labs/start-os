use color_eyre::eyre::eyre;
use emver::VersionRange;
use futures::{Future, FutureExt, TryFutureExt};
use indexmap::IndexMap;
use patch_db::HasModel;
use serde::{Deserialize, Serialize};
use tracing::instrument;

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
    #[instrument(skip(ctx))]
    pub fn from<'a>(
        &'a self,
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
                        None,
                    )
                    .map(|r| {
                        r.and_then(|r| {
                            r.map_err(|e| {
                                Error::new(eyre!("{}", e.1), crate::ErrorKind::MigrationFailed)
                            })
                        })
                    }),
            )
        } else {
            None
        }
    }

    #[instrument(skip(ctx))]
    pub fn to<'a>(
        &'a self,
        ctx: &'a RpcContext,
        version: &'a Version,
        pkg_id: &'a PackageId,
        pkg_version: &'a Version,
        volumes: &'a Volumes,
    ) -> Option<impl Future<Output = Result<MigrationRes, Error>> + 'a> {
        if let Some((_, migration)) = self.to.iter().find(|(range, _)| version.satisfies(*range)) {
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
                        None,
                    )
                    .map(|r| {
                        r.and_then(|r| {
                            r.map_err(|e| {
                                Error::new(eyre!("{}", e.1), crate::ErrorKind::MigrationFailed)
                            })
                        })
                    }),
            )
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
