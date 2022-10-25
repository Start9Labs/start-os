use std::collections::{BTreeMap, BTreeSet};

use color_eyre::eyre::eyre;
use nix::sys::signal::Signal;
use patch_db::HasModel;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use super::{Config, ConfigSpec};
use crate::context::RpcContext;
use crate::dependencies::Dependencies;
use crate::id::ImageId;
use crate::procedure::docker::DockerContainers;
use crate::procedure::{PackageProcedure, ProcedureName};
use crate::s9pk::manifest::PackageId;
use crate::status::health_check::HealthCheckId;
use crate::util::Version;
use crate::volume::Volumes;
use crate::{Error, ResultExt};

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct ConfigRes {
    pub config: Option<Config>,
    pub spec: ConfigSpec,
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
pub struct ConfigActions {
    pub get: PackageProcedure,
    pub set: PackageProcedure,
}
impl ConfigActions {
    #[instrument]
    pub fn validate(
        &self,
        container: &Option<DockerContainers>,
        eos_version: &Version,
        volumes: &Volumes,
        image_ids: &BTreeSet<ImageId>,
    ) -> Result<(), Error> {
        self.get
            .validate(container, eos_version, volumes, image_ids, true)
            .with_ctx(|_| (crate::ErrorKind::ValidateS9pk, "Config Get"))?;
        self.set
            .validate(container, eos_version, volumes, image_ids, true)
            .with_ctx(|_| (crate::ErrorKind::ValidateS9pk, "Config Set"))?;
        Ok(())
    }
    #[instrument(skip(ctx))]
    pub async fn get(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
    ) -> Result<ConfigRes, Error> {
        self.get
            .execute(
                ctx,
                pkg_id,
                pkg_version,
                ProcedureName::GetConfig,
                volumes,
                None::<()>,
                None,
            )
            .await
            .and_then(|res| {
                res.map_err(|e| Error::new(eyre!("{}", e.1), crate::ErrorKind::ConfigGen))
            })
    }

    #[instrument(skip(ctx))]
    pub async fn set(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        dependencies: &Dependencies,
        volumes: &Volumes,
        input: &Config,
    ) -> Result<SetResult, Error> {
        let res: SetResult = self
            .set
            .execute(
                ctx,
                pkg_id,
                pkg_version,
                ProcedureName::SetConfig,
                volumes,
                Some(input),
                None,
            )
            .await
            .and_then(|res| {
                res.map_err(|e| {
                    Error::new(eyre!("{}", e.1), crate::ErrorKind::ConfigRulesViolation)
                })
            })?;
        Ok(SetResult {
            signal: res.signal,
            depends_on: res
                .depends_on
                .into_iter()
                .filter(|(pkg, _)| dependencies.0.contains_key(pkg))
                .collect(),
        })
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct SetResult {
    #[serde(default)]
    #[serde(deserialize_with = "crate::util::serde::deserialize_from_str_opt")]
    #[serde(serialize_with = "crate::util::serde::serialize_display_opt")]
    pub signal: Option<Signal>,
    pub depends_on: BTreeMap<PackageId, BTreeSet<HealthCheckId>>,
}
