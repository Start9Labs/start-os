use std::collections::{BTreeMap, BTreeSet};

use color_eyre::eyre::eyre;
use models::{ImageId, ProcedureName};
use nix::sys::signal::Signal;
use patch_db::HasModel;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tracing::instrument;

use crate::config::Input;
use crate::container::DockerContainers;
use crate::context::RpcContext;
use crate::dependencies::Dependencies;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::util::Version;
use crate::volume::Volumes;

#[derive(Debug, Deserialize, Serialize)]
pub struct ConfigRes {
    pub input: Option<Input>,
    pub spec: Value,
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
        .and_then(|res| res.map_err(|e| Error::new(eyre!("{}", e.1), ErrorKind::ConfigGen)))
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
            res.map_err(|e| Error::new(eyre!("{}", e.1), ErrorKind::ConfigRulesViolation))
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
