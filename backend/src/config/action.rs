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

#[instrument(skip_all)]
pub async fn get(manager: Arc<Manager>) -> Result<ConfigRes, Error> {
    manager
        .run_procedure(ProcedureName::GetConfig, None::<()>, None)
        .await
}

#[instrument(skip_all)]
pub async fn set(manager: Arc<Manager>, input: &Input) -> Result<Value, Error> {
    manager
        .run_procedure(ProcedureName::SetConfig, Some(input), None)
        .await
}
