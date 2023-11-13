use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use container_init::ProcessGroupId;
use helpers::UnixRpcClient;
use models::VolumeId;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use tracing::instrument;

use super::ProcedureName;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::util::serde::IoFormat;
use crate::util::{Invoke, Version};
use crate::volume::Volumes;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "kebab-case")]

enum ErrorValue {
    Error(String),
    ErrorCode((i32, String)),
    Result(serde_json::Value),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ExecuteArgs {
    pub procedure: JsProcedure,
    pub directory: PathBuf,
    pub pkg_id: PackageId,
    pub pkg_version: Version,
    pub name: ProcedureName,
    pub volumes: Volumes,
    pub input: Option<serde_json::Value>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct JsProcedure {
    #[serde(default)]
    args: Vec<serde_json::Value>,
}

impl JsProcedure {
    pub fn validate(&self, _volumes: &Volumes) -> Result<(), color_eyre::eyre::Report> {
        Ok(())
    }
}
