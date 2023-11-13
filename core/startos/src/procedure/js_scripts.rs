use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use container_init::ProcessGroupId;
use helpers::UnixRpcClient;
pub use js_engine::JsError;
use js_engine::{JsExecutionEnvironment, PathForVolumeId};
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

impl PathForVolumeId for Volumes {
    fn path_for(
        &self,
        data_dir: &Path,
        package_id: &PackageId,
        version: &Version,
        volume_id: &VolumeId,
    ) -> Option<PathBuf> {
        let volume = self.get(volume_id)?;
        Some(volume.path_for(data_dir, package_id, version, volume_id))
    }

    fn readonly(&self, volume_id: &VolumeId) -> bool {
        self.get(volume_id).map(|x| x.readonly()).unwrap_or(false)
    }
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

fn unwrap_known_error<O: DeserializeOwned>(
    error_value: Option<ErrorValue>,
) -> Result<O, (JsError, String)> {
    let error_value = error_value.unwrap_or_else(|| ErrorValue::Result(serde_json::Value::Null));
    match error_value {
        ErrorValue::Error(error) => Err((JsError::Javascript, error)),
        ErrorValue::ErrorCode((code, message)) => Err((JsError::Code(code), message)),
        ErrorValue::Result(ref value) => match serde_json::from_value(value.clone()) {
            Ok(a) => Ok(a),
            Err(err) => {
                tracing::error!("{}", err);
                tracing::debug!("{:?}", err);
                Err((
                    JsError::BoundryLayerSerDe,
                    format!(
                        "Couldn't convert output = {:#?} to the correct type",
                        serde_json::to_string_pretty(&error_value).unwrap_or_default()
                    ),
                ))
            }
        },
    }
}
