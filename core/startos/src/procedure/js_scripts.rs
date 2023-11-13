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

    #[instrument(skip_all)]
    pub async fn execute<I: Serialize, O: DeserializeOwned>(
        &self,
        directory: &PathBuf,
        pkg_id: &PackageId,
        pkg_version: &Version,
        name: ProcedureName,
        volumes: &Volumes,
        input: Option<I>,
        timeout: Option<Duration>,
        _gid: ProcessGroupId,
        _rpc_client: Option<Arc<UnixRpcClient>>,
    ) -> Result<Result<O, (i32, String)>, Error> {
        #[cfg(not(test))]
        let mut cmd = Command::new("start-deno");
        #[cfg(test)]
        let mut cmd = test_start_deno_command().await?;

        cmd.arg("execute")
            .input(Some(&mut std::io::Cursor::new(IoFormat::Json.to_vec(
                &ExecuteArgs {
                    procedure: self.clone(),
                    directory: directory.clone(),
                    pkg_id: pkg_id.clone(),
                    pkg_version: pkg_version.clone(),
                    name,
                    volumes: volumes.clone(),
                    input: input.and_then(|x| serde_json::to_value(x).ok()),
                },
            )?)))
            .timeout(timeout)
            .invoke(ErrorKind::Javascript)
            .await
            .and_then(|res| IoFormat::Json.from_slice(&res))
    }

    #[instrument(skip_all)]
    pub async fn sandboxed<I: Serialize, O: DeserializeOwned>(
        &self,
        directory: &PathBuf,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
        input: Option<I>,
        timeout: Option<Duration>,
        name: ProcedureName,
    ) -> Result<Result<O, (i32, String)>, Error> {
        #[cfg(not(test))]
        let mut cmd = Command::new("start-deno");
        #[cfg(test)]
        let mut cmd = test_start_deno_command().await?;

        cmd.arg("sandbox")
            .input(Some(&mut std::io::Cursor::new(IoFormat::Json.to_vec(
                &ExecuteArgs {
                    procedure: self.clone(),
                    directory: directory.clone(),
                    pkg_id: pkg_id.clone(),
                    pkg_version: pkg_version.clone(),
                    name,
                    volumes: volumes.clone(),
                    input: input.and_then(|x| serde_json::to_value(x).ok()),
                },
            )?)))
            .timeout(timeout)
            .invoke(ErrorKind::Javascript)
            .await
            .and_then(|res| IoFormat::Json.from_slice(&res))
    }

    #[instrument(skip_all)]
    pub async fn execute_impl<I: Serialize, O: DeserializeOwned>(
        &self,
        directory: &PathBuf,
        pkg_id: &PackageId,
        pkg_version: &Version,
        name: ProcedureName,
        volumes: &Volumes,
        input: Option<I>,
    ) -> Result<Result<O, (i32, String)>, Error> {
        let res = async move {
            let running_action = JsExecutionEnvironment::load_from_package(
                directory,
                pkg_id,
                pkg_version,
                Box::new(volumes.clone()),
            )
            .await?
            .run_action(name, input, self.args.clone());
            let output: Option<ErrorValue> = running_action.await?;
            let output: O = unwrap_known_error(output)?;
            Ok(output)
        }
        .await
        .map_err(|(error, message)| (error.as_code_num(), message));

        Ok(res)
    }

    #[instrument(skip_all)]
    pub async fn sandboxed_impl<I: Serialize, O: DeserializeOwned>(
        &self,
        directory: &PathBuf,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
        input: Option<I>,
        name: ProcedureName,
    ) -> Result<Result<O, (i32, String)>, Error> {
        Ok(async move {
            let running_action = JsExecutionEnvironment::load_from_package(
                directory,
                pkg_id,
                pkg_version,
                Box::new(volumes.clone()),
            )
            .await?
            .read_only_effects()
            .run_action(name, input, self.args.clone());
            let output: Option<ErrorValue> = running_action.await?;
            let output: O = unwrap_known_error(output)?;
            Ok(output)
        }
        .await
        .map_err(|(error, message)| (error.as_code_num(), message)))
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

#[cfg(test)]
async fn test_start_deno_command() -> Result<Command, Error> {
    Command::new("cargo")
        .arg("build")
        .invoke(ErrorKind::Unknown)
        .await?;
    if tokio::fs::metadata("target/debug/start-deno")
        .await
        .is_err()
    {
        Command::new("ln")
            .arg("-rsf")
            .arg("target/debug/startbox")
            .arg("target/debug/start-deno")
            .invoke(crate::ErrorKind::Filesystem)
            .await?;
    }
    Ok(Command::new("target/debug/start-deno"))
}

#[tokio::test]
async fn js_action_execute() {
    let js_action = JsProcedure { args: vec![] };
    let path: PathBuf = "test/js_action_execute/"
        .parse::<PathBuf>()
        .unwrap()
        .canonicalize()
        .unwrap();
    let package_id = "test-package".parse().unwrap();
    let package_version: Version = "0.3.0.3".parse().unwrap();
    let name = ProcedureName::GetConfig;
    let volumes: Volumes = serde_json::from_value(serde_json::json!({
        "main": {
            "type": "data"
        },
        "compat": {
            "type": "assets"
        },
        "filebrowser" :{
            "package-id": "filebrowser",
            "path": "data",
            "readonly": true,
            "type": "pointer",
            "volume-id": "main",
        }
    }))
    .unwrap();
    let input: Option<serde_json::Value> = Some(serde_json::json!({"test":123}));
    let timeout = Some(Duration::from_secs(10));
    let _output: crate::config::action::ConfigRes = js_action
        .execute(
            &path,
            &package_id,
            &package_version,
            name,
            &volumes,
            input,
            timeout,
            ProcessGroupId(0),
            None,
        )
        .await
        .unwrap()
        .unwrap();
    assert_eq!(
        &std::fs::read_to_string(
            "test/js_action_execute/package-data/volumes/test-package/data/main/test.log"
        )
        .unwrap(),
        "This is a test"
    );
    std::fs::remove_file(
        "test/js_action_execute/package-data/volumes/test-package/data/main/test.log",
    )
    .unwrap();
}

#[tokio::test]
async fn js_action_execute_error() {
    let js_action = JsProcedure { args: vec![] };
    let path: PathBuf = "test/js_action_execute/"
        .parse::<PathBuf>()
        .unwrap()
        .canonicalize()
        .unwrap();
    let package_id = "test-package".parse().unwrap();
    let package_version: Version = "0.3.0.3".parse().unwrap();
    let name = ProcedureName::SetConfig;
    let volumes: Volumes = serde_json::from_value(serde_json::json!({
        "main": {
            "type": "data"
        },
        "compat": {
            "type": "assets"
        },
        "filebrowser" :{
            "package-id": "filebrowser",
            "path": "data",
            "readonly": true,
            "type": "pointer",
            "volume-id": "main",
        }
    }))
    .unwrap();
    let input: Option<serde_json::Value> = None;
    let timeout = Some(Duration::from_secs(10));
    let output: Result<serde_json::Value, _> = js_action
        .execute(
            &path,
            &package_id,
            &package_version,
            name,
            &volumes,
            input,
            timeout,
            ProcessGroupId(0),
            None,
        )
        .await
        .unwrap();
    assert_eq!("Err((2, \"Not setup\"))", &format!("{:?}", output));
}

#[tokio::test]
async fn js_action_fetch() {
    let js_action = JsProcedure { args: vec![] };
    let path: PathBuf = "test/js_action_execute/"
        .parse::<PathBuf>()
        .unwrap()
        .canonicalize()
        .unwrap();
    let package_id = "test-package".parse().unwrap();
    let package_version: Version = "0.3.0.3".parse().unwrap();
    let name = ProcedureName::Action("fetch".parse().unwrap());
    let volumes: Volumes = serde_json::from_value(serde_json::json!({
        "main": {
            "type": "data"
        },
        "compat": {
            "type": "assets"
        },
        "filebrowser" :{
            "package-id": "filebrowser",
            "path": "data",
            "readonly": true,
            "type": "pointer",
            "volume-id": "main",
        }
    }))
    .unwrap();
    let input: Option<serde_json::Value> = None;
    let timeout = Some(Duration::from_secs(10));
    js_action
        .execute::<serde_json::Value, serde_json::Value>(
            &path,
            &package_id,
            &package_version,
            name,
            &volumes,
            input,
            timeout,
            ProcessGroupId(0),
            None,
        )
        .await
        .unwrap()
        .unwrap();
}

#[tokio::test]
async fn js_test_slow() {
    let js_action = JsProcedure { args: vec![] };
    let path: PathBuf = "test/js_action_execute/"
        .parse::<PathBuf>()
        .unwrap()
        .canonicalize()
        .unwrap();
    let package_id = "test-package".parse().unwrap();
    let package_version: Version = "0.3.0.3".parse().unwrap();
    let name = ProcedureName::Action("slow".parse().unwrap());
    let volumes: Volumes = serde_json::from_value(serde_json::json!({
        "main": {
            "type": "data"
        },
        "compat": {
            "type": "assets"
        },
        "filebrowser" :{
            "package-id": "filebrowser",
            "path": "data",
            "readonly": true,
            "type": "pointer",
            "volume-id": "main",
        }
    }))
    .unwrap();
    let input: Option<serde_json::Value> = None;
    let timeout = Some(Duration::from_secs(10));
    tracing::debug!("testing start");
    tokio::select! {
        a = js_action
            .execute::<serde_json::Value, serde_json::Value>(
                &path,
                &package_id,
                &package_version,
                name,
                &volumes,
                input,
                timeout,
                ProcessGroupId(0),
                None,
            ) => { a.unwrap().unwrap(); },
        _ = tokio::time::sleep(Duration::from_secs(1)) => ()
    }
    tracing::debug!("testing end should");
    tokio::time::sleep(Duration::from_secs(2)).await;
    tracing::debug!("Done");
}
#[tokio::test]
async fn js_action_var_arg() {
    let js_action = JsProcedure {
        args: vec![42.into()],
    };
    let path: PathBuf = "test/js_action_execute/"
        .parse::<PathBuf>()
        .unwrap()
        .canonicalize()
        .unwrap();
    let package_id = "test-package".parse().unwrap();
    let package_version: Version = "0.3.0.3".parse().unwrap();
    let name = ProcedureName::Action("js-action-var-arg".parse().unwrap());
    let volumes: Volumes = serde_json::from_value(serde_json::json!({
        "main": {
            "type": "data"
        },
        "compat": {
            "type": "assets"
        },
        "filebrowser" :{
            "package-id": "filebrowser",
            "path": "data",
            "readonly": true,
            "type": "pointer",
            "volume-id": "main",
        }
    }))
    .unwrap();
    let input: Option<serde_json::Value> = None;
    let timeout = Some(Duration::from_secs(10));
    js_action
        .execute::<serde_json::Value, serde_json::Value>(
            &path,
            &package_id,
            &package_version,
            name,
            &volumes,
            input,
            timeout,
            ProcessGroupId(0),
            None,
        )
        .await
        .unwrap()
        .unwrap();
}

#[tokio::test]
async fn js_action_test_rename() {
    let js_action = JsProcedure { args: vec![] };
    let path: PathBuf = "test/js_action_execute/"
        .parse::<PathBuf>()
        .unwrap()
        .canonicalize()
        .unwrap();
    let package_id = "test-package".parse().unwrap();
    let package_version: Version = "0.3.0.3".parse().unwrap();
    let name = ProcedureName::Action("test-rename".parse().unwrap());
    let volumes: Volumes = serde_json::from_value(serde_json::json!({
        "main": {
            "type": "data"
        },
        "compat": {
            "type": "assets"
        },
        "filebrowser" :{
            "package-id": "filebrowser",
            "path": "data",
            "readonly": true,
            "type": "pointer",
            "volume-id": "main",
        }
    }))
    .unwrap();
    let input: Option<serde_json::Value> = None;
    let timeout = Some(Duration::from_secs(10));
    js_action
        .execute::<serde_json::Value, serde_json::Value>(
            &path,
            &package_id,
            &package_version,
            name,
            &volumes,
            input,
            timeout,
            ProcessGroupId(0),
            None,
        )
        .await
        .unwrap()
        .unwrap();
}

#[tokio::test]
async fn js_action_test_deep_dir() {
    let js_action = JsProcedure { args: vec![] };
    let path: PathBuf = "test/js_action_execute/"
        .parse::<PathBuf>()
        .unwrap()
        .canonicalize()
        .unwrap();
    let package_id = "test-package".parse().unwrap();
    let package_version: Version = "0.3.0.3".parse().unwrap();
    let name = ProcedureName::Action("test-deep-dir".parse().unwrap());
    let volumes: Volumes = serde_json::from_value(serde_json::json!({
        "main": {
            "type": "data"
        },
        "compat": {
            "type": "assets"
        },
        "filebrowser" :{
            "package-id": "filebrowser",
            "path": "data",
            "readonly": true,
            "type": "pointer",
            "volume-id": "main",
        }
    }))
    .unwrap();
    let input: Option<serde_json::Value> = None;
    let timeout = Some(Duration::from_secs(10));
    js_action
        .execute::<serde_json::Value, serde_json::Value>(
            &path,
            &package_id,
            &package_version,
            name,
            &volumes,
            input,
            timeout,
            ProcessGroupId(0),
            None,
        )
        .await
        .unwrap()
        .unwrap();
}
#[tokio::test]
async fn js_action_test_deep_dir_escape() {
    let js_action = JsProcedure { args: vec![] };
    let path: PathBuf = "test/js_action_execute/"
        .parse::<PathBuf>()
        .unwrap()
        .canonicalize()
        .unwrap();
    let package_id = "test-package".parse().unwrap();
    let package_version: Version = "0.3.0.3".parse().unwrap();
    let name = ProcedureName::Action("test-deep-dir-escape".parse().unwrap());
    let volumes: Volumes = serde_json::from_value(serde_json::json!({
        "main": {
            "type": "data"
        },
        "compat": {
            "type": "assets"
        },
        "filebrowser" :{
            "package-id": "filebrowser",
            "path": "data",
            "readonly": true,
            "type": "pointer",
            "volume-id": "main",
        }
    }))
    .unwrap();
    let input: Option<serde_json::Value> = None;
    let timeout = Some(Duration::from_secs(10));
    js_action
        .execute::<serde_json::Value, serde_json::Value>(
            &path,
            &package_id,
            &package_version,
            name,
            &volumes,
            input,
            timeout,
            ProcessGroupId(0),
            None,
        )
        .await
        .unwrap()
        .unwrap();
}
#[tokio::test]
async fn js_action_test_zero_dir() {
    let js_action = JsProcedure { args: vec![] };
    let path: PathBuf = "test/js_action_execute/"
        .parse::<PathBuf>()
        .unwrap()
        .canonicalize()
        .unwrap();
    let package_id = "test-package".parse().unwrap();
    let package_version: Version = "0.3.0.3".parse().unwrap();
    let name = ProcedureName::Action("test-zero-dir".parse().unwrap());
    let volumes: Volumes = serde_json::from_value(serde_json::json!({
        "main": {
            "type": "data"
        },
        "compat": {
            "type": "assets"
        },
        "filebrowser" :{
            "package-id": "filebrowser",
            "path": "data",
            "readonly": true,
            "type": "pointer",
            "volume-id": "main",
        }
    }))
    .unwrap();
    let input: Option<serde_json::Value> = None;
    let timeout = Some(Duration::from_secs(10));
    js_action
        .execute::<serde_json::Value, serde_json::Value>(
            &path,
            &package_id,
            &package_version,
            name,
            &volumes,
            input,
            timeout,
            ProcessGroupId(0),
            None,
        )
        .await
        .unwrap()
        .unwrap();
}
#[tokio::test]
async fn js_action_test_read_dir() {
    let js_action = JsProcedure { args: vec![] };
    let path: PathBuf = "test/js_action_execute/"
        .parse::<PathBuf>()
        .unwrap()
        .canonicalize()
        .unwrap();
    let package_id = "test-package".parse().unwrap();
    let package_version: Version = "0.3.0.3".parse().unwrap();
    let name = ProcedureName::Action("test-read-dir".parse().unwrap());
    let volumes: Volumes = serde_json::from_value(serde_json::json!({
        "main": {
            "type": "data"
        },
        "compat": {
            "type": "assets"
        },
        "filebrowser" :{
            "package-id": "filebrowser",
            "path": "data",
            "readonly": true,
            "type": "pointer",
            "volume-id": "main",
        }
    }))
    .unwrap();
    let input: Option<serde_json::Value> = None;
    let timeout = Some(Duration::from_secs(10));
    js_action
        .execute::<serde_json::Value, serde_json::Value>(
            &path,
            &package_id,
            &package_version,
            name,
            &volumes,
            input,
            timeout,
            ProcessGroupId(0),
            None,
        )
        .await
        .unwrap()
        .unwrap();
}

#[tokio::test]
async fn js_rsync() {
    let js_action = JsProcedure { args: vec![] };
    let path: PathBuf = "test/js_action_execute/"
        .parse::<PathBuf>()
        .unwrap()
        .canonicalize()
        .unwrap();
    let package_id = "test-package".parse().unwrap();
    let package_version: Version = "0.3.0.3".parse().unwrap();
    let name = ProcedureName::Action("test-rsync".parse().unwrap());
    let volumes: Volumes = serde_json::from_value(serde_json::json!({
        "main": {
            "type": "data"
        },
        "compat": {
            "type": "assets"
        },
        "filebrowser" :{
            "package-id": "filebrowser",
            "path": "data",
            "readonly": true,
            "type": "pointer",
            "volume-id": "main",
        }
    }))
    .unwrap();
    let input: Option<serde_json::Value> = None;
    let timeout = Some(Duration::from_secs(10));
    js_action
        .execute::<serde_json::Value, serde_json::Value>(
            &path,
            &package_id,
            &package_version,
            name,
            &volumes,
            input,
            timeout,
            ProcessGroupId(0),
            None,
        )
        .await
        .unwrap()
        .unwrap();
}

#[tokio::test]
async fn js_disk_usage() {
    let js_action = JsProcedure { args: vec![] };
    let path: PathBuf = "test/js_action_execute/"
        .parse::<PathBuf>()
        .unwrap()
        .canonicalize()
        .unwrap();
    let package_id = "test-package".parse().unwrap();
    let package_version: Version = "0.3.0.3".parse().unwrap();
    let name = ProcedureName::Action("test-disk-usage".parse().unwrap());
    let volumes: Volumes = serde_json::from_value(serde_json::json!({
        "main": {
            "type": "data"
        },
        "compat": {
            "type": "assets"
        },
        "filebrowser" :{
            "package-id": "filebrowser",
            "path": "data",
            "readonly": true,
            "type": "pointer",
            "volume-id": "main",
        }
    }))
    .unwrap();
    let input: Option<serde_json::Value> = None;
    let timeout = Some(Duration::from_secs(10));
    js_action
        .execute::<serde_json::Value, serde_json::Value>(
            &path,
            &package_id,
            &package_version,
            name,
            &volumes,
            input,
            timeout,
            ProcessGroupId(0),
            None,
        )
        .await
        .unwrap()
        .unwrap();
}
