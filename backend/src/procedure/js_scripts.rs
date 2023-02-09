use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use color_eyre::eyre::eyre;
use color_eyre::Report;
use embassy_container_init::{ProcessGroupId, SignalGroup, SignalGroupParams};
use helpers::{Address, AddressSchemaLocal, AddressSchemaOnion, Callback, OsApi, UnixRpcClient};
pub use js_engine::JsError;
use js_engine::{JsExecutionEnvironment, PathForVolumeId};
use models::{ErrorKind, InterfaceId, VolumeId};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use super::ProcedureName;
use crate::context::RpcContext;
use crate::s9pk::manifest::PackageId;
use crate::util::{GeneralGuard, Version};
use crate::volume::Volumes;
use crate::Error;

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

struct SandboxOsApi {
    _ctx: RpcContext,
}
#[async_trait::async_trait]
impl OsApi for SandboxOsApi {
    #[allow(unused_variables)]
    async fn get_service_config(
        &self,
        id: PackageId,
        path: &str,
        callback: Option<Callback>,
    ) -> Result<Vec<serde_json::Value>, Report> {
        Err(eyre!("Operation not permitted"))
    }
    #[allow(unused_variables)]
    async fn bind_local(
        &self,
        internal_port: u16,
        address_schema: AddressSchemaLocal,
    ) -> Result<Address, Report> {
        Err(eyre!("Operation not permitted"))
    }
    #[allow(unused_variables)]
    async fn bind_onion(
        &self,
        internal_port: u16,
        address_schema: AddressSchemaOnion,
    ) -> Result<Address, Report> {
        Err(eyre!("Operation not permitted"))
    }
    #[allow(unused_variables)]
    async fn unbind_local(&self, id: InterfaceId, external: u16) -> Result<(), Report> {
        Err(eyre!("Operation not permitted"))
    }
    #[allow(unused_variables)]
    async fn unbind_onion(&self, id: InterfaceId, external: u16) -> Result<(), Report> {
        Err(eyre!("Operation not permitted"))
    }
    fn set_started(&self) -> Result<(), Report> {
        Err(eyre!("Operation not permitted"))
    }
    async fn restart(&self) -> Result<(), Report> {
        Err(eyre!("Operation not permitted"))
    }
    async fn start(&self) -> Result<(), Report> {
        Err(eyre!("Operation not permitted"))
    }
    async fn stop(&self) -> Result<(), Report> {
        Err(eyre!("Operation not permitted"))
    }
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
        gid: ProcessGroupId,
        rpc_client: Option<Arc<UnixRpcClient>>,
        os: Arc<dyn OsApi>,
    ) -> Result<Result<O, (i32, String)>, Error> {
        let cleaner_client = rpc_client.clone();
        let cleaner = GeneralGuard::new(move || {
            tokio::spawn(async move {
                if let Some(client) = cleaner_client {
                    client
                        .request(SignalGroup, SignalGroupParams { gid, signal: 9 })
                        .await
                        .map_err(|e| {
                            Error::new(eyre!("{}: {:?}", e.message, e.data), ErrorKind::Docker)
                        })
                } else {
                    Ok(())
                }
            })
        });
        let res = async move {
            let running_action = JsExecutionEnvironment::load_from_package(
                os,
                directory,
                pkg_id,
                pkg_version,
                Box::new(volumes.clone()),
                gid,
                rpc_client,
            )
            .await?
            .run_action(name, input, self.args.clone());
            let output: Option<ErrorValue> = match timeout {
                Some(timeout_duration) => tokio::time::timeout(timeout_duration, running_action)
                    .await
                    .map_err(|_| (JsError::Timeout, "Timed out. Retrying soon...".to_owned()))??,
                None => running_action.await?,
            };
            let output: O = unwrap_known_error(output)?;
            Ok(output)
        }
        .await
        .map_err(|(error, message)| (error.as_code_num(), message));
        cleaner.drop().await.unwrap()?;
        Ok(res)
    }

    #[instrument(skip_all)]
    pub async fn sandboxed<I: Serialize, O: DeserializeOwned>(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
        input: Option<I>,
        timeout: Option<Duration>,
        name: ProcedureName,
    ) -> Result<Result<O, (i32, String)>, Error> {
        Ok(async move {
            let running_action = JsExecutionEnvironment::load_from_package(
                Arc::new(SandboxOsApi { _ctx: ctx.clone() }),
                &ctx.datadir,
                pkg_id,
                pkg_version,
                Box::new(volumes.clone()),
                ProcessGroupId(0),
                None,
            )
            .await?
            .read_only_effects()
            .run_action(name, input, self.args.clone());
            let output: Option<ErrorValue> = match timeout {
                Some(timeout_duration) => tokio::time::timeout(timeout_duration, running_action)
                    .await
                    .map_err(|_| (JsError::Timeout, "Timed out. Retrying soon...".to_owned()))??,
                None => running_action.await?,
            };
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
mod tests {
    use super::*;
    use helpers::{Address, AddressSchemaLocal, AddressSchemaOnion, Callback, OsApi};
    use serde_json::{json, Value};
    use tokio::sync::watch;

    struct OsApiMock {
        config_callbacks: watch::Sender<Vec<Callback>>,
    }
    impl Default for OsApiMock {
        fn default() -> Self {
            Self {
                config_callbacks: watch::channel(Vec::new()).0,
            }
        }
    }

    #[async_trait::async_trait]
    impl OsApi for OsApiMock {
        #[allow(unused_variables)]
        async fn get_service_config(
            &self,
            id: PackageId,
            path: &str,
            callback: Callback,
        ) -> Result<serde_json::Value, Report> {
            println!("Adding callback");
            self.config_callbacks.send_modify(|x| x.push(callback));
            Ok(Value::Null)
        }
        #[allow(unused_variables)]
        async fn bind_local(
            &self,
            internal_port: u16,
            address_schema: AddressSchemaLocal,
        ) -> Result<Address, Report> {
            todo!()
        }
        #[allow(unused_variables)]
        async fn bind_onion(
            &self,
            internal_port: u16,
            address_schema: AddressSchemaOnion,
        ) -> Result<Address, Report> {
            todo!()
        }
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
        let volumes: Volumes = serde_json::from_value(json!({
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
        let input: Option<serde_json::Value> = Some(json!({"test":123}));
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
                Arc::new(OsApiMock::default()),
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
        let volumes: Volumes = serde_json::from_value(json!({
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
                Arc::new(OsApiMock::default()),
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
        let volumes: Volumes = serde_json::from_value(json!({
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
                Arc::new(OsApiMock::default()),
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
        let volumes: Volumes = serde_json::from_value(json!({
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
                    Arc::new(OsApiMock::default())
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
        let volumes: Volumes = serde_json::from_value(json!({
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
                Arc::new(OsApiMock::default()),
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
        let volumes: Volumes = serde_json::from_value(json!({
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
        let volumes: Volumes = serde_json::from_value(json!({
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
                Arc::new(OsApiMock::default()),
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
        let volumes: Volumes = serde_json::from_value(json!({
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
                Arc::new(OsApiMock::default()),
            )
            .await
            .unwrap()
            .unwrap();
    }
    #[tokio::test]
    async fn js_permissions_and_own() {
        let js_action = JsProcedure { args: vec![] };
        let path: PathBuf = "test/js_action_execute/"
            .parse::<PathBuf>()
            .unwrap()
            .canonicalize()
            .unwrap();
        let package_id = "test-package".parse().unwrap();
        let package_version: Version = "0.3.0.3".parse().unwrap();
        let name = ProcedureName::Action("test-permission-chown".parse().unwrap());
        let volumes: Volumes = serde_json::from_value(json!({
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
                Arc::new(OsApiMock::default()),
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
        let volumes: Volumes = serde_json::from_value(json!({
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
                Arc::new(OsApiMock::default()),
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
        let volumes: Volumes = serde_json::from_value(json!({
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
                Arc::new(OsApiMock::default()),
            )
            .await
            .unwrap()
            .unwrap();
    }
    #[tokio::test]
    async fn test_callback() {
        let api = Arc::new(OsApiMock::default());
        let action_api = api.clone();
        let spawned = tokio::spawn(async move {
            let mut watching = api.config_callbacks.subscribe();
            loop {
                if watching.borrow().is_empty() {
                    watching.changed().await.unwrap();
                    continue;
                }
                api.config_callbacks.send_modify(|x| {
                    x[0](json!("This is something across the wire!"))
                        .map_err(|e| format!("Failed call"))
                        .unwrap();
                });
                break;
            }
        });
        let js_action = JsProcedure { args: vec![] };
        let path: PathBuf = "test/js_action_execute/"
            .parse::<PathBuf>()
            .unwrap()
            .canonicalize()
            .unwrap();
        let package_id = "test-package".parse().unwrap();
        let package_version: Version = "0.3.0.3".parse().unwrap();
        let name = ProcedureName::Action("test-callback".parse().unwrap());
        let volumes: Volumes = serde_json::from_value(json!({
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
                action_api,
            )
            .await
            .unwrap()
            .unwrap();
        spawned.await.unwrap();
    }
}
