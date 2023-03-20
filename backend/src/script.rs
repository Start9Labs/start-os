use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use color_eyre::eyre::eyre;
use color_eyre::Report;
use embassy_container_init::{ProcessGroupId, SignalGroup, SignalGroupParams};
use helpers::{Address, AddressSchemaLocal, AddressSchemaOnion, Callback, OsApi, UnixRpcClient};
pub use js_engine::JsError as JsEngineError;
use js_engine::{JsExecutionEnvironment, PathForVolumeId};
use models::{ErrorKind, InterfaceId, ProcedureName, VolumeId};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::util::{GeneralGuard, Version};
use crate::volume::Volumes;
use crate::{context::RpcContext, manager::manager_seed::ManagerSeed};

#[derive(Serialize, Deserialize, Clone)]
#[serde(rename_all = "kebab-case")]
struct JsError {
    message: String,
    stack: Option<String>,
}
impl std::fmt::Display for JsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.message.fmt(f)
    }
}
impl std::fmt::Debug for JsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)?;
        if let Some(stack) = &self.stack {
            write!(f, "\n{}", stack)?;
        }
        Ok(())
    }
}
impl std::error::Error for JsError {}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "kebab-case")]

enum ErrorValue {
    Error(JsError),
    Result(Value),
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
    ctx: RpcContext,
    package_id: PackageId,
}
#[async_trait::async_trait]
impl OsApi for SandboxOsApi {
    #[allow(unused_variables)]
    async fn get_service_config(
        &self,
        id: Option<PackageId>,
        path: Option<&str>,
        callback: Option<Callback>,
    ) -> Result<Vec<Value>, Report> {
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
    async fn get_service_local_address(
        &self,
        package_id: PackageId,
        interface_name: &str,
    ) -> Result<String, Report> {
        let db = self.seed.ctx.db.peek().await?;
        get_service_local_address(db, &package_id, interface_name)
    }
    async fn get_service_tor_address(
        &self,
        pcakge_id: PackageId,
        interface_name: &str,
    ) -> Result<String, Report> {
        let db = self.seed.ctx.db.peek().await?;
        get_service_tor_address(db, &package_id, interface_name)
    }
    async fn get_service_port_forward(
        &self,
        pcakge_id: PackageId,
        interface_name: &str,
    ) -> Result<String, Report> {
        todo!()
    }
    async fn export_address(
        &self,
        name: String,
        description: String,
        addresses: Vec<String>,
        id: String,
        ui: bool,
    ) -> Result<(), Report> {
        todo!()
    }
    async fn remove_address(&self, id: String) -> Result<(), Report> {
        Err(eyre!("Operation not permitted"))
    }
    async fn export_action(
        &self,
        name: String,
        description: String,
        id: String,
        input: Value,
        group: Option<String>,
        warning: Option<String>,
    ) -> Result<(), Report> {
        Err(eyre!("Operation not permitted"))
    }
    async fn remove_action(&self, id: String) -> Result<(), Report> {
        Err(eyre!("Operation not permitted"))
    }
    async fn get_configured(&self) -> Result<bool, Report> {
        todo!()
    }
    async fn set_configured(&self, configured: bool) -> Result<(), Report> {
        Err(eyre!("Operation not permitted"))
    }
    async fn get_ssl_certificate(
        &self,
        id: String,
        algorithm: Algorithm,
    ) -> Result<(String, String, String), Report> {
        todo!()
    }
    async fn get_ssl_key(&self, id: String, algorithm: Algorithm) -> Result<String, Report> {
        todo!()
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub struct JsProcedure;

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
    ) -> Result<O, Error> {
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
            .run_action(name, input);
            let output: Option<ErrorValue> = match timeout {
                Some(timeout_duration) => tokio::time::timeout(timeout_duration, running_action)
                    .await
                    .map_err(|_| {
                        (
                            JsEngineError::Timeout,
                            "Timed out. Retrying soon...".to_owned(),
                        )
                    })??,
                None => running_action.await?,
            };
            Ok(output)
        }
        .await
        .map_err(|(kind, message)| Error::new(eyre!("{kind:?}: {message}"), ErrorKind::Javascript));
        cleaner.drop().await.unwrap()?;
        unwrap_known_error(res?)
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
    ) -> Result<O, Error> {
        let res = async move {
            let running_action = JsExecutionEnvironment::load_from_package(
                Arc::new(SandboxOsApi {
                    ctx: ctx.clone(),
                    package_id: pkg_id.clone(),
                }),
                &ctx.datadir,
                pkg_id,
                pkg_version,
                Box::new(volumes.clone()),
                ProcessGroupId(0),
                None,
            )
            .await?
            .read_only_effects()
            .run_action(name, input);
            let output: Option<ErrorValue> = match timeout {
                Some(timeout_duration) => tokio::time::timeout(timeout_duration, running_action)
                    .await
                    .map_err(|_| {
                        (
                            JsEngineError::Timeout,
                            "Timed out. Retrying soon...".to_owned(),
                        )
                    })??,
                None => running_action.await?,
            };
            Ok(output)
        }
        .await
        .map_err(|(kind, message)| Error::new(eyre!("{kind:?}: {message}"), ErrorKind::Javascript));
        unwrap_known_error(res?)
    }
}

fn unwrap_known_error<O: DeserializeOwned>(error_value: Option<ErrorValue>) -> Result<O, Error> {
    let error_value = error_value.unwrap_or_else(|| ErrorValue::Result(Value::Null));
    match error_value {
        ErrorValue::Error(error) => Err(error).with_kind(ErrorKind::Javascript),
        ErrorValue::Result(ref value) => match from_value(value.clone()) {
            Ok(a) => Ok(a),
            Err(err) => {
                tracing::error!("{}", err);
                tracing::debug!("{:?}", err);
                Err((
                    JsEngineError::BoundryLayerSerDe,
                    format!(
                        "Couldn't convert output = {:#?} to the correct type",
                        serde_json::to_string_pretty(&error_value).unwrap_or_default()
                    ),
                ))
                .map_err(|(kind, message)| {
                    Error::new(eyre!("{kind:?}: {message}"), ErrorKind::Javascript)
                })
            }
        },
    }
}

#[derive(Debug)]
pub struct NoOutput;
impl<'de> Deserialize<'de> for NoOutput {
    fn deserialize<D>(_: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(NoOutput)
    }
}

#[cfg(test)]
mod tests {
    use helpers::{Address, AddressSchemaLocal, AddressSchemaOnion, Callback, OsApi};
    use serde_json::{json, Value};
    use tokio::sync::watch;

    use super::*;

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
            id: Option<PackageId>,
            path: Option<&str>,
            callback: Option<Callback>,
        ) -> Result<Vec<Value>, Report> {
            if let Some(callback) = callback {
                println!("Adding callback");
                self.config_callbacks.send_modify(|x| x.push(callback));
            }
            Ok(vec![Value::Null])
        }
        #[allow(unused_variables)]
        async fn bind_local(
            &self,
            internal_port: u16,
            address_schema: AddressSchemaLocal,
        ) -> Result<Address, Report> {
            unimplemented!()
        }
        #[allow(unused_variables)]
        async fn bind_onion(
            &self,
            internal_port: u16,
            address_schema: AddressSchemaOnion,
        ) -> Result<Address, Report> {
            unimplemented!()
        }
        #[allow(unused_variables)]
        async fn unbind_local(&self, id: InterfaceId, external: u16) -> Result<(), Report> {
            unimplemented!()
        }
        #[allow(unused_variables)]
        async fn unbind_onion(&self, id: InterfaceId, external: u16) -> Result<(), Report> {
            unimplemented!()
        }
        fn set_started(&self) -> Result<(), Report> {
            unimplemented!()
        }
        async fn restart(&self) -> Result<(), Report> {
            unimplemented!()
        }
        async fn start(&self) -> Result<(), Report> {
            unimplemented!()
        }
        async fn stop(&self) -> Result<(), Report> {
            unimplemented!()
        }
        async fn get_service_local_address(
            &self,
            pcakge_id: PackageId,
            interface_name: &str,
        ) -> Result<String, Report> {
            unimplemented!()
        }
        async fn get_service_tor_address(
            &self,
            pcakge_id: PackageId,
            interface_name: &str,
        ) -> Result<String, Report> {
            unimplemented!()
        }
        async fn get_service_port_forward(
            &self,
            pcakge_id: PackageId,
            interface_name: &str,
        ) -> Result<String, Report> {
            unimplemented!()
        }
        async fn export_address(
            &self,
            name: String,
            description: String,
            addresses: Vec<String>,
            id: String,
            ui: bool,
        ) -> Result<(), Report> {
            unimplemented!()
        }
        async fn remove_address(&self, id: String) -> Result<(), Report> {
            unimplemented!()
        }
        async fn export_action(
            &self,
            name: String,
            description: String,
            id: String,
            input: Value,
            group: Option<String>,
            warning: Option<String>,
        ) -> Result<(), Report> {
            unimplemented!()
        }
        async fn remove_action(&self, id: String) -> Result<(), Report> {
            unimplemented!()
        }
        async fn get_configured(&self) -> Result<bool, Report> {
            unimplemented!()
        }
        async fn set_configured(&self, configured: bool) -> Result<(), Report> {
            unimplemented!()
        }
        async fn get_ssl_certificate(
            &self,
            id: String,
            algorithm: Algorithm,
        ) -> Result<(String, String, String), Report> {
            unimplemented!()
        }
        async fn get_ssl_key(&self, id: String, algorithm: Algorithm) -> Result<String, Report> {
            unimplemented!()
        }
    }
    #[tokio::test]
    async fn js_action_execute() {
        let js_action = JsProcedure;
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
        let js_action = JsProcedure;
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
        let output: Result<Value, _> = js_action
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
        let js_action = JsProcedure;
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
            .unwrap();
    }

    #[tokio::test]
    async fn js_test_slow() {
        let js_action = JsProcedure;
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
                ) => { a.unwrap(); },
            _ = tokio::time::sleep(Duration::from_secs(1)) => ()
        }
        tracing::debug!("testing end should");
        tokio::time::sleep(Duration::from_secs(2)).await;
        tracing::debug!("Done");
    }

    #[tokio::test]
    async fn js_action_test_rename() {
        let js_action = JsProcedure;
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
                Arc::new(OsApiMock::default()),
            )
            .await
            .unwrap();
    }
    #[tokio::test]
    async fn js_action_test_deep_dir_escape() {
        let js_action = JsProcedure;
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
                Arc::new(OsApiMock::default()),
            )
            .await
            .unwrap();
    }
    #[tokio::test]
    async fn js_action_test_zero_dir() {
        let js_action = JsProcedure;
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
                Arc::new(OsApiMock::default()),
            )
            .await
            .unwrap();
    }
    #[tokio::test]
    async fn js_action_test_read_dir() {
        let js_action = JsProcedure;
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
                Arc::new(OsApiMock::default()),
            )
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn js_action_test_deep_dir() {
        let js_action = JsProcedure;
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
            .unwrap();
    }
    #[tokio::test]
    async fn js_permissions_and_own() {
        let js_action = JsProcedure;
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
            .unwrap();
    }

    #[tokio::test]
    async fn js_rsync() {
        let js_action = JsProcedure;
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
                    x[0].call(vec![json!("This is something across the wire!")])
                        .map_err(|e| format!("Failed call"))
                        .unwrap();
                });
                break;
            }
        });
        let js_action = JsProcedure;
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
            .unwrap();
        spawned.await.unwrap();
    }
}
