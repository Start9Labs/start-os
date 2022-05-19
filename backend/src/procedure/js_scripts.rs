use std::{path::PathBuf, time::Duration};

use futures::future::Either as EitherFuture;
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use tokio::io::AsyncReadExt;
use tracing::instrument;

use crate::{
    context::RpcContext, install::PKG_SCRIPT_DIR, s9pk::manifest::PackageId, util::Version,
    volume::Volumes, Error,
};

use self::js_runtime::JsExecutionBuilder;

use super::ProcedureName;

#[derive(Serialize, Deserialize, Debug, Default, Clone)]
pub struct JsCode(String);

#[derive(Debug, Clone, Copy)]
pub enum JsError {
    Unknown = 1,
    Javascript = 2,
    Engine = 3,
    BoundryLayerSerDe = 4,
    Tokio = 5,
    FileSystem = 6,
    Timeout = 143,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct JsProcedure {}

impl JsProcedure {
    pub fn validate(&self, volumes: &Volumes) -> Result<(), color_eyre::eyre::Report> {
        Ok(())
    }

    #[instrument(skip(directory, input))]
    pub async fn execute<I: Serialize, O: for<'de> Deserialize<'de>>(
        &self,
        directory: &PathBuf,
        pkg_id: &PackageId,
        pkg_version: &Version,
        name: ProcedureName,
        volumes: &Volumes,
        input: Option<I>,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error> {
        Ok(async move {
            let running_action =
                JsExecutionBuilder::load_js_file(directory, pkg_id, pkg_version, volumes.clone())
                    .await?
                    .with_effects()
                    .run_action(name, input);
            let output: O = match timeout {
                Some(timeout_duration) => tokio::time::timeout(timeout_duration, running_action)
                    .await
                    .map_err(|_| (JsError::Timeout, "Timed out. Retrying soon...".to_owned()))??,
                None => running_action.await?,
            };
            Ok(output)
        }
        .await
        .map_err(|(error, message)| (error as i32, message)))
    }

    #[instrument(skip(ctx, input))]
    pub async fn sandboxed<I: Serialize, O: for<'de> Deserialize<'de>>(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
        input: Option<I>,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error> {
        Ok(async move {
            let running_action = JsExecutionBuilder::load_js_file(
                &ctx.datadir,
                pkg_id,
                pkg_version,
                volumes.clone(),
            )
            .await?
            .read_only_effects()
            .run_action(ProcedureName::GetConfig, input);
            let output: O = match timeout {
                Some(timeout_duration) => tokio::time::timeout(timeout_duration, running_action)
                    .await
                    .map_err(|_| (JsError::Timeout, "Timed out. Retrying soon...".to_owned()))??,
                None => running_action.await?,
            };
            Ok(output)
        }
        .await
        .map_err(|(error, message)| (error as i32, message)))
    }
}

mod js_runtime {
    use deno_ast::MediaType;
    use deno_ast::ParseParams;
    use deno_ast::SourceTextInfo;
    use deno_core::anyhow::{anyhow, bail};
    use deno_core::resolve_import;
    use deno_core::resolve_path;
    use deno_core::JsRuntime;
    use deno_core::ModuleLoader;
    use deno_core::ModuleSource;
    use deno_core::ModuleSourceFuture;
    use deno_core::ModuleSpecifier;
    use deno_core::ModuleType;
    use deno_core::RuntimeOptions;
    use deno_core::{error::AnyError, serde_v8, v8};
    use deno_core::{Extension, OpDecl};
    use futures::FutureExt;
    use serde::{Deserialize, Serialize};
    use serde_json::Value;
    use std::sync::Arc;
    use std::{convert::TryFrom, path::PathBuf, pin::Pin};
    use tokio::io::AsyncReadExt;

    use crate::s9pk::manifest::PackageId;
    use crate::util::Version;
    use crate::volume::VolumeId;
    use crate::volume::Volumes;

    use super::super::ProcedureName;
    use super::{JsCode, JsError};

    pub struct JsFunctionInformation {
        action_name: ProcedureName,
        input: Value,
    }

    #[derive(Clone)]
    struct JsContext {
        sandboxed: bool,
        datadir: PathBuf,
        run_function: String,
        version: Version,
        package_id: PackageId,
        volumes: Arc<Volumes>,
    }

    #[derive(Clone, Default)]
    struct AnswerState(std::sync::Arc<deno_core::parking_lot::Mutex<Value>>);

    #[derive(Clone, Debug)]
    struct ModsLoader {
        code: JsCode,
    }

    impl ModuleLoader for ModsLoader {
        fn resolve(
            &self,
            specifier: &str,
            referrer: &str,
            _is_main: bool,
        ) -> Result<ModuleSpecifier, AnyError> {
            let s = resolve_import(specifier, referrer).unwrap();
            Ok(s)
        }

        fn load(
            &self,
            module_specifier: &ModuleSpecifier,
            _maybe_referrer: Option<ModuleSpecifier>,
            _is_dyn_import: bool,
        ) -> Pin<Box<ModuleSourceFuture>> {
            let module_source = match module_specifier.as_str() {
                "file:///main.js" => Ok(ModuleSource {
                    module_url_specified: "file:///main_module.js".to_string(),
                    module_url_found: "file:///main_module.js".to_string(),
                    code: include_str!("./js_scripts/loadModule.js")
                        .as_bytes()
                        .to_vec()
                        .into_boxed_slice(),
                    module_type: ModuleType::JavaScript,
                }),
                "file:///embassy.js" => Ok(ModuleSource {
                    module_url_specified: "file:///embassy.js".to_string(),
                    module_url_found: "file:///embassy.js".to_string(),
                    code: self.code.0.as_bytes().to_vec().into_boxed_slice(),
                    module_type: ModuleType::JavaScript,
                }),
                _ => unreachable!(),
            };
            async move { module_source }.boxed()
        }
    }
    pub struct JsExecutionBuilder {
        sandboxed: bool,
        base_directory: PathBuf,
        module_loader: ModsLoader,
        package_id: PackageId,
        version: Version,
        operations: Vec<OpDecl>,
        volumes: Arc<Volumes>,
    }

    impl JsExecutionBuilder {
        pub async fn load_js_file(
            path: &std::path::PathBuf,
            package_id: &crate::s9pk::manifest::PackageId,
            version: &crate::util::Version,
            volumes: Volumes,
        ) -> Result<Self, (JsError, String)> {
            let base_directory = path.clone();
            let js_code = JsCode({
                let file_path = path
                    .clone()
                    .join(&*crate::install::PKG_SCRIPT_DIR)
                    .join(&package_id)
                    .join(version.to_string())
                    .join("embassy.js");
                let mut file = match tokio::fs::File::open(file_path.clone()).await {
                    Ok(x) => x,
                    Err(e) => {
                        tracing::debug!("{:?}", e);
                        return Err((
                            JsError::FileSystem,
                            format!("The file opening '{:?}' created error: {}", file_path, e),
                        ));
                    }
                };
                let mut buffer = Default::default();
                if let Err(err) = file.read_to_string(&mut buffer).await {
                    tracing::debug!("{:?}", err);
                    return Err((
                        JsError::FileSystem,
                        format!("The file reading created error: {}", err),
                    ));
                };
                buffer
            });
            println!("Volumes: {:?}", volumes);
            Ok(Self {
                base_directory,
                module_loader: ModsLoader { code: js_code },
                operations: Default::default(),
                package_id: package_id.clone(),
                version: version.clone(),
                volumes: Arc::new(volumes),
                sandboxed: false,
            })
        }
        pub fn read_only_effects(mut self) -> Self {
            self.sandboxed = true;
            self.with_effects()
        }

        pub fn with_effects(mut self) -> Self {
            self.operations = vec![
                fns::read_file::decl(),
                fns::write_file::decl(),
                fns::println::decl(),
                fns::current_function::decl(),
                fns::set_value::decl(),
                fns::remove_file::decl(),
                fns::is_sandboxed::decl(),
            ];
            self
        }
        pub async fn run_action<I: Serialize, O: for<'de> Deserialize<'de>>(
            self,
            action_name: ProcedureName,
            input: Option<I>,
        ) -> Result<O, (JsError, String)> {
            let input = match serde_json::to_value(input) {
                Ok(a) => a,
                Err(err) => {
                    tracing::error!("{}", err);
                    tracing::debug!("{:?}", err);
                    return Err((
                        JsError::BoundryLayerSerDe,
                        "Couldn't convert input".to_string(),
                    ));
                }
            };
            let js_function_information = JsFunctionInformation { action_name, input };
            let output = tokio::task::spawn_blocking(move || self.execute(js_function_information))
                .await
                .map_err(|err| (JsError::Tokio, format!("Tokio gave us the error: {}", err)))??;
            match serde_json::from_value(output.clone()) {
                Ok(x) => Ok(x),
                Err(err) => {
                    tracing::error!("{}", err);
                    tracing::debug!("{:?}", err);
                    return Err((
                        JsError::BoundryLayerSerDe,
                        format!(
                            "Couldn't convert output = {:#?} to the correct type",
                            serde_json::to_string_pretty(&output).unwrap_or_default()
                        ),
                    ));
                }
            }
        }

        fn execute(
            &self,
            js_function_information: JsFunctionInformation,
        ) -> Result<Value, (JsError, String)> {
            let base_directory = self.base_directory.clone();
            let answer_state = AnswerState::default();
            let ext_answer_state = answer_state.clone();
            let js_ctx = JsContext {
                datadir: base_directory,
                run_function: js_function_information.action_name.js_function_name(),
                package_id: self.package_id.clone(),
                volumes: self.volumes.clone(),
                version: self.version.clone(),
                sandboxed: self.sandboxed,
            };
            let ext = Extension::builder()
                .ops(self.operations.clone())
                .state(move |state| {
                    state.put(ext_answer_state.clone());
                    state.put(js_ctx.clone());
                    Ok(())
                })
                .build();

            let loader = std::rc::Rc::new(self.module_loader.clone());
            let mut runtime = JsRuntime::new(RuntimeOptions {
                module_loader: Some(loader),
                extensions: vec![ext],
                ..Default::default()
            });

            let future = async move {
                let mod_id = runtime
                    .load_main_module(&"file:///main.js".parse().unwrap(), None)
                    .await?;
                let loaded = runtime.mod_evaluate(mod_id);
                runtime.run_event_loop(false).await?;
                Ok::<_, AnyError>(())
            };

            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap()
                .block_on(future)
                .map_err(|e| {
                    tracing::debug!("{:?}", e);
                    (JsError::Javascript, format!("Execution error: {}", e))
                })?;

            let answer = answer_state.0.lock().clone();
            Ok(answer)
        }

        fn run_js_function(
            &self,
            action_name: &ProcedureName,
            scope: &mut v8::HandleScope,
            scope_function: v8::Local<v8::Function>,
            input: &Value,
            local: v8::Local<v8::Value>,
        ) -> Result<Value, (JsError, String)> {
            let function_name = action_name.js_function_name();
            let args = serde_v8::to_v8(scope, input).map_err(|e| {
                tracing::warn!("JsError: {}", e);
                tracing::debug!("{:?}", e);
                (
                    JsError::BoundryLayerSerDe,
                    "Js <-> Rust Args boundry serialization issue".to_string(),
                )
            })?;
            let function_name = serde_v8::to_v8(scope, function_name).map_err(|e| {
                tracing::warn!("JsError: {}", e);
                tracing::debug!("{:?}", e);
                (
                    JsError::BoundryLayerSerDe,
                    "Js <-> Rust Args boundry serialization issue".to_string(),
                )
            })?;
            let output = match scope_function.call(scope, local, &[function_name, args]) {
                Some(x) => x,
                None => return Err((JsError::Javascript, "Javascript runtime error".to_string())),
            };
            serde_v8::from_v8::<Value>(scope, output).map_err(|e| {
                tracing::warn!("JsError: {}", e);
                tracing::debug!("{:?}", e);
                (
                    JsError::BoundryLayerSerDe,
                    "Js <-> Rust boundry serialization issue".to_string(),
                )
            })
        }
    }

    mod fns {
        use deno_core::{anyhow::bail, error::AnyError, *};
        use serde_json::Value;

        use std::fs;
        use std::{convert::TryFrom, path::PathBuf};

        use crate::volume::VolumeId;

        use super::{AnswerState, JsContext};

        #[op]
        fn read_file(
            state: &mut OpState,
            volume_id: VolumeId,
            path_in: PathBuf,
        ) -> Result<String, AnyError> {
            let ctx = state.borrow::<JsContext>();
            let volume = match ctx.volumes.get(&volume_id) {
                Some(a) => a,
                None => {
                    bail!("There is no {} in volumes", volume_id);
                }
            };
            let volume_path =
                volume.path_for(&ctx.datadir, &ctx.package_id, &ctx.version, &volume_id);
            //get_path_for in volume.rs
            let new_file = dbg!(volume_path.clone().join(path_in));
            if !new_file.starts_with(volume_path) {
                bail!("Path has broken away from parent");
            }
            let answer = fs::read_to_string(new_file)?;
            Ok(answer)
        }
        #[op]
        fn write_file(
            state: &mut OpState,
            volume_id: VolumeId,
            path_in: PathBuf,
            write: String,
        ) -> Result<(), AnyError> {
            let ctx = state.borrow::<JsContext>();
            if ctx.sandboxed {
                bail!("Cannot write in sandbox mode");
            }
            let volume = match ctx.volumes.get(&volume_id) {
                Some(a) => a,
                None => {
                    bail!("There is no {} in volumes", volume_id);
                }
            };
            if volume.readonly() {
                bail!("Volume {} is readonly", volume_id);
            }
            let volume_path =
                volume.path_for(&ctx.datadir, &ctx.package_id, &ctx.version, &volume_id);
            println!("Here is the voume path: {:?}", volume_path);
            let new_file = dbg!(volume_path.clone().join(path_in));
            // With the volume check
            if !new_file.starts_with(volume_path) {
                bail!("Path has broken away from parent");
            }
            fs::write(new_file, write)?;
            Ok(())
        }
        #[op]
        fn remove_file(
            state: &mut OpState,
            volume_id: VolumeId,
            path_in: PathBuf,
        ) -> Result<(), AnyError> {
            let ctx = state.borrow::<JsContext>();
            if ctx.sandboxed {
                bail!("Cannot write in sandbox mode");
            }
            let volume = match ctx.volumes.get(&volume_id) {
                Some(a) => a,
                None => {
                    bail!("There is no {} in volumes", volume_id);
                }
            };
            if volume.readonly() {
                bail!("Volume {} is readonly", volume_id);
            }
            let volume_path =
                volume.path_for(&ctx.datadir, &ctx.package_id, &ctx.version, &volume_id);
            println!("Here is the voume path: {:?}", volume_path);
            let new_file = dbg!(volume_path.clone().join(path_in));
            // With the volume check
            if !new_file.starts_with(volume_path) {
                bail!("Path has broken away from parent");
            }
            fs::remove_file(new_file)?;
            Ok(())
        }

        #[op]
        fn current_function(state: &mut OpState) -> Result<String, AnyError> {
            let ctx = state.borrow::<JsContext>();
            Ok(ctx.run_function.clone())
        }

        #[op]
        fn println(input: String) -> Result<(), AnyError> {
            println!("{}", input);
            Ok(())
        }
        #[op]
        fn set_value(state: &mut OpState, value: Value) -> Result<(), AnyError> {
            let mut answer = state.borrow::<AnswerState>().0.lock();
            *answer = value;
            Ok(())
        }
        #[op]
        fn is_sandboxed(state: &mut OpState) -> Result<bool, AnyError> {
            let ctx = state.borrow::<JsContext>();
            Ok(ctx.sandboxed)
        }
    }
}
#[tokio::test]
async fn js_action_execute() {
    let js_action = JsProcedure {};
    let path: PathBuf = "test/js_action_execute/".parse().unwrap();
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
    let input: Option<String> = None;
    let timeout = None;
    let _output: crate::config::action::ConfigRes = js_action
        .execute(
            &path,
            &package_id,
            &package_version,
            name,
            &volumes,
            input,
            timeout,
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
