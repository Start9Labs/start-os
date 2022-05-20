use std::{path::PathBuf, time::Duration};

use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::{
    context::RpcContext, s9pk::manifest::PackageId, util::Version, volume::Volumes, Error,
};

use self::js_runtime::JsExecutionEnvironment;

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
            let running_action = JsExecutionEnvironment::load_from_package(
                directory,
                pkg_id,
                pkg_version,
                volumes.clone(),
            )
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
            let running_action = JsExecutionEnvironment::load_from_package(
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
    use deno_core::anyhow::{anyhow, bail};
    use deno_core::error::AnyError;
    use deno_core::resolve_import;
    use deno_core::JsRuntime;
    use deno_core::ModuleLoader;
    use deno_core::ModuleSource;
    use deno_core::ModuleSourceFuture;
    use deno_core::ModuleSpecifier;
    use deno_core::ModuleType;
    use deno_core::RuntimeOptions;
    use deno_core::{Extension, OpDecl};
    use serde::{Deserialize, Serialize};
    use serde_json::Value;
    use std::sync::Arc;
    use std::{path::PathBuf, pin::Pin};
    use tokio::io::AsyncReadExt;

    use crate::s9pk::manifest::PackageId;
    use crate::util::Version;
    use crate::volume::{script_dir, Volumes};

    use super::super::ProcedureName;
    use super::{JsCode, JsError};

    #[derive(Clone, Deserialize, Serialize)]
    struct JsContext {
        sandboxed: bool,
        datadir: PathBuf,
        run_function: String,
        version: Version,
        package_id: PackageId,
        volumes: Arc<Volumes>,
        input: Value,
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
            if referrer.contains("embassy") {
                bail!("Embassy.js cannot import anything else");
            }
            let s = resolve_import(specifier, referrer).unwrap();
            Ok(s)
        }

        fn load(
            &self,
            module_specifier: &ModuleSpecifier,
            maybe_referrer: Option<ModuleSpecifier>,
            is_dyn_import: bool,
        ) -> Pin<Box<ModuleSourceFuture>> {
            let module_specifier = module_specifier.as_str().to_owned();
            let module = match &*module_specifier {
                "file:///deno_global.js" => Ok(ModuleSource {
                    module_url_specified: "file:///deno_global.js".to_string(),
                    module_url_found: "file:///deno_global.js".to_string(),
                    code: "const old_deno = Deno; Deno = null; export default old_deno"
                        .as_bytes()
                        .to_vec()
                        .into_boxed_slice(),
                    module_type: ModuleType::JavaScript,
                }),
                "file:///loadModule.js" => Ok(ModuleSource {
                    module_url_specified: "file:///loadModule.js".to_string(),
                    module_url_found: "file:///loadModule.js".to_string(),
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
                x => Err(anyhow!("Not allowed to import: {}", x)),
            };
            Box::pin(async move {
                if is_dyn_import {
                    bail!("Will not import dynamic");
                }
                match &maybe_referrer {
                    Some(x) if x.as_str() == "file:///embassy.js" => {
                        bail!("Embassy is not allowed to import")
                    }
                    _ => (),
                }
                module
            })
        }
    }
    pub struct JsExecutionEnvironment {
        sandboxed: bool,
        base_directory: PathBuf,
        module_loader: ModsLoader,
        package_id: PackageId,
        version: Version,
        operations: Vec<OpDecl>,
        volumes: Arc<Volumes>,
    }

    impl JsExecutionEnvironment {
        pub async fn load_from_package(
            data_directory: impl AsRef<std::path::Path>,
            package_id: &crate::s9pk::manifest::PackageId,
            version: &crate::util::Version,
            volumes: Volumes,
        ) -> Result<Self, (JsError, String)> {
            let data_dir = data_directory.as_ref();
            let base_directory = data_dir;
            let js_code = JsCode({
                let file_path = script_dir(data_dir, package_id, version).join("embassy.js");
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
            Ok(Self {
                base_directory: base_directory.to_owned(),
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
                fns::create_dir::decl(),
                fns::remove_dir::decl(),
                fns::get_context::decl(),
                fns::log_trace::decl(),
                fns::log_warn::decl(),
                fns::log_error::decl(),
                fns::log_debug::decl(),
                fns::log_info::decl(),
                fns::current_function::decl(),
                fns::set_value::decl(),
                fns::remove_file::decl(),
                fns::is_sandboxed::decl(),
                fns::get_input::decl(),
            ];
            self
        }
        pub async fn run_action<I: Serialize, O: for<'de> Deserialize<'de>>(
            self,
            procedure_name: ProcedureName,
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
            let safer_handle: crate::util::NonDetachingJoinHandle<_> =
                tokio::task::spawn_blocking(move || self.execute(procedure_name, input)).into();
            let output = safer_handle
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
            procedure_name: ProcedureName,
            input: Value,
        ) -> Result<Value, (JsError, String)> {
            let base_directory = self.base_directory.clone();
            let answer_state = AnswerState::default();
            let ext_answer_state = answer_state.clone();
            let js_ctx = JsContext {
                datadir: base_directory,
                run_function: procedure_name.js_function_name(),
                package_id: self.package_id.clone(),
                volumes: self.volumes.clone(),
                version: self.version.clone(),
                sandboxed: self.sandboxed,
                input,
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
                    .load_main_module(&"file:///loadModule.js".parse().unwrap(), None)
                    .await?;
                let evaluated = runtime.mod_evaluate(mod_id);
                let res = runtime.run_event_loop(false).await;
                evaluated.await??;
                res?;
                Ok::<_, AnyError>(())
            };

            tokio::runtime::Handle::current()
                .block_on(future)
                .map_err(|e| {
                    tracing::debug!("{:?}", e);
                    (JsError::Javascript, format!("Execution error: {}", e))
                })?;

            let answer = answer_state.0.lock().clone();
            Ok(answer)
        }
    }

    /// Note: Make sure that we have the assumption that all these methods are callable at any time, and all call restrictions should be in rust
    mod fns {
        use deno_core::{anyhow::bail, error::AnyError, *};
        use serde_json::Value;

        use std::{convert::TryFrom, path::PathBuf};

        use crate::volume::VolumeId;

        use super::{AnswerState, JsContext};

        #[op]
        async fn read_file(
            ctx: JsContext,
            volume_id: VolumeId,
            path_in: PathBuf,
        ) -> Result<String, AnyError> {
            let volume = match ctx.volumes.get(&volume_id) {
                Some(a) => a,
                None => {
                    bail!("There is no {} in volumes", volume_id);
                }
            };
            let volume_path =
                volume.path_for(&ctx.datadir, &ctx.package_id, &ctx.version, &volume_id);
            //get_path_for in volume.rs
            let new_file = volume_path.join(path_in);
            if !new_file.starts_with(volume_path) {
                bail!("Path has broken away from parent");
            }
            let answer = tokio::fs::read_to_string(new_file).await?;
            Ok(answer)
        }
        #[op]
        async fn write_file(
            ctx: JsContext,
            volume_id: VolumeId,
            path_in: PathBuf,
            write: String,
        ) -> Result<(), AnyError> {
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
            let new_file = volume_path.join(path_in);
            // With the volume check
            if !new_file.starts_with(volume_path) {
                bail!("Path has broken away from parent");
            }
            tokio::fs::write(new_file, write).await?;
            Ok(())
        }
        #[op]
        async fn remove_file(
            ctx: JsContext,
            volume_id: VolumeId,
            path_in: PathBuf,
        ) -> Result<(), AnyError> {
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
            let new_file = volume_path.clone().join(path_in);
            // With the volume check
            if !new_file.starts_with(volume_path) {
                bail!("Path has broken away from parent");
            }
            tokio::fs::remove_file(new_file).await?;
            Ok(())
        }
        #[op]
        async fn remove_dir(
            ctx: JsContext,
            volume_id: VolumeId,
            path_in: PathBuf,
        ) -> Result<(), AnyError> {
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
            let new_file = volume_path.clone().join(path_in);
            // With the volume check
            if !new_file.starts_with(volume_path) {
                bail!("Path has broken away from parent");
            }
            tokio::fs::remove_dir_all(new_file).await?;
            Ok(())
        }
        #[op]
        async fn create_dir(
            ctx: JsContext,
            volume_id: VolumeId,
            path_in: PathBuf,
        ) -> Result<(), AnyError> {
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
            let new_file = volume_path.clone().join(path_in);
            // With the volume check
            if !new_file.starts_with(volume_path) {
                bail!("Path has broken away from parent");
            }
            tokio::fs::create_dir_all(new_file).await?;
            Ok(())
        }

        #[op]
        fn current_function(state: &mut OpState) -> Result<String, AnyError> {
            let ctx = state.borrow::<JsContext>();
            Ok(ctx.run_function.clone())
        }

        #[op]
        fn log_trace(state: &mut OpState, input: String) -> Result<(), AnyError> {
            let ctx = state.borrow::<JsContext>();
            tracing::trace!(
                package_id = tracing::field::display(&ctx.package_id),
                run_function = tracing::field::display(&ctx.run_function),
                "{}",
                input
            );
            Ok(())
        }
        #[op]
        fn log_warn(state: &mut OpState, input: String) -> Result<(), AnyError> {
            let ctx = state.borrow::<JsContext>();
            tracing::warn!(
                package_id = tracing::field::display(&ctx.package_id),
                run_function = tracing::field::display(&ctx.run_function),
                "{}",
                input
            );
            Ok(())
        }
        #[op]
        fn log_error(state: &mut OpState, input: String) -> Result<(), AnyError> {
            let ctx = state.borrow::<JsContext>();
            tracing::error!(
                package_id = tracing::field::display(&ctx.package_id),
                run_function = tracing::field::display(&ctx.run_function),
                "{}",
                input
            );
            Ok(())
        }
        #[op]
        fn log_debug(state: &mut OpState, input: String) -> Result<(), AnyError> {
            let ctx = state.borrow::<JsContext>();
            tracing::debug!(
                package_id = tracing::field::display(&ctx.package_id),
                run_function = tracing::field::display(&ctx.run_function),
                "{}",
                input
            );
            Ok(())
        }
        #[op]
        fn log_info(state: &mut OpState, input: String) -> Result<(), AnyError> {
            let ctx = state.borrow::<JsContext>();
            tracing::info!(
                package_id = tracing::field::display(&ctx.package_id),
                run_function = tracing::field::display(&ctx.run_function),
                "{}",
                input
            );
            Ok(())
        }

        #[op]
        fn get_context(state: &mut OpState) -> Result<JsContext, AnyError> {
            let ctx = state.borrow::<JsContext>();
            Ok(ctx.clone())
        }
        #[op]
        fn get_input(state: &mut OpState) -> Result<Value, AnyError> {
            let ctx = state.borrow::<JsContext>();
            Ok(ctx.input.clone())
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
    let input: Option<serde_json::Value> = Some(serde_json::json!({"test":123}));
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
