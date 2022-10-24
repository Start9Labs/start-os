use std::collections::BTreeMap;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::sync::Arc;
use std::time::SystemTime;

use deno_core::anyhow::{anyhow, bail};
use deno_core::error::AnyError;
use deno_core::{
    resolve_import, Extension, JsRuntime, ModuleLoader, ModuleSource, ModuleSourceFuture,
    ModuleSpecifier, ModuleType, OpDecl, RuntimeOptions, Snapshot,
};
use helpers::{script_dir, SingleThreadJoinHandle};
use models::{ExecCommand, PackageId, ProcedureName, TermCommand, Version, VolumeId};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::io::AsyncReadExt;
use tokio::sync::Mutex;

pub trait PathForVolumeId: Send + Sync {
    fn path_for(
        &self,
        data_dir: &Path,
        package_id: &PackageId,
        version: &Version,
        volume_id: &VolumeId,
    ) -> Option<PathBuf>;
    fn readonly(&self, volume_id: &VolumeId) -> bool;
}

#[derive(Serialize, Deserialize, Debug, Default, Clone)]
pub struct JsCode(String);

#[derive(Debug, Clone, Copy)]
pub enum JsError {
    Unknown,
    Javascript,
    Engine,
    BoundryLayerSerDe,
    Tokio,
    FileSystem,
    Code(i32),
    Timeout,
    NotValidProcedureName,
}

impl JsError {
    pub fn as_code_num(&self) -> i32 {
        match self {
            JsError::Unknown => 1,
            JsError::Javascript => 2,
            JsError::Engine => 3,
            JsError::BoundryLayerSerDe => 4,
            JsError::Tokio => 5,
            JsError::FileSystem => 6,
            JsError::NotValidProcedureName => 7,
            JsError::Code(code) => *code,
            JsError::Timeout => 143,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MetadataJs {
    file_type: String,
    is_dir: bool,
    is_file: bool,
    is_symlink: bool,
    len: u64,
    modified: Option<u64>,
    accessed: Option<u64>,
    created: Option<u64>,
    readonly: bool,
    gid: u32,
    mode: u32,
    uid: u32,
}

#[cfg(target_arch = "x86_64")]
const SNAPSHOT_BYTES: &[u8] = include_bytes!("./artifacts/JS_SNAPSHOT.bin");

#[cfg(target_arch = "aarch64")]
const SNAPSHOT_BYTES: &[u8] = include_bytes!("./artifacts/ARM_JS_SNAPSHOT.bin");
type WaitFns = Arc<Mutex<BTreeMap<u32, Pin<Box<dyn Future<Output = ResultType>>>>>>;

#[derive(Clone)]
struct JsContext {
    sandboxed: bool,
    datadir: PathBuf,
    run_function: String,
    version: Version,
    package_id: PackageId,
    volumes: Arc<dyn PathForVolumeId>,
    input: Value,
    variable_args: Vec<serde_json::Value>,
    command_inserter: ExecCommand,
    term_command: TermCommand,
    wait_fns: WaitFns,
}
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
enum ResultType {
    Error(String),
    ErrorCode(i32, String),
    Result(serde_json::Value),
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
                code: include_str!("./artifacts/loadModule.js")
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
    volumes: Arc<dyn PathForVolumeId>,
    command_inserter: ExecCommand,
    term_command: TermCommand,
}

impl JsExecutionEnvironment {
    pub async fn load_from_package(
        data_directory: impl AsRef<std::path::Path>,
        package_id: &PackageId,
        version: &Version,
        volumes: Box<dyn PathForVolumeId>,
        command_inserter: ExecCommand,
        term_command: TermCommand,
    ) -> Result<JsExecutionEnvironment, (JsError, String)> {
        let data_dir = data_directory.as_ref();
        let base_directory = data_dir;
        let js_code = JsCode({
            let file_path = script_dir(data_dir, package_id, version).join("embassy.js");
            let mut file = match tokio::fs::File::open(file_path.clone()).await {
                Ok(x) => x,
                Err(e) => {
                    tracing::debug!("path: {:?}", file_path);
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
        Ok(JsExecutionEnvironment {
            base_directory: base_directory.to_owned(),
            module_loader: ModsLoader { code: js_code },
            package_id: package_id.clone(),
            version: version.clone(),
            volumes: volumes.into(),
            sandboxed: false,
            command_inserter,
            term_command,
        })
    }
    pub fn read_only_effects(mut self) -> Self {
        self.sandboxed = true;
        self
    }

    pub async fn run_action<I: Serialize, O: for<'de> Deserialize<'de>>(
        self,
        procedure_name: ProcedureName,
        input: Option<I>,
        variable_args: Vec<serde_json::Value>,
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
        let safer_handle =
            SingleThreadJoinHandle::new(move || self.execute(procedure_name, input, variable_args));
        let output = safer_handle.await?;
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
    fn declarations() -> Vec<OpDecl> {
        vec![
            fns::fetch::decl(),
            fns::read_file::decl(),
            fns::metadata::decl(),
            fns::write_file::decl(),
            fns::rename::decl(),
            fns::remove_file::decl(),
            fns::create_dir::decl(),
            fns::remove_dir::decl(),
            fns::current_function::decl(),
            fns::log_trace::decl(),
            fns::log_warn::decl(),
            fns::log_error::decl(),
            fns::log_debug::decl(),
            fns::log_info::decl(),
            fns::get_input::decl(),
            fns::get_variable_args::decl(),
            fns::set_value::decl(),
            fns::is_sandboxed::decl(),
            fns::start_command::decl(),
            fns::wait_command::decl(),
            fns::sleep::decl(),
            fns::term_command::decl(),
        ]
    }

    async fn execute(
        self,
        procedure_name: ProcedureName,
        input: Value,
        variable_args: Vec<serde_json::Value>,
    ) -> Result<Value, (JsError, String)> {
        let base_directory = self.base_directory.clone();
        let answer_state = AnswerState::default();
        let ext_answer_state = answer_state.clone();
        let js_ctx = JsContext {
            datadir: base_directory,
            run_function: procedure_name
                .js_function_name()
                .map(Ok)
                .unwrap_or_else(|| {
                    Err((
                        JsError::NotValidProcedureName,
                        format!("procedure is not value: {:?}", procedure_name),
                    ))
                })?,
            package_id: self.package_id.clone(),
            volumes: self.volumes.clone(),
            version: self.version.clone(),
            sandboxed: self.sandboxed,
            input,
            variable_args,
            command_inserter: self.command_inserter.clone(),
            term_command: self.term_command.clone(),
            wait_fns: Default::default(),
        };
        let ext = Extension::builder()
            .ops(Self::declarations())
            .state(move |state| {
                state.put(ext_answer_state.clone());
                state.put(js_ctx.clone());
                Ok(())
            })
            .build();

        let loader = std::rc::Rc::new(self.module_loader.clone());
        let runtime_options = RuntimeOptions {
            module_loader: Some(loader),
            extensions: vec![ext],
            startup_snapshot: Some(Snapshot::Static(SNAPSHOT_BYTES)),
            ..Default::default()
        };
        let runtime = Arc::new(Mutex::new(JsRuntime::new(runtime_options)));

        let future = async move {
            let mod_id = runtime
                .lock()
                .await
                .load_main_module(&"file:///loadModule.js".parse().unwrap(), None)
                .await?;
            let evaluated = runtime.lock().await.mod_evaluate(mod_id);
            let res = runtime.lock().await.run_event_loop(false).await;
            res?;
            evaluated.await??;
            Ok::<_, AnyError>(())
        };

        future.await.map_err(|e| {
            tracing::debug!("{:?}", e);
            (JsError::Javascript, format!("{}", e))
        })?;

        let answer = answer_state.0.lock().clone();
        Ok(answer)
    }
}

/// Note: Make sure that we have the assumption that all these methods are callable at any time, and all call restrictions should be in rust
mod fns {
    use std::collections::BTreeMap;
    use std::convert::TryFrom;
    use std::os::unix::fs::MetadataExt;
    use std::path::{Path, PathBuf};
    use std::rc::Rc;
    use std::{cell::RefCell, time::Duration};

    use deno_core::anyhow::{anyhow, bail};
    use deno_core::error::AnyError;
    use deno_core::*;
    use embassy_container_init::RpcId;
    use helpers::{to_tmp_path, AtomicFile};
    use models::{TermCommand, VolumeId};
    use serde_json::Value;
    use tokio::io::AsyncWriteExt;

    use super::{AnswerState, JsContext};
    use crate::{system_time_as_unix_ms, MetadataJs, ResultType};

    #[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Default)]
    struct FetchOptions {
        method: Option<String>,
        headers: Option<BTreeMap<String, String>>,
        body: Option<String>,
    }
    #[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Default)]
    struct FetchResponse {
        method: String,
        ok: bool,
        status: u32,
        headers: BTreeMap<String, String>,
        body: Option<String>,
    }
    #[op]
    async fn fetch(
        state: Rc<RefCell<OpState>>,
        url: url::Url,
        options: Option<FetchOptions>,
    ) -> Result<FetchResponse, AnyError> {
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run fetch in sandboxed mode");
        }

        let client = reqwest::Client::new();
        let options = options.unwrap_or_default();
        let method = options
            .method
            .unwrap_or_else(|| "GET".to_string())
            .to_uppercase();
        let mut request_builder = match &*method {
            "GET" => client.get(url),
            "POST" => client.post(url),
            "PUT" => client.put(url),
            "DELETE" => client.delete(url),
            "HEAD" => client.head(url),
            "PATCH" => client.patch(url),
            x => bail!("Unsupported method: {}", x),
        };
        if let Some(headers) = options.headers {
            for (key, value) in headers {
                request_builder = request_builder.header(key, value);
            }
        }
        if let Some(body) = options.body {
            request_builder = request_builder.body(body);
        }
        let response = request_builder.send().await?;

        let fetch_response = FetchResponse {
            method,
            ok: response.status().is_success(),
            status: response.status().as_u16() as u32,
            headers: response
                .headers()
                .iter()
                .filter_map(|(head, value)| {
                    Some((format!("{}", head), value.to_str().ok()?.to_string()))
                })
                .collect(),
            body: response.text().await.ok(),
        };

        Ok(fetch_response)
    }

    #[op]
    async fn read_file(
        state: Rc<RefCell<OpState>>,
        volume_id: VolumeId,
        path_in: PathBuf,
    ) -> Result<String, AnyError> {
        let volume_path = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.volumes
                .path_for(&ctx.datadir, &ctx.package_id, &ctx.version, &volume_id)
                .ok_or_else(|| anyhow!("There is no {} in volumes", volume_id))?
        };
        //get_path_for in volume.rs
        let new_file = volume_path.join(path_in);
        if !is_subset(&volume_path, &new_file).await? {
            bail!(
                "Path '{}' has broken away from parent '{}'",
                new_file.to_string_lossy(),
                volume_path.to_string_lossy(),
            );
        }
        let answer = tokio::fs::read_to_string(new_file).await?;
        Ok(answer)
    }
    #[op]
    async fn metadata(
        state: Rc<RefCell<OpState>>,
        volume_id: VolumeId,
        path_in: PathBuf,
    ) -> Result<MetadataJs, AnyError> {
        let volume_path = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.volumes
                .path_for(&ctx.datadir, &ctx.package_id, &ctx.version, &volume_id)
                .ok_or_else(|| anyhow!("There is no {} in volumes", volume_id))?
        };
        //get_path_for in volume.rs
        let new_file = volume_path.join(path_in);
        if !is_subset(&volume_path, &new_file).await? {
            bail!(
                "Path '{}' has broken away from parent '{}'",
                new_file.to_string_lossy(),
                volume_path.to_string_lossy(),
            );
        }
        let answer = tokio::fs::metadata(new_file).await?;
        let metadata_js = MetadataJs {
            file_type: format!("{:?}", answer.file_type()),
            is_dir: answer.is_dir(),
            is_file: answer.is_file(),
            is_symlink: answer.is_symlink(),
            len: answer.len(),
            modified: answer
                .modified()
                .ok()
                .as_ref()
                .and_then(system_time_as_unix_ms),
            accessed: answer
                .accessed()
                .ok()
                .as_ref()
                .and_then(system_time_as_unix_ms),
            created: answer
                .created()
                .ok()
                .as_ref()
                .and_then(system_time_as_unix_ms),
            readonly: answer.permissions().readonly(),
            gid: answer.gid(),
            mode: answer.mode(),
            uid: answer.uid(),
        };

        Ok(metadata_js)
    }
    #[op]
    async fn write_file(
        state: Rc<RefCell<OpState>>,
        volume_id: VolumeId,
        path_in: PathBuf,
        write: String,
    ) -> Result<(), AnyError> {
        let (volumes, volume_path) = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            let volume_path = ctx
                .volumes
                .path_for(&ctx.datadir, &ctx.package_id, &ctx.version, &volume_id)
                .ok_or_else(|| anyhow!("There is no {} in volumes", volume_id))?;
            (ctx.volumes.clone(), volume_path)
        };
        if volumes.readonly(&volume_id) {
            bail!("Volume {} is readonly", volume_id);
        }

        let new_file = volume_path.join(&path_in);
        let parent_new_file = new_file
            .parent()
            .ok_or_else(|| anyhow!("Expecting that file is not root"))?;
        // With the volume check
        if !is_subset(&volume_path, &parent_new_file).await? {
            bail!(
                "Path '{}' has broken away from parent '{}'",
                new_file.to_string_lossy(),
                volume_path.to_string_lossy(),
            );
        }
        let new_volume_tmp = to_tmp_path(&volume_path).map_err(|e| anyhow!("{}", e))?;
        let hashed_name = {
            use std::os::unix::ffi::OsStrExt;

            use sha2::{Digest, Sha256};
            let mut hasher = Sha256::new();

            hasher.update(path_in.as_os_str().as_bytes());
            let result = hasher.finalize();
            format!("{:X}", result)
        };
        let temp_file = new_volume_tmp.join(&hashed_name);
        let mut file = AtomicFile::new(&new_file, Some(&temp_file))
            .await
            .map_err(|e| anyhow!("{}", e))?;
        file.write_all(write.as_bytes()).await?;
        file.save().await.map_err(|e| anyhow!("{}", e))?;
        Ok(())
    }
    #[op]
    async fn rename(
        state: Rc<RefCell<OpState>>,
        src_volume: VolumeId,
        src_path: PathBuf,
        dst_volume: VolumeId,
        dst_path: PathBuf,
    ) -> Result<(), AnyError> {
        let (volumes, volume_path, volume_path_out) = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            let volume_path = ctx
                .volumes
                .path_for(&ctx.datadir, &ctx.package_id, &ctx.version, &src_volume)
                .ok_or_else(|| anyhow!("There is no {} in volumes", src_volume))?;
            let volume_path_out = ctx
                .volumes
                .path_for(&ctx.datadir, &ctx.package_id, &ctx.version, &dst_volume)
                .ok_or_else(|| anyhow!("There is no {} in volumes", dst_volume))?;
            (ctx.volumes.clone(), volume_path, volume_path_out)
        };
        if volumes.readonly(&dst_volume) {
            bail!("Volume {} is readonly", dst_volume);
        }

        let old_file = volume_path.join(src_path);
        let parent_old_file = old_file
            .parent()
            .ok_or_else(|| anyhow!("Expecting that file is not root"))?;
        // With the volume check
        if !is_subset(&volume_path, &parent_old_file).await? {
            bail!(
                "Path '{}' has broken away from parent '{}'",
                old_file.to_string_lossy(),
                volume_path.to_string_lossy(),
            );
        }

        let new_file = volume_path_out.join(dst_path);
        let parent_new_file = new_file
            .parent()
            .ok_or_else(|| anyhow!("Expecting that file is not root"))?;
        // With the volume check
        if !is_subset(&volume_path_out, &parent_new_file).await? {
            bail!(
                "Path '{}' has broken away from parent '{}'",
                new_file.to_string_lossy(),
                volume_path_out.to_string_lossy(),
            );
        }
        tokio::fs::rename(old_file, new_file).await?;
        Ok(())
    }
    #[op]
    async fn remove_file(
        state: Rc<RefCell<OpState>>,
        volume_id: VolumeId,
        path_in: PathBuf,
    ) -> Result<(), AnyError> {
        let (volumes, volume_path) = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            let volume_path = ctx
                .volumes
                .path_for(&ctx.datadir, &ctx.package_id, &ctx.version, &volume_id)
                .ok_or_else(|| anyhow!("There is no {} in volumes", volume_id))?;
            (ctx.volumes.clone(), volume_path)
        };
        if volumes.readonly(&volume_id) {
            bail!("Volume {} is readonly", volume_id);
        }
        let new_file = volume_path.join(path_in);
        // With the volume check
        if !is_subset(&volume_path, &new_file).await? {
            bail!(
                "Path '{}' has broken away from parent '{}'",
                new_file.to_string_lossy(),
                volume_path.to_string_lossy(),
            );
        }
        tokio::fs::remove_file(new_file).await?;
        Ok(())
    }
    #[op]
    async fn remove_dir(
        state: Rc<RefCell<OpState>>,
        volume_id: VolumeId,
        path_in: PathBuf,
    ) -> Result<(), AnyError> {
        let (volumes, volume_path) = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            let volume_path = ctx
                .volumes
                .path_for(&ctx.datadir, &ctx.package_id, &ctx.version, &volume_id)
                .ok_or_else(|| anyhow!("There is no {} in volumes", volume_id))?;
            (ctx.volumes.clone(), volume_path)
        };
        if volumes.readonly(&volume_id) {
            bail!("Volume {} is readonly", volume_id);
        }
        let new_file = volume_path.join(path_in);
        // With the volume check
        if !is_subset(&volume_path, &new_file).await? {
            bail!(
                "Path '{}' has broken away from parent '{}'",
                new_file.to_string_lossy(),
                volume_path.to_string_lossy(),
            );
        }
        tokio::fs::remove_dir_all(new_file).await?;
        Ok(())
    }
    #[op]
    async fn create_dir(
        state: Rc<RefCell<OpState>>,
        volume_id: VolumeId,
        path_in: PathBuf,
    ) -> Result<(), AnyError> {
        let (volumes, volume_path) = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            let volume_path = ctx
                .volumes
                .path_for(&ctx.datadir, &ctx.package_id, &ctx.version, &volume_id)
                .ok_or_else(|| anyhow!("There is no {} in volumes", volume_id))?;
            (ctx.volumes.clone(), volume_path)
        };
        if volumes.readonly(&volume_id) {
            bail!("Volume {} is readonly", volume_id);
        }
        let new_file = volume_path.join(path_in);
        let parent_new_file = new_file
            .parent()
            .ok_or_else(|| anyhow!("Expecting that file is not root"))?;
        // With the volume check
        if !is_subset(&volume_path, &parent_new_file).await? {
            bail!(
                "Path '{}' has broken away from parent '{}'",
                new_file.to_string_lossy(),
                volume_path.to_string_lossy(),
            );
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
    fn get_input(state: &mut OpState) -> Result<Value, AnyError> {
        let ctx = state.borrow::<JsContext>();
        Ok(ctx.input.clone())
    }
    #[op]
    fn get_variable_args(state: &mut OpState) -> Result<Vec<Value>, AnyError> {
        let ctx = state.borrow::<JsContext>();
        Ok(ctx.variable_args.clone())
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

    #[op]
    async fn term_command(state: Rc<RefCell<OpState>>, id: u32) -> Result<(), AnyError> {
        let term_command_impl: TermCommand = {
            let state = state.borrow();
            let ctx = state.borrow::<JsContext>();
            ctx.term_command.clone()
        };
        if let Err(err) = term_command_impl(embassy_container_init::RpcId::UInt(id)).await {
            bail!("{}", err);
        }
        Ok(())
    }

    #[op]
    async fn start_command(
        state: Rc<RefCell<OpState>>,
        command: String,
        args: Vec<String>,
        timeout: Option<u64>,
    ) -> Result<u32, AnyError> {
        use embassy_container_init::Output;
        let (command_inserter, wait_fns) = {
            let state = state.borrow();
            let ctx = state.borrow::<JsContext>();
            (ctx.command_inserter.clone(), ctx.wait_fns.clone())
        };

        let (sender, mut receiver) = tokio::sync::mpsc::unbounded_channel::<Output>();
        let id = match command_inserter(
            command,
            args.into_iter().collect(),
            sender,
            timeout.map(std::time::Duration::from_millis),
        )
        .await
        {
            Err(err) => bail!(err),
            Ok(RpcId::UInt(a)) => a,
        };

        let wait = async move {
            let mut answer = String::new();
            let mut command_error = String::new();
            let mut status: Option<i32> = None;
            while let Some(output) = receiver.recv().await {
                match output {
                    Output::Line(value) => {
                        answer.push_str(&value);
                        answer.push('\n');
                    }
                    Output::Error(error) => {
                        command_error.push_str(&error);
                        command_error.push('\n');
                    }
                    Output::Done(error_code) => {
                        status = error_code;
                        break;
                    }
                }
            }
            if !command_error.is_empty() {
                if let Some(status) = status {
                    return ResultType::ErrorCode(status, command_error);
                }

                return ResultType::Error(command_error);
            }

            ResultType::Result(serde_json::Value::String(answer))
        };
        wait_fns.lock().await.insert(id, Box::pin(wait));
        Ok(id)
    }

    #[op]
    async fn wait_command(state: Rc<RefCell<OpState>>, id: u32) -> Result<ResultType, AnyError> {
        let wait_fns = {
            let state = state.borrow();
            let ctx = state.borrow::<JsContext>();
            ctx.wait_fns.clone()
        };

        let found_future = match wait_fns.lock().await.remove(&id) {
            Some(a) => a,
            None => bail!("No future for id {id}, could have been removed already"),
        };

        Ok(found_future.await)
    }
    #[op]
    async fn sleep(time_ms: u64) -> Result<(), AnyError> {
        tokio::time::sleep(Duration::from_millis(time_ms)).await;

        Ok(())
    }

    /// We need to make sure that during the file accessing, we don't reach beyond our scope of control
    async fn is_subset(
        parent: impl AsRef<Path>,
        child: impl AsRef<Path>,
    ) -> Result<bool, AnyError> {
        let child = {
            let mut child_count = 0;
            let mut child = child.as_ref();
            loop {
                if child.ends_with("..") {
                    child_count += 1;
                } else if child_count > 0 {
                    child_count -= 1;
                } else {
                    let meta = tokio::fs::metadata(child).await;
                    if meta.is_ok() {
                        break;
                    }
                }
                child = match child.parent() {
                    Some(child) => child,
                    None => {
                        return Ok(false);
                    }
                };
            }
            tokio::fs::canonicalize(child).await?
        };
        let parent = tokio::fs::canonicalize(parent).await?;
        Ok(child.starts_with(parent))
    }

    #[tokio::test]
    async fn test_is_subset() {
        assert!(
            !is_subset("/home/drbonez", "/home/drbonez/code/fakedir/../../..")
                .await
                .unwrap()
        )
    }
}

fn system_time_as_unix_ms(system_time: &SystemTime) -> Option<u64> {
    system_time
        .duration_since(SystemTime::UNIX_EPOCH)
        .ok()?
        .as_millis()
        .try_into()
        .ok()
}
