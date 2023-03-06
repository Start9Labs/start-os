use std::collections::BTreeMap;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::sync::Arc;
use std::task::Poll;
use std::time::SystemTime;

use deno_core::anyhow::{anyhow, bail};
use deno_core::error::AnyError;
use deno_core::{
    resolve_import, Extension, JsRuntime, ModuleLoader, ModuleSource, ModuleSourceFuture,
    ModuleSpecifier, ModuleType, OpDecl, RuntimeOptions, Snapshot,
};
use embassy_container_init::ProcessGroupId;
use helpers::{script_dir, spawn_local, OsApi, Rsync, UnixRpcClient};
use models::{PackageId, ProcedureName, Version, VolumeId};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::io::AsyncReadExt;
use tokio::sync::{mpsc, Mutex};
use tracing::instrument;

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Algorithm {
    Ecdsa,
    Ed25519,
}
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

#[derive(Clone)]
struct JsContext {
    sandboxed: bool,
    os: Arc<dyn OsApi>,
    datadir: PathBuf,
    run_function: String,
    version: Version,
    package_id: PackageId,
    volumes: Arc<dyn PathForVolumeId>,
    input: Value,
    variable_args: Vec<serde_json::Value>,
    container_process_gid: ProcessGroupId,
    container_rpc_client: Option<Arc<UnixRpcClient>>,
    rsyncs: Arc<Mutex<(usize, BTreeMap<usize, Rsync>)>>,
    callback_sender: mpsc::UnboundedSender<(Arc<String>, Vec<Value>)>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
enum ResultType {
    Error(String),
    ErrorCode(i32, String),
    Result(serde_json::Value),
}
#[derive(Clone)]
struct AnswerState(mpsc::Sender<Value>);

impl AnswerState {
    fn new() -> (Self, mpsc::Receiver<Value>) {
        let (send, recv) = mpsc::channel(1);
        (Self(send), recv)
    }
}

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
    os: Arc<dyn OsApi>,
    base_directory: PathBuf,
    module_loader: ModsLoader,
    package_id: PackageId,
    version: Version,
    volumes: Arc<dyn PathForVolumeId>,
    container_process_gid: ProcessGroupId,
    container_rpc_client: Option<Arc<UnixRpcClient>>,
}

impl JsExecutionEnvironment {
    pub async fn load_from_package(
        os: Arc<dyn OsApi>,
        data_directory: impl AsRef<std::path::Path>,
        package_id: &PackageId,
        version: &Version,
        volumes: Box<dyn PathForVolumeId>,
        container_process_gid: ProcessGroupId,
        container_rpc_client: Option<Arc<UnixRpcClient>>,
    ) -> Result<Self, (JsError, String)> {
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
            os,
            base_directory: base_directory.to_owned(),
            module_loader: ModsLoader { code: js_code },
            package_id: package_id.clone(),
            version: version.clone(),
            volumes: volumes.into(),
            sandboxed: false,
            container_process_gid,
            container_rpc_client,
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
        let safer_handle = spawn_local(|| self.execute(procedure_name, input, variable_args)).await;
        let output = safer_handle.await.unwrap()?;
        match serde_json::from_value(output.clone()) {
            Ok(x) => Ok(x),
            Err(err) => {
                tracing::error!("{}", err);
                tracing::debug!("{:?}", err);
                Err((
                    JsError::BoundryLayerSerDe,
                    format!(
                        "Couldn't convert output = {:#?} to the correct type",
                        serde_json::to_string_pretty(&output).unwrap_or_default()
                    ),
                ))
            }
        }
    }
    fn declarations() -> Vec<OpDecl> {
        vec![
            fns::bind_local::decl(),
            fns::bind_onion::decl(),
            fns::chown::decl(),
            fns::fetch::decl(),
            fns::read_file::decl(),
            fns::metadata::decl(),
            fns::write_file::decl(),
            fns::rename::decl(),
            fns::remove_file::decl(),
            fns::create_dir::decl(),
            fns::remove_dir::decl(),
            fns::read_dir::decl(),
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
            fns::send_signal::decl(),
            fns::chmod::decl(),
            fns::signal_group::decl(),
            fns::rsync::decl(),
            fns::rsync_wait::decl(),
            fns::rsync_progress::decl(),
            fns::get_service_config::decl(),
            fns::set_started::decl(),
            fns::restart::decl(),
            fns::start::decl(),
            fns::stop::decl(),
        ]
    }

    #[instrument(skip(self))]
    async fn execute(
        self,
        procedure_name: ProcedureName,
        input: Value,
        variable_args: Vec<serde_json::Value>,
    ) -> Result<Value, (JsError, String)> {
        let base_directory = self.base_directory.clone();
        let (answer_state, mut receive_answer) = AnswerState::new();
        let ext_answer_state = answer_state.clone();
        let (callback_sender, callback_receiver) = mpsc::unbounded_channel();
        let js_ctx = JsContext {
            os: self.os,
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
            container_process_gid: self.container_process_gid,
            container_rpc_client: self.container_rpc_client.clone(),
            callback_sender,
            rsyncs: Default::default(),
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
        let mut runtime = JsRuntime::new(runtime_options);

        let future = async move {
            let mod_id = runtime
                .load_main_module(&"file:///loadModule.js".parse().unwrap(), None)
                .await?;
            let evaluated = runtime.mod_evaluate(mod_id);
            let res = RuntimeEventLoop {
                runtime: &mut runtime,
                callback_receiver,
            }
            .await;
            res?;
            evaluated.await??;
            Ok::<_, AnyError>(())
        };
        let answer = tokio::select! {
            Some(x) = receive_answer.recv() => x,
            _ = future => {
                if let Some(x) = receive_answer.recv().await {
                    x
                }
                else {
                    serde_json::json!({"error": "JS Engine Shutdown"})
                }
            },

        };
        Ok(answer)
    }
}

#[pin_project::pin_project]
struct RuntimeEventLoop<'a> {
    runtime: &'a mut JsRuntime,
    callback_receiver: mpsc::UnboundedReceiver<(Arc<String>, Vec<Value>)>,
}
impl<'a> Future for RuntimeEventLoop<'a> {
    type Output = Result<(), AnyError>;
    fn poll(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = self.project();
        if let Poll::Ready(Some((uuid, args))) = this.callback_receiver.poll_recv(cx) {
            match this.runtime.execute_script(
                "callback",
                &format!("globalThis.runCallback(\"{uuid}\", {})", Value::Array(args)),
            ) {
                Ok(_) => (),
                Err(e) => return Poll::Ready(Err(e)),
            }
        }
        this.runtime.poll_event_loop(cx, false)
    }
}

/// Note: Make sure that we have the assumption that all these methods are callable at any time, and all call restrictions should be in rust
mod fns {
    use std::cell::RefCell;
    use std::collections::BTreeMap;
    use std::convert::TryFrom;
    use std::fs::Permissions;
    use std::os::unix::fs::MetadataExt;
    use std::os::unix::prelude::PermissionsExt;
    use std::path::{Path, PathBuf};
    use std::rc::Rc;
    use std::time::Duration;

    use deno_core::anyhow::{anyhow, bail};
    use deno_core::error::AnyError;
    use deno_core::*;
    use embassy_container_init::{
        OutputParams, OutputStrategy, ProcessGroupId, ProcessId, RunCommand, RunCommandParams,
        SendSignal, SendSignalParams, SignalGroup, SignalGroupParams,
    };
    use helpers::{
        to_tmp_path, AddressSchemaLocal, AddressSchemaOnion, AtomicFile, Callback, Rsync,
        RsyncOptions,
    };
    use models::{PackageId, VolumeId};
    use serde::{Deserialize, Serialize};
    use serde_json::{json, Value};
    use tokio::io::AsyncWriteExt;

    use super::{AnswerState, JsContext};
    use crate::{system_time_as_unix_ms, Algorithm, MetadataJs, ResultType};

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
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run writeFile in sandboxed mode");
        }

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
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run rename in sandboxed mode");
        }

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
    async fn rsync(
        state: Rc<RefCell<OpState>>,
        src_volume: VolumeId,
        src_path: PathBuf,
        dst_volume: VolumeId,
        dst_path: PathBuf,
        options: RsyncOptions,
    ) -> Result<usize, AnyError> {
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run rsync in sandboxed mode");
        }

        let (volumes, volume_path, volume_path_out, rsyncs) = {
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
            (
                ctx.volumes.clone(),
                volume_path,
                volume_path_out,
                ctx.rsyncs.clone(),
            )
        };
        if volumes.readonly(&dst_volume) {
            bail!("Volume {} is readonly", dst_volume);
        }

        let src = volume_path.join(src_path);
        // With the volume check
        if !is_subset(&volume_path, &src).await? {
            bail!(
                "Path '{}' has broken away from parent '{}'",
                src.to_string_lossy(),
                volume_path.to_string_lossy(),
            );
        }
        if tokio::fs::metadata(&src).await.is_err() {
            bail!("Source at {} does not exists", src.to_string_lossy());
        }

        let dst = volume_path_out.join(dst_path);
        // With the volume check
        if !is_subset(&volume_path_out, &dst).await? {
            bail!(
                "Path '{}' has broken away from parent '{}'",
                dst.to_string_lossy(),
                volume_path_out.to_string_lossy(),
            );
        }

        let running_rsync = Rsync::new(src, dst, options)
            .await
            .map_err(|e| anyhow::anyhow!("{:?}", e.source))?;
        let insert_id = {
            let mut rsyncs = rsyncs.lock().await;
            let next = rsyncs.0 + 1;
            rsyncs.0 = next;
            rsyncs.1.insert(next, running_rsync);
            next
        };
        Ok(insert_id)
    }

    #[op]
    async fn rsync_wait(state: Rc<RefCell<OpState>>, id: usize) -> Result<(), AnyError> {
        let rsyncs = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.rsyncs.clone()
        };
        let running_rsync = match rsyncs.lock().await.1.remove(&id) {
            Some(a) => a,
            None => bail!("Couldn't find rsync at id {id}"),
        };
        running_rsync
            .wait()
            .await
            .map_err(|x| anyhow::anyhow!("{}", x.source))?;
        Ok(())
    }
    #[op]
    async fn rsync_progress(state: Rc<RefCell<OpState>>, id: usize) -> Result<f64, AnyError> {
        use futures::StreamExt;
        let rsyncs = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.rsyncs.clone()
        };
        let mut running_rsync = match rsyncs.lock().await.1.remove(&id) {
            Some(a) => a,
            None => bail!("Couldn't find rsync at id {id}"),
        };
        let progress = running_rsync.progress.next().await.unwrap_or_default();
        rsyncs.lock().await.1.insert(id, running_rsync);
        Ok(progress)
    }
    #[op]
    async fn chown(
        state: Rc<RefCell<OpState>>,
        volume_id: VolumeId,
        path_in: PathBuf,
        ownership: u32,
    ) -> Result<(), AnyError> {
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run chown in sandboxed mode");
        }

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
        let output = tokio::process::Command::new("chown")
            .arg("--recursive")
            .arg(format!("{ownership}"))
            .arg(new_file.as_os_str())
            .output()
            .await?;
        if !output.status.success() {
            return Err(anyhow!("Chown Error"));
        }
        Ok(())
    }
    #[op]
    async fn chmod(
        state: Rc<RefCell<OpState>>,
        volume_id: VolumeId,
        path_in: PathBuf,
        mode: u32,
    ) -> Result<(), AnyError> {
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run chmod in sandboxed mode");
        }

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
        tokio::fs::set_permissions(new_file, Permissions::from_mode(mode)).await?;
        Ok(())
    }
    #[op]
    async fn remove_file(
        state: Rc<RefCell<OpState>>,
        volume_id: VolumeId,
        path_in: PathBuf,
    ) -> Result<(), AnyError> {
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run removeFile in sandboxed mode");
        }

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
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run removeDir in sandboxed mode");
        }

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
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run createDir in sandboxed mode");
        }

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
        tokio::fs::create_dir_all(new_file).await?;
        Ok(())
    }
    #[op]
    async fn read_dir(
        state: Rc<RefCell<OpState>>,
        volume_id: VolumeId,
        path_in: PathBuf,
    ) -> Result<Vec<String>, AnyError> {
        let volume_path = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            let volume_path = ctx
                .volumes
                .path_for(&ctx.datadir, &ctx.package_id, &ctx.version, &volume_id)
                .ok_or_else(|| anyhow!("There is no {} in volumes", volume_id))?;
            volume_path
        };
        let new_file = volume_path.join(path_in);

        // With the volume check
        if !is_subset(&volume_path, &new_file).await? {
            bail!(
                "Path '{}' has broken away from parent '{}'",
                new_file.to_string_lossy(),
                volume_path.to_string_lossy(),
            );
        }
        let mut reader = tokio::fs::read_dir(&new_file).await?;
        let mut paths: Vec<String> = Vec::new();
        let origin_path = format!("{}/", new_file.to_str().unwrap_or_default());
        let remove_new_file = |other_path: String| other_path.replacen(&origin_path, "", 1);
        let has_origin_path = |other_path: &String| other_path.starts_with(&origin_path);
        while let Some(entry) = reader.next_entry().await? {
            entry
                .path()
                .to_str()
                .into_iter()
                .map(ToString::to_string)
                .filter(&has_origin_path)
                .map(&remove_new_file)
                .for_each(|x| paths.push(x));
        }
        paths.sort();
        Ok(paths)
    }

    #[op]
    fn current_function(state: &mut OpState) -> Result<String, AnyError> {
        let ctx = state.borrow::<JsContext>();
        Ok(ctx.run_function.clone())
    }

    #[op]
    async fn log_trace(state: Rc<RefCell<OpState>>, input: String) -> Result<(), AnyError> {
        let ctx = {
            let state = state.borrow();
            state.borrow::<JsContext>().clone()
        };
        if let Some(rpc_client) = ctx.container_rpc_client {
            return rpc_client
                .request(
                    embassy_container_init::Log,
                    embassy_container_init::LogParams {
                        gid: Some(ctx.container_process_gid),
                        level: embassy_container_init::LogLevel::Trace(input),
                    },
                )
                .await
                .map_err(|e| anyhow!("{}: {:?}", e.message, e.data));
        }
        tracing::trace!(
            package_id = tracing::field::display(&ctx.package_id),
            run_function = tracing::field::display(&ctx.run_function),
            "{}",
            input
        );
        Ok(())
    }
    #[op]
    async fn log_warn(state: Rc<RefCell<OpState>>, input: String) -> Result<(), AnyError> {
        let ctx = {
            let state = state.borrow();
            state.borrow::<JsContext>().clone()
        };
        if let Some(rpc_client) = ctx.container_rpc_client {
            return rpc_client
                .request(
                    embassy_container_init::Log,
                    embassy_container_init::LogParams {
                        gid: Some(ctx.container_process_gid),
                        level: embassy_container_init::LogLevel::Warn(input),
                    },
                )
                .await
                .map_err(|e| anyhow!("{}: {:?}", e.message, e.data));
        }
        tracing::warn!(
            package_id = tracing::field::display(&ctx.package_id),
            run_function = tracing::field::display(&ctx.run_function),
            "{}",
            input
        );
        Ok(())
    }
    #[op]
    async fn log_error(state: Rc<RefCell<OpState>>, input: String) -> Result<(), AnyError> {
        let ctx = {
            let state = state.borrow();
            state.borrow::<JsContext>().clone()
        };
        if let Some(rpc_client) = ctx.container_rpc_client {
            return rpc_client
                .request(
                    embassy_container_init::Log,
                    embassy_container_init::LogParams {
                        gid: Some(ctx.container_process_gid),
                        level: embassy_container_init::LogLevel::Error(input),
                    },
                )
                .await
                .map_err(|e| anyhow!("{}: {:?}", e.message, e.data));
        }
        tracing::error!(
            package_id = tracing::field::display(&ctx.package_id),
            run_function = tracing::field::display(&ctx.run_function),
            "{}",
            input
        );
        Ok(())
    }
    #[op]
    async fn log_debug(state: Rc<RefCell<OpState>>, input: String) -> Result<(), AnyError> {
        let ctx = {
            let state = state.borrow();
            state.borrow::<JsContext>().clone()
        };
        if let Some(rpc_client) = ctx.container_rpc_client {
            return rpc_client
                .request(
                    embassy_container_init::Log,
                    embassy_container_init::LogParams {
                        gid: Some(ctx.container_process_gid),
                        level: embassy_container_init::LogLevel::Debug(input),
                    },
                )
                .await
                .map_err(|e| anyhow!("{}: {:?}", e.message, e.data));
        }
        tracing::debug!(
            package_id = tracing::field::display(&ctx.package_id),
            run_function = tracing::field::display(&ctx.run_function),
            "{}",
            input
        );
        Ok(())
    }
    #[op]
    async fn log_info(state: Rc<RefCell<OpState>>, input: String) -> Result<(), AnyError> {
        let ctx = {
            let state = state.borrow();
            state.borrow::<JsContext>().clone()
        };
        if let Some(rpc_client) = ctx.container_rpc_client {
            return rpc_client
                .request(
                    embassy_container_init::Log,
                    embassy_container_init::LogParams {
                        gid: Some(ctx.container_process_gid),
                        level: embassy_container_init::LogLevel::Info(input),
                    },
                )
                .await
                .map_err(|e| anyhow!("{}: {:?}", e.message, e.data));
        }
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
    async fn set_value(state: Rc<RefCell<OpState>>, value: Value) -> Result<(), AnyError> {
        let sender = {
            let state = state.borrow();
            let answer_state = state.borrow::<AnswerState>().0.clone();
            answer_state
        };
        sender
            .send(value)
            .await
            .map_err(|_e| anyhow!("Could not set a value"))?;
        Ok(())
    }
    #[op]
    fn is_sandboxed(state: &mut OpState) -> Result<bool, AnyError> {
        let ctx = state.borrow::<JsContext>();
        Ok(ctx.sandboxed)
    }

    #[op]
    async fn send_signal(
        state: Rc<RefCell<OpState>>,
        pid: u32,
        signal: u32,
    ) -> Result<(), AnyError> {
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run sendSignal in sandboxed mode");
        }

        if let Some(rpc_client) = {
            let state = state.borrow();
            let ctx = state.borrow::<JsContext>();
            ctx.container_rpc_client.clone()
        } {
            rpc_client
                .request(
                    SendSignal,
                    SendSignalParams {
                        pid: ProcessId(pid),
                        signal,
                    },
                )
                .await
                .map_err(|e| anyhow!("{}: {:?}", e.message, e.data))?;

            Ok(())
        } else {
            Err(anyhow!("No RpcClient for command operations"))
        }
    }

    #[op]
    async fn signal_group(
        state: Rc<RefCell<OpState>>,
        gid: u32,
        signal: u32,
    ) -> Result<(), AnyError> {
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run signalGroup in sandboxed mode");
        }

        if let Some(rpc_client) = {
            let state = state.borrow();
            let ctx = state.borrow::<JsContext>();
            ctx.container_rpc_client.clone()
        } {
            rpc_client
                .request(
                    SignalGroup,
                    SignalGroupParams {
                        gid: ProcessGroupId(gid),
                        signal,
                    },
                )
                .await
                .map_err(|e| anyhow!("{}: {:?}", e.message, e.data))?;

            Ok(())
        } else {
            Err(anyhow!("No RpcClient for command operations"))
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub struct StartCommand {
        process_id: ProcessId,
    }

    #[op]
    async fn start_command(
        state: Rc<RefCell<OpState>>,
        command: String,
        args: Vec<String>,
        output: OutputStrategy,
        timeout: Option<u64>,
    ) -> Result<StartCommand, AnyError> {
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run command in sandboxed mode");
        }

        if let (gid, Some(rpc_client)) = {
            let state = state.borrow();
            let ctx = state.borrow::<JsContext>();
            (ctx.container_process_gid, ctx.container_rpc_client.clone())
        } {
            let pid = rpc_client
                .request(
                    RunCommand,
                    RunCommandParams {
                        gid: Some(gid),
                        command,
                        args,
                        output,
                    },
                )
                .await
                .map_err(|e| anyhow!("{}: {:?}", e.message, e.data))?;

            if let Some(timeout) = timeout {
                tokio::spawn(async move {
                    tokio::time::sleep(Duration::from_millis(timeout)).await;
                    if let Err(err) = rpc_client
                        .request(SendSignal, SendSignalParams { pid, signal: 9 })
                        .await
                        .map_err(|e| anyhow!("{}: {:?}", e.message, e.data))
                    {
                        tracing::warn!("Could not kill process {pid:?}");
                        tracing::debug!("{err:?}");
                    }
                });
            }

            Ok(StartCommand { process_id: pid })
        } else {
            Err(anyhow!("No RpcClient for command operations"))
        }
    }

    #[op]
    async fn wait_command(
        state: Rc<RefCell<OpState>>,
        pid: ProcessId,
    ) -> Result<ResultType, AnyError> {
        if let Some(rpc_client) = {
            let state = state.borrow();
            let ctx = state.borrow::<JsContext>();
            ctx.container_rpc_client.clone()
        } {
            Ok(
                match rpc_client
                    .request(embassy_container_init::Output, OutputParams { pid })
                    .await
                {
                    Ok(a) => ResultType::Result(json!(a)),
                    Err(e) => ResultType::ErrorCode(
                        e.code,
                        match e.data {
                            Some(Value::String(s)) => s,
                            e => format!("{:?}", e),
                        },
                    ),
                },
            )
        } else {
            Err(anyhow!("No RpcClient for command operations"))
        }
    }

    #[op]
    async fn sleep(time_ms: u64) -> Result<(), AnyError> {
        tokio::time::sleep(Duration::from_millis(time_ms)).await;

        Ok(())
    }

    #[op]
    async fn get_service_config(
        state: Rc<RefCell<OpState>>,
        service_id: Option<PackageId>,
        path: Option<String>,
        callback: Option<String>,
    ) -> Result<Vec<Value>, AnyError> {
        let (sender, os) = {
            let state = state.borrow();
            let ctx = state.borrow::<JsContext>();
            (ctx.callback_sender.clone(), ctx.os.clone())
        };
        os.get_service_config(
            service_id,
            path.as_deref(),
            callback.map(|id| Callback::new(id, sender)),
        )
        .await
        .map_err(|e| anyhow!("Couldn't get service config: {e:?}"))
    }

    #[op]
    async fn bind_onion(
        state: Rc<RefCell<OpState>>,
        internal_port: u16,
        address_schema: AddressSchemaOnion,
    ) -> Result<helpers::Address, AnyError> {
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run bindOnion in sandboxed mode");
        }

        let os = {
            let state = state.borrow();
            let ctx = state.borrow::<JsContext>();
            ctx.os.clone()
        };
        os.bind_onion(internal_port, address_schema)
            .await
            .map_err(|e| anyhow!("{e:?}"))
    }
    #[op]
    async fn bind_local(
        state: Rc<RefCell<OpState>>,
        internal_port: u16,
        address_schema: AddressSchemaLocal,
    ) -> Result<helpers::Address, AnyError> {
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run bindLocal in sandboxed mode");
        }

        let os = {
            let state = state.borrow();
            let ctx = state.borrow::<JsContext>();
            ctx.os.clone()
        };
        os.bind_local(internal_port, address_schema)
            .await
            .map_err(|e| anyhow!("{e:?}"))
    }

    #[op]
    fn set_started(state: &mut OpState) -> Result<(), AnyError> {
        let os = {
            let ctx = state.borrow::<JsContext>();
            ctx.os.clone()
        };
        os.set_started().map_err(|e| anyhow!("{e:?}"))
    }

    #[op]
    async fn restart(state: Rc<RefCell<OpState>>) -> Result<(), AnyError> {
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run restart in sandboxed mode");
        }

        let os = {
            let state = state.borrow();
            let ctx = state.borrow::<JsContext>();
            ctx.os.clone()
        };
        os.restart().await.map_err(|e| anyhow!("{e:?}"))
    }

    #[op]
    async fn start(state: Rc<RefCell<OpState>>) -> Result<(), AnyError> {
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run start in sandboxed mode");
        }

        let os = {
            let state = state.borrow();
            let ctx = state.borrow::<JsContext>();
            ctx.os.clone()
        };
        os.start().await.map_err(|e| anyhow!("{e:?}"))
    }

    #[op]
    async fn stop(state: Rc<RefCell<OpState>>) -> Result<(), AnyError> {
        let sandboxed = {
            let state = state.borrow();
            let ctx: &JsContext = state.borrow();
            ctx.sandboxed
        };

        if sandboxed {
            bail!("Will not run stop in sandboxed mode");
        }

        let os = {
            let state = state.borrow();
            let ctx = state.borrow::<JsContext>();
            ctx.os.clone()
        };
        os.stop().await.map_err(|e| anyhow!("{e:?}"))
    }
    #[op]
    async fn get_service_local_address(
        state: Rc<RefCell<OpState>>,
        package_id: PackageId,
        interface_name: String,
    ) -> Result<(), AnyError> {
        todo!()
    }
    #[op]
    async fn get_service_tor_address(
        state: Rc<RefCell<OpState>>,
        package_id: PackageId,
        interface_name: String,
    ) -> Result<(), AnyError> {
        todo!()
    }
    #[op]
    async fn get_service_port_forward(
        state: Rc<RefCell<OpState>>,
        package_id: PackageId,
        interface_name: String,
    ) -> Result<(), AnyError> {
        todo!()
    }

    #[op]
    async fn export_address(
        state: Rc<RefCell<OpState>>,
        name: String,
        description: String,
        address: String,
        id: String,
        ui: bool,
    ) -> Result<(), AnyError> {
        todo!()
    }
    #[op]
    async fn remove_address(state: Rc<RefCell<OpState>>, id: String) -> Result<(), AnyError> {
        todo!()
    }

    #[op]
    async fn export_action(
        state: Rc<RefCell<OpState>>,
        name: String,
        description: String,
        address: String,
        id: String,
        ui: bool,
        group: Option<String>,
    ) -> Result<(), AnyError> {
        todo!()
    }
    #[op]
    async fn remove_action(state: Rc<RefCell<OpState>>, id: String) -> Result<(), AnyError> {
        todo!()
    }

    #[op]
    async fn get_configured(state: Rc<RefCell<OpState>>) -> Result<bool, AnyError> {
        todo!()
    }
    #[op]
    async fn set_configured(state: Rc<RefCell<OpState>>, configured: bool) -> Result<(), AnyError> {
        todo!()
    }
    #[op]
    async fn get_ssl_certifcate(
        state: Rc<RefCell<OpState>>,
        ist: String,
        algorithm: Algorithm,
    ) -> Result<(String, String, String), AnyError> {
        todo!()
    }
    #[op]
    async fn get_ssl_key(
        state: Rc<RefCell<OpState>>,
        id: String,
        algorithm: Algorithm,
    ) -> Result<String, AnyError> {
        todo!()
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
