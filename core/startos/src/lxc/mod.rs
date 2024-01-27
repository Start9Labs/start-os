use std::collections::BTreeSet;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Weak};
use std::time::Duration;

use clap::Parser;
use futures::{FutureExt, StreamExt};
use helpers::UnixRpcClient;
use imbl_value::{InOMap, InternedString};
use rpc_toolkit::yajrc::{RpcError, RpcResponse};
use rpc_toolkit::{
    from_fn_async, AnyContext, CallRemote, CallRemoteHandler, GenericRpcMethod, Handler,
    HandlerArgs, HandlerExt, ParentHandler, RpcRequest,
};
use serde::{Deserialize, Serialize};
use tokio::io::AsyncBufReadExt;
use tokio::process::Command;
use tokio::sync::Mutex;
use tokio::time::Instant;

use crate::context::{CliContext, RpcContext};
use crate::core::rpc_continuations::{RequestGuid, RpcContinuation};
use crate::disk::mount::filesystem::block_dev::BlockDev;
use crate::disk::mount::filesystem::overlayfs::OverlayGuard;
use crate::disk::mount::filesystem::ReadWrite;
use crate::disk::mount::guard::{GenericMountGuard, MountGuard};
use crate::disk::mount::util::unmount;
use crate::prelude::*;
use crate::util::serde::IoFormat;
use crate::util::{new_guid, Invoke};

const LXC_CONTAINER_DIR: &str = "/var/lib/lxc";
pub const CONTAINER_RPC_SERVER_SOCKET: &str = "run/startos/service.sock"; // must not be absolute path
pub const HOST_RPC_SERVER_SOCKET: &str = "run/startos/host.sock"; // must not be absolute path

pub struct LxcManager {
    containers: Mutex<Vec<Weak<InternedString>>>,
}
impl LxcManager {
    pub fn new() -> Self {
        Self {
            containers: Default::default(),
        }
    }

    pub async fn create(self: &Arc<Self>, config: LxcConfig) -> Result<LxcContainer, Error> {
        let container = LxcContainer::new(self, config).await?;
        let mut guard = self.containers.lock().await;
        *guard = std::mem::take(&mut *guard)
            .into_iter()
            .filter(|g| g.strong_count() > 0)
            .chain(std::iter::once(Arc::downgrade(&container.guid)))
            .collect();
        Ok(container)
    }

    pub async fn gc(&self) -> Result<(), Error> {
        let expected = BTreeSet::from_iter(
            self.containers
                .lock()
                .await
                .iter()
                .filter_map(|g| g.upgrade())
                .map(|g| (&*g).clone()),
        );
        for container in String::from_utf8(
            Command::new("lxc-ls")
                .arg("-1")
                .invoke(ErrorKind::Lxc)
                .await?,
        )?
        .lines()
        .map(|s| s.trim())
        {
            if !expected.contains(container) {
                let rootfs_path = Path::new(LXC_CONTAINER_DIR).join(container).join("rootfs");
                if tokio::fs::metadata(&rootfs_path).await.is_ok() {
                    unmount(Path::new(LXC_CONTAINER_DIR).join(container).join("rootfs")).await?;
                    if tokio_stream::wrappers::ReadDirStream::new(
                        tokio::fs::read_dir(&rootfs_path).await?,
                    )
                    .count()
                    .await
                        > 0
                    {
                        return Err(Error::new(
                            eyre!("rootfs is not empty, refusing to delete"),
                            ErrorKind::InvalidRequest,
                        ));
                    }
                }
                Command::new("lxc-destroy")
                    .arg("--force")
                    .arg("--name")
                    .arg(container)
                    .invoke(ErrorKind::Lxc)
                    .await?;
            }
        }
        Ok(())
    }
}

pub struct LxcContainer {
    manager: Weak<LxcManager>,
    rootfs: Option<OverlayGuard>,
    guid: Arc<InternedString>,
    config: LxcConfig,
    exited: bool,
}
impl LxcContainer {
    async fn new(manager: &Arc<LxcManager>, config: LxcConfig) -> Result<Self, Error> {
        let guid = new_guid();
        let container_dir = Path::new(LXC_CONTAINER_DIR).join(&*guid);
        tokio::fs::create_dir_all(&container_dir).await?;
        tokio::fs::write(
            container_dir.join("config"),
            format!(include_str!("./config.template"), guid = &*guid),
        )
        .await?;
        // TODO: append config
        let rootfs_dir = container_dir.join("rootfs");
        let rootfs = OverlayGuard::mount(
            &BlockDev::new("/usr/lib/startos/container-runtime/lxc/rootfs.squashfs"),
            &rootfs_dir,
        )
        .await?;
        tokio::fs::write(rootfs_dir.join("etc/hostname"), format!("{guid}\n")).await?;
        Command::new("sed")
            .arg("-i")
            .arg(format!("s/LXC_NAME/{guid}/g"))
            .arg(rootfs_dir.join("etc/hosts"))
            .invoke(ErrorKind::Filesystem)
            .await?;
        Command::new("mount")
            .arg("--make-rshared")
            .arg(rootfs.path())
            .invoke(ErrorKind::Filesystem)
            .await?;
        Command::new("chown")
            .arg("-R")
            .arg("100000:100000")
            .arg(rootfs.path())
            .invoke(ErrorKind::Filesystem)
            .await?;
        Command::new("lxc-start")
            .arg("-d")
            .arg("--name")
            .arg(&*guid)
            .invoke(ErrorKind::Lxc)
            .await?;
        Ok(Self {
            manager: Arc::downgrade(manager),
            rootfs: Some(rootfs),
            guid: Arc::new(guid),
            config,
            exited: false,
        })
    }

    pub fn rootfs_dir(&self) -> PathBuf {
        Path::new(LXC_CONTAINER_DIR)
            .join(&*self.guid)
            .join("rootfs")
    }

    pub async fn exit(mut self) -> Result<(), Error> {
        if let Some(guard) = self.rootfs.take() {
            guard.unmount(true).await?;
        }
        let rootfs_path = self.rootfs_dir();
        if tokio::fs::metadata(&rootfs_path).await.is_ok() {
            if tokio_stream::wrappers::ReadDirStream::new(tokio::fs::read_dir(&rootfs_path).await?)
                .count()
                .await
                > 0
            {
                return Err(Error::new(
                    eyre!("rootfs is not empty, refusing to delete"),
                    ErrorKind::InvalidRequest,
                ));
            }
        }
        Command::new("lxc-destroy")
            .arg("--force")
            .arg("--name")
            .arg(&**self.guid)
            .invoke(ErrorKind::Lxc)
            .await?;

        self.exited = true;

        Ok(())
    }

    pub async fn connect_rpc(&self, timeout: Option<Duration>) -> Result<UnixRpcClient, Error> {
        let started = Instant::now();
        let sock_path = self.rootfs_dir().join(CONTAINER_RPC_SERVER_SOCKET);
        while tokio::fs::metadata(&sock_path).await.is_err() {
            if timeout.map_or(false, |t| started.elapsed() > t) {
                return Err(Error::new(
                    eyre!("timed out waiting for socket"),
                    ErrorKind::Timeout,
                ));
            }
            tokio::time::sleep(Duration::from_millis(100)).await;
        }
        Ok(UnixRpcClient::new(sock_path))
    }
}
impl Drop for LxcContainer {
    fn drop(&mut self) {
        if !self.exited {
            tracing::warn!(
                "Container {} was ungracefully dropped. Cleaning up dangling containers...",
                &**self.guid
            );
            let rootfs = self.rootfs.take();
            let guid = std::mem::take(&mut self.guid);
            if let Some(manager) = self.manager.upgrade() {
                tokio::spawn(async move {
                    if let Some(rootfs) = rootfs {
                        rootfs.unmount(true).await.unwrap();
                    }
                    drop(guid);
                    if let Err(e) = manager.gc().await {
                        tracing::error!("Error cleaning up dangling LXC containers: {e}");
                        tracing::debug!("{e:?}")
                    } else {
                        tracing::info!("Successfully cleaned up dangling LXC containers");
                    }
                });
            }
        }
    }
}

#[derive(Default, Serialize)]
pub struct LxcConfig {}

pub fn lxc() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "create",
            from_fn_async(create).with_remote_cli::<CliContext>(),
        )
        .subcommand(
            "list",
            from_fn_async(list)
                .with_custom_display_fn::<AnyContext, _>(|_, res| {
                    use prettytable::*;
                    let mut table = table!([bc => "GUID"]);
                    for guid in res {
                        table.add_row(row![&*guid]);
                    }
                    table.printstd();
                    Ok(())
                })
                .with_remote_cli::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove)
                .no_display()
                .with_remote_cli::<CliContext>(),
        )
        .subcommand("connect", from_fn_async(connect).no_cli())
        .subcommand("connect", from_fn_async(connect_cli).no_display())
}

pub async fn create(ctx: RpcContext) -> Result<InternedString, Error> {
    let container = ctx.lxc_manager.create(LxcConfig::default()).await?;
    let guid = container.guid.deref().clone();
    ctx.dev.lxc.lock().await.insert(guid.clone(), container);
    Ok(guid)
}

pub async fn list(ctx: RpcContext) -> Result<Vec<InternedString>, Error> {
    Ok(ctx.dev.lxc.lock().await.keys().cloned().collect())
}

#[derive(Deserialize, Serialize, Parser)]
pub struct RemoveParams {
    pub guid: InternedString,
}

pub async fn remove(ctx: RpcContext, RemoveParams { guid }: RemoveParams) -> Result<(), Error> {
    if let Some(container) = ctx.dev.lxc.lock().await.remove(&guid) {
        container.exit().await?;
    }
    Ok(())
}

#[derive(Deserialize, Serialize, Parser)]
pub struct ConnectParams {
    pub guid: InternedString,
}

pub async fn connect(
    ctx: RpcContext,
    ConnectParams { guid }: ConnectParams,
) -> Result<RequestGuid, Error> {
    use axum::extract::ws::Message;

    let rpc = ctx
        .dev
        .lxc
        .lock()
        .await
        .get(&guid)
        .ok_or_else(|| Error::new(eyre!("No container with guid: {guid}"), ErrorKind::NotFound))?
        .connect_rpc(Some(Duration::from_secs(30)))
        .await?;
    let guid = RequestGuid::new();
    ctx.add_continuation(
        guid.clone(),
        RpcContinuation::ws(
            Box::new(|mut ws| {
                async move {
                    if let Err(e) = async {
                        loop {
                            match ws.next().await {
                                None => break,
                                Some(Ok(Message::Text(txt))) => {
                                    let mut id = None;
                                    let result = async {
                                        let req: RpcRequest =
                                            serde_json::from_str(&txt).map_err(|e| RpcError {
                                                data: Some(serde_json::Value::String(
                                                    e.to_string(),
                                                )),
                                                ..rpc_toolkit::yajrc::PARSE_ERROR
                                            })?;
                                        id = req.id;
                                        rpc.request(req.method, req.params).await
                                    }
                                    .await;
                                    ws.send(Message::Text(
                                        serde_json::to_string(&RpcResponse::<GenericRpcMethod> {
                                            id,
                                            result,
                                        })
                                        .with_kind(ErrorKind::Serialization)?,
                                    ))
                                    .await
                                    .with_kind(ErrorKind::Network)?;
                                }
                                Some(Ok(_)) => (),
                                Some(Err(e)) => {
                                    return Err(Error::new(e, ErrorKind::Network));
                                }
                            }
                        }
                        Ok::<_, Error>(())
                    }
                    .await
                    {
                        tracing::error!("{e}");
                        tracing::debug!("{e:?}");
                    }
                }
                .boxed()
            }),
            Duration::from_secs(30),
        ),
    )
    .await;
    Ok(guid)
}

pub async fn connect_cli(handle_args: HandlerArgs<CliContext, ConnectParams>) -> Result<(), Error> {
    use futures::SinkExt;
    use tokio_tungstenite::tungstenite::Message;

    let ctx = handle_args.context.clone();
    let guid = CallRemoteHandler::<CliContext, _>::new(from_fn_async(connect))
        .handle_async(handle_args)
        .await?;
    let mut ws = ctx.ws_continuation(guid).await?;
    let mut input = tokio::io::BufReader::new(tokio::io::stdin()).lines();
    loop {
        tokio::select! {
            line = input.next_line() => {
                if let Some(line) = line? {
                    if serde_json::from_str::<RpcRequest>(&line).is_ok() {
                        ws.send(Message::Text(line))
                            .await
                            .with_kind(ErrorKind::Network)?;
                    } else {
                        match shell_words::split(&line) {
                            Ok(command) => {
                                if let Some((method, rest)) = command.split_first() {
                                    let mut params = InOMap::new();
                                    for arg in rest {
                                        if let Some((name, value)) = arg.split_once("=") {
                                            params.insert(InternedString::intern(name), if value.is_empty() {
                                                Value::Null
                                            } else if let Ok(v) = serde_json::from_str(value) {
                                                v
                                            } else {
                                                Value::String(Arc::new(value.into()))
                                            });
                                        } else {
                                            tracing::error!("argument without a value: {arg}");
                                            tracing::debug!("help: set the value of {arg} with `{arg}=...`");
                                            continue;
                                        }
                                    }
                                    ws.send(Message::Text(match serde_json::to_string(&RpcRequest {
                                        id: None,
                                        method: GenericRpcMethod::new(method.into()),
                                        params: Value::Object(params),
                                    }) {
                                        Ok(a) => a,
                                        Err(e) => {
                                            tracing::error!("Error Serializing Request: {e}");
                                            tracing::debug!("{e:?}");
                                            continue;
                                        }
                                    })).await.with_kind(ErrorKind::Network)?;
                                }
                            }
                            Err(e) => {
                                tracing::error!("{e}");
                                tracing::debug!("{e:?}");
                            }
                        }
                    }
                } else {
                    ws.send(Message::Close(None)).await.with_kind(ErrorKind::Network)?;
                }
            }
            msg = ws.next() => {
                match msg {
                    None => break,
                    Some(Ok(Message::Text(txt))) => {
                        match serde_json::from_str::<RpcResponse>(&txt) {
                            Ok(RpcResponse { result: Ok(a), .. }) => {
                                println!("{a:?}");
                            }
                            Ok(RpcResponse { result: Err(e), .. }) => {
                                let e: Error = e.into();
                                tracing::error!("{e}");
                                tracing::debug!("{e:?}");
                            }
                            Err(e) => {
                                tracing::error!("Error Parsing RPC response: {e}");
                                tracing::debug!("{e:?}");
                            }
                        }
                    }
                    Some(Ok(_)) => (),
                    Some(Err(e)) => {
                        return Err(Error::new(e, ErrorKind::Network));
                    }
                }
            }
        }
    }

    Ok(())
}