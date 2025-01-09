use std::collections::BTreeSet;
use std::net::Ipv4Addr;
use std::path::Path;
use std::sync::{Arc, Weak};
use std::time::Duration;

use clap::builder::ValueParserFactory;
use futures::{AsyncWriteExt, StreamExt};
use imbl_value::{InOMap, InternedString};
use models::{FromStrParser, InvalidId};
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{GenericRpcMethod, RpcRequest, RpcResponse};
use rustyline_async::{ReadlineEvent, SharedWriter};
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Command;
use tokio::sync::Mutex;
use tokio::time::Instant;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::disk::mount::filesystem::bind::Bind;
use crate::disk::mount::filesystem::block_dev::BlockDev;
use crate::disk::mount::filesystem::idmapped::IdMapped;
use crate::disk::mount::filesystem::overlayfs::OverlayGuard;
use crate::disk::mount::filesystem::{MountType, ReadOnly, ReadWrite};
use crate::disk::mount::guard::{GenericMountGuard, MountGuard, TmpMountGuard};
use crate::disk::mount::util::unmount;
use crate::prelude::*;
use crate::rpc_continuations::{Guid, RpcContinuation};
use crate::util::io::open_file;
use crate::util::rpc_client::UnixRpcClient;
use crate::util::{new_guid, Invoke};

// #[cfg(feature = "dev")]
pub mod dev;

const LXC_CONTAINER_DIR: &str = "/var/lib/lxc";
const RPC_DIR: &str = "media/startos/rpc"; // must not be absolute path
pub const CONTAINER_RPC_SERVER_SOCKET: &str = "service.sock"; // must not be absolute path
pub const HOST_RPC_SERVER_SOCKET: &str = "host.sock"; // must not be absolute path
const CONTAINER_DHCP_TIMEOUT: Duration = Duration::from_secs(30);

#[derive(
    Clone, Debug, Serialize, Deserialize, Default, PartialEq, Eq, PartialOrd, Ord, Hash, TS,
)]
#[ts(type = "string")]
pub struct ContainerId(InternedString);
impl std::ops::Deref for ContainerId {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl std::fmt::Display for ContainerId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &*self.0)
    }
}
impl TryFrom<&str> for ContainerId {
    type Error = InvalidId;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(ContainerId(InternedString::intern(value)))
    }
}
impl std::str::FromStr for ContainerId {
    type Err = InvalidId;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
    }
}
impl ValueParserFactory for ContainerId {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        FromStrParser::new()
    }
}

#[derive(Default)]
pub struct LxcManager {
    containers: Mutex<Vec<Weak<ContainerId>>>,
}

impl LxcManager {
    pub fn new() -> Self {
        Self::default()
    }

    pub async fn create(
        self: &Arc<Self>,
        log_mount: Option<&Path>,
        config: LxcConfig,
    ) -> Result<LxcContainer, Error> {
        let container = LxcContainer::new(self, log_mount, config).await?;
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
                .map(|g| (*g).clone()),
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
            if !expected.contains(&ContainerId::try_from(container)?) {
                let rootfs_path = Path::new(LXC_CONTAINER_DIR).join(container).join("rootfs");
                if tokio::fs::metadata(&rootfs_path).await.is_ok() {
                    unmount(
                        Path::new(LXC_CONTAINER_DIR).join(container).join("rootfs"),
                        true,
                    )
                    .await
                    .log_err();
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
    rootfs: OverlayGuard<TmpMountGuard>,
    pub guid: Arc<ContainerId>,
    rpc_bind: TmpMountGuard,
    log_mount: Option<MountGuard>,
    config: LxcConfig,
    exited: bool,
}
impl LxcContainer {
    async fn new(
        manager: &Arc<LxcManager>,
        log_mount: Option<&Path>,
        config: LxcConfig,
    ) -> Result<Self, Error> {
        let guid = new_guid();
        let machine_id = hex::encode(rand::random::<[u8; 16]>());
        let container_dir = Path::new(LXC_CONTAINER_DIR).join(&*guid);
        tokio::fs::create_dir_all(&container_dir).await?;
        tokio::fs::write(
            container_dir.join("config"),
            format!(include_str!("./config.template"), guid = &*guid),
        )
        .await?;
        // TODO: append config
        let rootfs_dir = container_dir.join("rootfs");
        tokio::fs::create_dir_all(&rootfs_dir).await?;
        Command::new("chown")
            .arg("100000:100000")
            .arg(&rootfs_dir)
            .invoke(ErrorKind::Filesystem)
            .await?;
        let rootfs = OverlayGuard::mount(
            TmpMountGuard::mount(
                &IdMapped::new(
                    BlockDev::new("/usr/lib/startos/container-runtime/rootfs.squashfs"),
                    0,
                    100000,
                    65536,
                ),
                ReadOnly,
            )
            .await?,
            &rootfs_dir,
        )
        .await?;
        tokio::fs::write(rootfs_dir.join("etc/machine-id"), format!("{machine_id}\n")).await?;
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
        let rpc_dir = rootfs_dir.join(RPC_DIR);
        tokio::fs::create_dir_all(&rpc_dir).await?;
        let rpc_bind = TmpMountGuard::mount(&Bind::new(rpc_dir), ReadWrite).await?;
        Command::new("chown")
            .arg("-R")
            .arg("100000:100000")
            .arg(rpc_bind.path())
            .invoke(ErrorKind::Filesystem)
            .await?;
        let log_mount = if let Some(path) = log_mount {
            let log_mount_point = rootfs_dir.join("var/log/journal").join(machine_id);
            let log_mount =
                MountGuard::mount(&Bind::new(path), &log_mount_point, MountType::ReadWrite).await?;
            Command::new("chown")
                // This was needed as 100999 because the group id of journald
                .arg("100000:100999")
                .arg(&log_mount_point)
                .invoke(crate::ErrorKind::Filesystem)
                .await?;
            Some(log_mount)
        } else {
            None
        };
        Command::new("lxc-start")
            .arg("-d")
            .arg("--name")
            .arg(&*guid)
            .invoke(ErrorKind::Lxc)
            .await?;
        Ok(Self {
            manager: Arc::downgrade(manager),
            rootfs,
            guid: Arc::new(ContainerId::try_from(&*guid)?),
            rpc_bind,
            config,
            exited: false,
            log_mount,
        })
    }

    pub fn rootfs_dir(&self) -> &Path {
        self.rootfs.path()
    }

    pub async fn ip(&self) -> Result<Ipv4Addr, Error> {
        let start = Instant::now();
        let guid: &str = &self.guid;
        loop {
            let output = String::from_utf8(
                Command::new("lxc-info")
                    .arg("--name")
                    .arg(guid)
                    .arg("-iH")
                    .invoke(ErrorKind::Docker)
                    .await?,
            )?;
            for line in output.lines() {
                if let Ok(ip) = line.trim().parse() {
                    return Ok(ip);
                }
            }
            if start.elapsed() > CONTAINER_DHCP_TIMEOUT {
                return Err(Error::new(
                    eyre!("Timed out waiting for container to acquire DHCP lease"),
                    ErrorKind::Timeout,
                ));
            }
            tokio::time::sleep(Duration::from_millis(100)).await;
        }
    }

    pub fn rpc_dir(&self) -> &Path {
        self.rpc_bind.path()
    }

    pub async fn command(&self, commands: &[&str]) -> Result<String, Error> {
        let mut cmd = Command::new("lxc-attach");
        cmd.kill_on_drop(true);

        let output = cmd
            .arg(&**self.guid)
            .arg("--")
            .args(commands)
            .output()
            .await?;

        if !output.status.success() {
            return Err(Error::new(
                eyre!(
                    "Command failed with exit code: {:?} \n Message: {:?}",
                    output.status.code(),
                    String::from_utf8(output.stderr)
                ),
                ErrorKind::Docker,
            ));
        }
        Ok(String::from_utf8(output.stdout)?)
    }

    #[instrument(skip_all)]
    pub async fn exit(mut self) -> Result<(), Error> {
        Command::new("lxc-stop")
            .arg("--name")
            .arg(&**self.guid)
            .invoke(ErrorKind::Lxc)
            .await?;
        self.rpc_bind.take().unmount().await?;
        if let Some(log_mount) = self.log_mount.take() {
            log_mount.unmount(true).await?;
        }
        self.rootfs.take().unmount(true).await?;
        let rootfs_path = self.rootfs_dir();
        if tokio::fs::metadata(&rootfs_path).await.is_ok()
            && tokio_stream::wrappers::ReadDirStream::new(tokio::fs::read_dir(&rootfs_path).await?)
                .count()
                .await
                > 0
        {
            return Err(Error::new(
                eyre!("rootfs is not empty, refusing to delete"),
                ErrorKind::InvalidRequest,
            ));
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
        let sock_path = self.rpc_dir().join(CONTAINER_RPC_SERVER_SOCKET);
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
                    if let Err(e) = async {
                        let err_path = rootfs.path().join("var/log/containerRuntime.err");
                        if tokio::fs::metadata(&err_path).await.is_ok() {
                            let mut lines = BufReader::new(open_file(&err_path).await?).lines();
                            while let Some(line) = lines.next_line().await? {
                                let container = &**guid;
                                tracing::error!(container, "{}", line);
                            }
                        }
                        Ok::<_, Error>(())
                    }
                    .await
                    {
                        tracing::error!("Error reading logs from crashed container: {e}");
                        tracing::debug!("{e:?}")
                    }
                    rootfs.unmount(true).await.log_err();
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
pub async fn connect(ctx: &RpcContext, container: &LxcContainer) -> Result<Guid, Error> {
    use axum::extract::ws::Message;

    let rpc = container.connect_rpc(Some(Duration::from_secs(30))).await?;
    let guid = Guid::new();
    ctx.rpc_continuations
        .add(
            guid.clone(),
            RpcContinuation::ws(
                |mut ws| async move {
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
                                        serde_json::to_string(&RpcResponse { id, result })
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
                },
                Duration::from_secs(30),
            ),
        )
        .await;
    Ok(guid)
}

pub async fn connect_cli(ctx: &CliContext, guid: Guid) -> Result<(), Error> {
    use futures::SinkExt;
    use tokio_tungstenite::tungstenite::Message;

    let mut ws = ctx.ws_continuation(guid).await?;
    let (mut input, mut output) =
        rustyline_async::Readline::new("> ".into()).with_kind(ErrorKind::Filesystem)?;

    async fn handle_message(
        msg: Option<Result<Message, tokio_tungstenite::tungstenite::Error>>,
        output: &mut SharedWriter,
    ) -> Result<bool, Error> {
        match msg {
            None => return Ok(true),
            Some(Ok(Message::Text(txt))) => match serde_json::from_str::<RpcResponse>(&txt) {
                Ok(RpcResponse { result: Ok(a), .. }) => {
                    output
                        .write_all(
                            (serde_json::to_string(&a).with_kind(ErrorKind::Serialization)? + "\n")
                                .as_bytes(),
                        )
                        .await?;
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
            },
            Some(Ok(_)) => (),
            Some(Err(e)) => {
                return Err(Error::new(e, ErrorKind::Network));
            }
        };
        Ok(false)
    }

    loop {
        tokio::select! {
            line = input.readline() => {
                let line = line.with_kind(ErrorKind::Filesystem)?;
                if let ReadlineEvent::Line(line) = line {
                    input.add_history_entry(line.clone());
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
                                        if let Some((name, value)) = arg.split_once('=') {
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
                                    if handle_message(ws.next().await, &mut output).await? {
                                        break
                                    }
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
                if handle_message(msg, &mut output).await? {
                    break;
                }
            }
        }
    }

    Ok(())
}
