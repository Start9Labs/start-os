use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use futures::future::ready;
use helpers::{NonDetachingJoinHandle, UnixRpcClient};
use imbl_value::InternedString;
use models::ProcedureName;
use rpc_toolkit::{Server, ShutdownHandle};
use serde::de::DeserializeOwned;
use tokio::sync::{oneshot, watch, Mutex, OnceCell};
use tracing::instrument;

use super::service_effect_handler::{service_effect_handler, EffectContext};
use super::ServiceActorSeed;
use crate::context::RpcContext;
use crate::disk::mount::filesystem::loop_dev::LoopDev;
use crate::disk::mount::filesystem::overlayfs::OverlayGuard;
use crate::disk::mount::filesystem::ReadOnly;
use crate::disk::mount::guard::MountGuard;
use crate::lxc::{LxcConfig, LxcContainer, HOST_RPC_SERVER_SOCKET};
use crate::prelude::*;
use crate::s9pk::S9pk;
use crate::service::rpc::{self, convert_rpc_error, StopParams};
use crate::service::start_stop::StartStop;
use crate::service::RunningStatus;
use crate::{shutdown, ARCH};

const RPC_CONNECT_TIMEOUT: Duration = Duration::from_secs(10);

struct ProcedureId(u64);

// @DRB On top of this we need to also have  the procedures to have the effects and get the results back for them, maybe lock them to the running instance?
/// This contains the LXC container running the javascript init system
/// that can be used via a JSON RPC Client connected to a unix domain
/// socket served by the container
pub struct PersistentContainer {
    pub(super) s9pk: S9pk,
    pub(super) lxc_container: LxcContainer,
    rpc_client: UnixRpcClient,
    rpc_server: OnceCell<(NonDetachingJoinHandle<()>, ShutdownHandle)>,
    procedures: Mutex<Vec<(ProcedureName, ProcedureId)>>,
    js_mount: MountGuard,
    pub(super) overlays: Mutex<BTreeMap<InternedString, OverlayGuard>>,
    pub(super) current_state: watch::Sender<StartStop>,
    pub(super) desired_state: watch::Receiver<StartStop>,
    pub(super) temp_desired_state: watch::Receiver<Option<StartStop>>,
    pub(super) running_status: watch::Sender<Option<RunningStatus>>,
}

impl PersistentContainer {
    pub async fn new(
        ctx: &RpcContext,
        s9pk: S9pk,
        desired_state: watch::Receiver<StartStop>,
        temp_desired_state: watch::Receiver<Option<StartStop>>,
    ) -> Result<Self, Error> {
        let lxc_container = ctx.lxc_manager.create(LxcConfig::default()).await?;
        let js_mount = MountGuard::mount(
            &LoopDev::from(
                &**s9pk
                    .as_archive()
                    .contents()
                    .get_path("javascript.squashfs")
                    .and_then(|f| f.as_file())
                    .or_not_found("javascript")?,
            ),
            lxc_container.rootfs_dir().join("usr/lib/startos/package"),
            ReadOnly,
        )
        .await?;
        let rpc_client = lxc_container.connect_rpc(Some(RPC_CONNECT_TIMEOUT)).await?;
        Ok(Self {
            s9pk,
            lxc_container,
            rpc_client,
            rpc_server: OnceCell::new(),
            procedures: Default::default(),
            js_mount,
            overlays: Mutex::new(BTreeMap::new()),
            current_state: watch::channel(StartStop::Stop).0,
            desired_state,
            temp_desired_state,
            running_status: watch::channel(None).0,
        })
    }
    #[instrument(skip_all)]
    pub async fn init(&self, seed: Arc<ServiceActorSeed>) -> Result<(), Error> {
        let socket_server_context = EffectContext::new(seed);
        let server = Server::new(
            move || ready(Ok(socket_server_context.clone())),
            service_effect_handler(),
        );
        let path = self.lxc_container.rootfs_dir().join(HOST_RPC_SERVER_SOCKET);
        let (send, recv) = oneshot::channel();
        let handle = NonDetachingJoinHandle::spawn(async move {
            let (shutdown, fut) = match server.run_unix(&path, |err| {
                tracing::error!("error on unix socket {}: {e}", path.display())
            }) {
                Ok((shutdown, fut)) = (Ok(shutdown), Some(fut)),
                Err(e) => (Err(e), None),
            }
            send.send(shutdown);
            if let Some(fut) = fut {
                fut.await
            }
        });
        let shutdown = recv.await?;
        self.rpc_server
            .set(
                (handle, shutdown),
            )
            .map_err(|_| {
                Error::new(
                    eyre!("PersistentContainer already initialized"),
                    ErrorKind::InvalidRequest,
                )
            })?;
        // @todo @Blu-J @dr-bonez  Make it so the persistent cointainer uses as socket server for the effects.
        // let socket_server = run_unix(service_effect_handler(s9pk.as_manifest().id.clone()));

        self.rpc_client
            .request(rpc::Init, ())
            .await
            .map_err(convert_rpc_error)?;

        Ok(())
    }

    pub async fn exit(self) -> Result<(), Error> {
        let Self {
            rpc_client,
            js_mount,
            overlays,
            lxc_container,
            ..
        } = self;
        rpc_client
            .request(rpc::Exit, ())
            .await
            .map_err(convert_rpc_error)?;
        js_mount.unmount(true).await?;
        for (_, overlay) in std::mem::take(&mut *overlays.lock().await) {
            overlay.unmount(true).await?;
        }
        lxc_container.exit().await?;

        Ok(())
    }

    pub async fn start(&self) -> Result<(), Error> {
        self.rpc_client
            .request(rpc::Start, ())
            .await
            .map_err(convert_rpc_error)
    }

    pub async fn stop(&self, timeout: Option<Duration>) -> Result<(), Error> {
        self.rpc_client
            .request(rpc::Stop, StopParams::new(timeout))
            .await
            .map_err(convert_rpc_error)
    }

    pub async fn execute<O>(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error>
    where
        O: DeserializeOwned,
    {
        match self._execute(name, input, timeout).await {
            Ok(Ok(a)) => Ok(Ok(from_value(a)?)),
            Ok(Err(e)) => Ok(Err(e)),
            Err(e) => Err(e),
        }
    }

    pub async fn sanboxed<O>(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error>
    where
        O: DeserializeOwned,
    {
        match self._sandboxed(name, input, timeout).await {
            Ok(Ok(a)) => Ok(Ok(from_value(a)?)),
            Ok(Err(e)) => Ok(Err(e)),
            Err(e) => Err(e),
        }
    }

    async fn _execute(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Result<Value, (i32, String)>, Error> {
        let fut = self
            .rpc_client
            .request(rpc::Execute, rpc::ExecuteParams::new(name, input, timeout));

        Ok(if let Some(timeout) = timeout {
            tokio::time::timeout(timeout, fut)
                .await
                .with_kind(ErrorKind::Timeout)?
        } else {
            fut.await
        }
        .map_err(|e| (e.code, e.message.into_owned())))
    }

    async fn _sandboxed(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Result<Value, (i32, String)>, Error> {
        let fut = self
            .rpc_client
            .request(rpc::Sandbox, rpc::ExecuteParams::new(name, input, timeout));

        Ok(if let Some(timeout) = timeout {
            tokio::time::timeout(timeout, fut)
                .await
                .with_kind(ErrorKind::Timeout)?
        } else {
            fut.await
        }
        .map_err(|e| (e.code, e.message.into_owned())))
    }
}
