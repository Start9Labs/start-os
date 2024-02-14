use std::collections::BTreeMap;
use std::path::Path;
use std::sync::{Arc, Weak};
use std::time::Duration;

use futures::future::ready;
use futures::Future;
use helpers::NonDetachingJoinHandle;
use imbl_value::InternedString;
use models::{ProcedureName, VolumeId};
use rpc_toolkit::{Empty, Server, ShutdownHandle};
use serde::de::DeserializeOwned;
use tokio::fs::File;
use tokio::process::Command;
use tokio::sync::{oneshot, watch, Mutex, OnceCell};
use tracing::instrument;

use super::service_effect_handler::{service_effect_handler, EffectContext};
use super::ServiceActorSeed;
use crate::context::RpcContext;
use crate::disk::mount::filesystem::bind::Bind;
use crate::disk::mount::filesystem::idmapped::IdMapped;
use crate::disk::mount::filesystem::loop_dev::LoopDev;
use crate::disk::mount::filesystem::overlayfs::OverlayGuard;
use crate::disk::mount::filesystem::{MountType, ReadOnly};
use crate::disk::mount::guard::{GenericMountGuard, MountGuard};
use crate::lxc::{LxcConfig, LxcContainer, HOST_RPC_SERVER_SOCKET};
use crate::prelude::*;
use crate::s9pk::merkle_archive::source::FileSource;
use crate::s9pk::S9pk;
use crate::service::start_stop::StartStop;
use crate::service::{rpc, RunningStatus};
use crate::util::rpc_client::UnixRpcClient;
use crate::util::Invoke;
use crate::volume::{asset_dir, data_dir};
use crate::ARCH;

const RPC_CONNECT_TIMEOUT: Duration = Duration::from_secs(10);

struct ProcedureId(u64);

// @DRB On top of this we need to also have  the procedures to have the effects and get the results back for them, maybe lock them to the running instance?
/// This contains the LXC container running the javascript init system
/// that can be used via a JSON RPC Client connected to a unix domain
/// socket served by the container
pub struct PersistentContainer {
    pub(super) s9pk: S9pk,
    pub(super) lxc_container: OnceCell<LxcContainer>,
    rpc_client: UnixRpcClient,
    pub(super) rpc_server: watch::Sender<Option<(NonDetachingJoinHandle<()>, ShutdownHandle)>>,
    // procedures: Mutex<Vec<(ProcedureName, ProcedureId)>>,
    js_mount: MountGuard,
    volumes: BTreeMap<VolumeId, MountGuard>,
    assets: BTreeMap<VolumeId, MountGuard>,
    pub(super) overlays: Arc<Mutex<BTreeMap<InternedString, OverlayGuard>>>,
    pub(super) current_state: watch::Sender<StartStop>,
    // pub(super) desired_state: watch::Receiver<StartStop>,
    // pub(super) temp_desired_state: watch::Receiver<Option<StartStop>>,
    pub(super) running_status: watch::Sender<Option<RunningStatus>>,
}

impl PersistentContainer {
    #[instrument(skip_all)]
    pub async fn new(
        ctx: &RpcContext,
        s9pk: S9pk,
        // desired_state: watch::Receiver<StartStop>,
        // temp_desired_state: watch::Receiver<Option<StartStop>>,
    ) -> Result<Self, Error> {
        let lxc_container = ctx.lxc_manager.create(LxcConfig::default()).await?;
        let rpc_client = lxc_container.connect_rpc(Some(RPC_CONNECT_TIMEOUT)).await?;
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
        let mut volumes = BTreeMap::new();
        for volume in &s9pk.as_manifest().volumes {
            let mount = MountGuard::mount(
                &IdMapped::new(
                    Bind::new(data_dir(&ctx.datadir, &s9pk.as_manifest().id, volume)),
                    0,
                    100000,
                    65536,
                ),
                lxc_container
                    .rootfs_dir()
                    .join("media/startos/volumes")
                    .join(volume),
                MountType::ReadWrite,
            )
            .await?;
            volumes.insert(volume.clone(), mount);
        }
        let mut assets = BTreeMap::new();
        for asset in &s9pk.as_manifest().assets {
            assets.insert(
                asset.clone(),
                MountGuard::mount(
                    &Bind::new(
                        asset_dir(
                            &ctx.datadir,
                            &s9pk.as_manifest().id,
                            &s9pk.as_manifest().version,
                        )
                        .join(asset),
                    ),
                    lxc_container
                        .rootfs_dir()
                        .join("media/startos/assets")
                        .join(asset),
                    MountType::ReadWrite,
                )
                .await?,
            );
        }
        let env_path = lxc_container.rootfs_dir().join("media/startos/env");
        tokio::fs::create_dir_all(&env_path).await?;
        for image in &s9pk.as_manifest().images {
            let filename = Path::new(image.as_ref()).with_extension("env");
            if let Some(env) = s9pk
                .as_archive()
                .contents()
                .get_path(Path::new("images").join(&*ARCH).join(&filename))
                .and_then(|e| e.as_file())
            {
                env.copy(&mut File::create(env_path.join(&filename)).await?)
                    .await?;
            }
        }
        Ok(Self {
            s9pk,
            lxc_container: OnceCell::new_with(Some(lxc_container)),
            rpc_client,
            rpc_server: watch::channel(None).0,
            // procedures: Default::default(),
            js_mount,
            volumes,
            assets,
            overlays: Arc::new(Mutex::new(BTreeMap::new())),
            current_state: watch::channel(StartStop::Stop).0,
            // desired_state,
            // temp_desired_state,
            running_status: watch::channel(None).0,
        })
    }

    #[instrument(skip_all)]
    pub async fn init(&self, seed: Weak<ServiceActorSeed>) -> Result<(), Error> {
        let socket_server_context = EffectContext::new(seed);
        let server = Server::new(
            move || ready(Ok(socket_server_context.clone())),
            service_effect_handler(),
        );
        let path = self
            .lxc_container
            .get()
            .ok_or_else(|| {
                Error::new(
                    eyre!("PersistentContainer has been destroyed"),
                    ErrorKind::Incoherent,
                )
            })?
            .rpc_dir()
            .join(HOST_RPC_SERVER_SOCKET);
        let (send, recv) = oneshot::channel();
        let handle = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let (shutdown, fut) = match async {
                let res = server.run_unix(&path, |err| {
                    tracing::error!("error on unix socket {}: {err}", path.display())
                })?;
                Command::new("chown")
                    .arg("100000:100000")
                    .arg(&path)
                    .invoke(ErrorKind::Filesystem)
                    .await?;
                Ok::<_, Error>(res)
            }
            .await
            {
                Ok((shutdown, fut)) => (Ok(shutdown), Some(fut)),
                Err(e) => (Err(e), None),
            };
            if send.send(shutdown).is_err() {
                panic!("failed to send shutdown handle");
            }
            if let Some(fut) = fut {
                fut.await;
            }
        }));
        let shutdown = recv.await.map_err(|_| {
            Error::new(
                eyre!("unix socket server thread panicked"),
                ErrorKind::Unknown,
            )
        })??;
        if self
            .rpc_server
            .send_replace(Some((handle, shutdown)))
            .is_some()
        {
            return Err(Error::new(
                eyre!("PersistentContainer already initialized"),
                ErrorKind::InvalidRequest,
            ));
        }

        self.rpc_client.request(rpc::Init, Empty {}).await?;

        Ok(())
    }

    #[instrument(skip_all)]
    fn destroy(&mut self) -> impl Future<Output = Result<(), Error>> + 'static {
        let rpc_client = self.rpc_client.clone();
        let rpc_server = self.rpc_server.send_replace(None);
        let js_mount = self.js_mount.take();
        let volumes = std::mem::take(&mut self.volumes);
        let assets = std::mem::take(&mut self.assets);
        let overlays = self.overlays.clone();
        let lxc_container = self.lxc_container.take();
        async move {
            let mut errs = ErrorCollection::new();
            errs.handle(dbg!(rpc_client.request(rpc::Exit, Empty {}).await));
            if let Some((hdl, shutdown)) = rpc_server {
                shutdown.shutdown();
                errs.handle(hdl.await.with_kind(ErrorKind::Cancelled));
            }
            for (_, volume) in volumes {
                errs.handle(volume.unmount(true).await);
            }
            for (_, assets) in assets {
                errs.handle(assets.unmount(true).await);
            }
            for (_, overlay) in std::mem::take(&mut *overlays.lock().await) {
                errs.handle(overlay.unmount(true).await);
            }
            errs.handle(js_mount.unmount(true).await);
            if let Some(lxc_container) = lxc_container {
                errs.handle(lxc_container.exit().await);
            }
            errs.into_result()
        }
    }

    #[instrument(skip_all)]
    pub async fn exit(mut self) -> Result<(), Error> {
        self.destroy().await?;

        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn start(&self) -> Result<(), Error> {
        self.execute(
            ProcedureName::StartMain,
            Value::Null,
            Some(Duration::from_secs(5)), // TODO
        )
        .await?;
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn stop(&self, timeout: Option<Duration>) -> Result<(), Error> {
        self.execute(ProcedureName::StopMain, Value::Null, timeout)
            .await?;
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn execute<O>(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<O, Error>
    where
        O: DeserializeOwned,
    {
        self._execute(name, input, timeout)
            .await
            .and_then(from_value)
    }

    #[instrument(skip_all)]
    pub async fn sanboxed<O>(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<O, Error>
    where
        O: DeserializeOwned,
    {
        self._sandboxed(name, input, timeout)
            .await
            .and_then(from_value)
    }

    #[instrument(skip_all)]
    async fn _execute(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Value, Error> {
        let fut = self
            .rpc_client
            .request(rpc::Execute, rpc::ExecuteParams::new(name, input, timeout));

        Ok(if let Some(timeout) = timeout {
            tokio::time::timeout(timeout, fut)
                .await
                .with_kind(ErrorKind::Timeout)??
        } else {
            fut.await?
        })
    }

    #[instrument(skip_all)]
    async fn _sandboxed(
        &self,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Value, Error> {
        let fut = self
            .rpc_client
            .request(rpc::Sandbox, rpc::ExecuteParams::new(name, input, timeout));

        Ok(if let Some(timeout) = timeout {
            tokio::time::timeout(timeout, fut)
                .await
                .with_kind(ErrorKind::Timeout)??
        } else {
            fut.await?
        })
    }
}

impl Drop for PersistentContainer {
    fn drop(&mut self) {
        let destroy = self.destroy();
        tokio::spawn(async move { destroy.await.unwrap() });
    }
}
