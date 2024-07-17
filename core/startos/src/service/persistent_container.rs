use std::collections::BTreeMap;
use std::path::Path;
use std::sync::{Arc, Weak};
use std::time::Duration;

use futures::future::ready;
use futures::{Future, FutureExt};
use helpers::NonDetachingJoinHandle;
use models::{ImageId, ProcedureName, VolumeId};
use rpc_toolkit::{Empty, Server, ShutdownHandle};
use serde::de::DeserializeOwned;
use tokio::process::Command;
use tokio::sync::{oneshot, watch, Mutex, OnceCell};
use tracing::instrument;

use super::service_effect_handler::{service_effect_handler, EffectContext};
use super::transition::{TransitionKind, TransitionState};
use crate::context::RpcContext;
use crate::disk::mount::filesystem::bind::Bind;
use crate::disk::mount::filesystem::idmapped::IdMapped;
use crate::disk::mount::filesystem::loop_dev::LoopDev;
use crate::disk::mount::filesystem::overlayfs::OverlayGuard;
use crate::disk::mount::filesystem::{MountType, ReadOnly};
use crate::disk::mount::guard::{GenericMountGuard, MountGuard};
use crate::lxc::{LxcConfig, LxcContainer, HOST_RPC_SERVER_SOCKET};
use crate::net::net_controller::NetService;
use crate::prelude::*;
use crate::rpc_continuations::Guid;
use crate::s9pk::merkle_archive::source::FileSource;
use crate::s9pk::S9pk;
use crate::service::start_stop::StartStop;
use crate::service::{rpc, RunningStatus, Service};
use crate::util::io::create_file;
use crate::util::rpc_client::UnixRpcClient;
use crate::util::Invoke;
use crate::volume::{asset_dir, data_dir};
use crate::ARCH;

const RPC_CONNECT_TIMEOUT: Duration = Duration::from_secs(10);

#[derive(Debug)]
pub struct ServiceState {
    // This contains the start time and health check information for when the service is running. Note: Will be overwritting to the db,
    pub(super) running_status: Option<RunningStatus>,
    /// Setting this value causes the service actor to try to bring the service to the specified state. This is done in the background job created in ServiceActor::init
    pub(super) desired_state: StartStop,
    /// Override the current desired state for the service during a transition (this is protected by a guard that sets this value to null on drop)
    pub(super) temp_desired_state: Option<StartStop>,
    /// This represents a currently running task that affects the service's shown state, such as BackingUp or Restarting.
    pub(super) transition_state: Option<TransitionState>,
}

#[derive(Debug)]
pub struct ServiceStateKinds {
    pub transition_state: Option<TransitionKind>,
    pub running_status: Option<RunningStatus>,
    pub desired_state: StartStop,
}

impl ServiceState {
    pub fn new(desired_state: StartStop) -> Self {
        Self {
            running_status: Default::default(),
            temp_desired_state: Default::default(),
            transition_state: Default::default(),
            desired_state,
        }
    }
    pub fn kinds(&self) -> ServiceStateKinds {
        ServiceStateKinds {
            transition_state: self.transition_state.as_ref().map(|x| x.kind()),
            desired_state: self.temp_desired_state.unwrap_or(self.desired_state),
            running_status: self.running_status.clone(),
        }
    }
}

// @DRB On top of this we need to also have  the procedures to have the effects and get the results back for them, maybe lock them to the running instance?
/// This contains the LXC container running the javascript init system
/// that can be used via a JSON RPC Client connected to a unix domain
/// socket served by the container
pub struct PersistentContainer {
    pub(super) s9pk: S9pk,
    pub(super) lxc_container: OnceCell<LxcContainer>,
    pub(super) rpc_client: UnixRpcClient,
    pub(super) rpc_server: watch::Sender<Option<(NonDetachingJoinHandle<()>, ShutdownHandle)>>,
    // procedures: Mutex<Vec<(ProcedureName, ProcedureId)>>,
    js_mount: MountGuard,
    volumes: BTreeMap<VolumeId, MountGuard>,
    assets: BTreeMap<VolumeId, MountGuard>,
    pub(super) images: BTreeMap<ImageId, Arc<MountGuard>>,
    pub(super) overlays: Arc<Mutex<BTreeMap<Guid, OverlayGuard<Arc<MountGuard>>>>>,
    pub(super) state: Arc<watch::Sender<ServiceState>>,
    pub(super) net_service: Mutex<NetService>,
    destroyed: bool,
}

impl PersistentContainer {
    #[instrument(skip_all)]
    pub async fn new(ctx: &RpcContext, s9pk: S9pk, start: StartStop) -> Result<Self, Error> {
        let lxc_container = ctx
            .lxc_manager
            .create(
                Some(
                    &ctx.datadir
                        .join("package-data/logs")
                        .join(&s9pk.as_manifest().id),
                ),
                LxcConfig::default(),
            )
            .await?;
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
            let mountpoint = lxc_container
                .rootfs_dir()
                .join("media/startos/volumes")
                .join(volume);
            tokio::fs::create_dir_all(&mountpoint).await?;
            Command::new("chown")
                .arg("100000:100000")
                .arg(&mountpoint)
                .invoke(crate::ErrorKind::Filesystem)
                .await?;
            let mount = MountGuard::mount(
                &IdMapped::new(
                    Bind::new(data_dir(&ctx.datadir, &s9pk.as_manifest().id, volume)),
                    0,
                    100000,
                    65536,
                ),
                mountpoint,
                MountType::ReadWrite,
            )
            .await?;
            volumes.insert(volume.clone(), mount);
        }
        let mut assets = BTreeMap::new();
        for asset in &s9pk.as_manifest().assets {
            let mountpoint = lxc_container
                .rootfs_dir()
                .join("media/startos/assets")
                .join(asset);
            tokio::fs::create_dir_all(&mountpoint).await?;
            Command::new("chown")
                .arg("100000:100000")
                .arg(&mountpoint)
                .invoke(crate::ErrorKind::Filesystem)
                .await?;
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
                    mountpoint,
                    MountType::ReadWrite,
                )
                .await?,
            );
        }

        let mut images = BTreeMap::new();
        let image_path = lxc_container.rootfs_dir().join("media/startos/images");
        tokio::fs::create_dir_all(&image_path).await?;
        for (image, config) in &s9pk.as_manifest().images {
            let mut arch = ARCH;
            let mut sqfs_path = Path::new("images")
                .join(arch)
                .join(image)
                .with_extension("squashfs");
            if !s9pk
                .as_archive()
                .contents()
                .get_path(&sqfs_path)
                .and_then(|e| e.as_file())
                .is_some()
            {
                arch = if let Some(arch) = config.emulate_missing_as.as_deref() {
                    arch
                } else {
                    continue;
                };
                sqfs_path = Path::new("images")
                    .join(arch)
                    .join(image)
                    .with_extension("squashfs");
            }
            let sqfs = s9pk
                .as_archive()
                .contents()
                .get_path(&sqfs_path)
                .and_then(|e| e.as_file())
                .or_not_found(sqfs_path.display())?;
            let mountpoint = image_path.join(image);
            tokio::fs::create_dir_all(&mountpoint).await?;
            Command::new("chown")
                .arg("100000:100000")
                .arg(&mountpoint)
                .invoke(ErrorKind::Filesystem)
                .await?;
            images.insert(
                image.clone(),
                Arc::new(
                    MountGuard::mount(
                        &IdMapped::new(LoopDev::from(&**sqfs), 0, 100000, 65536),
                        &mountpoint,
                        ReadOnly,
                    )
                    .await?,
                ),
            );
            let env_filename = Path::new(image.as_ref()).with_extension("env");
            if let Some(env) = s9pk
                .as_archive()
                .contents()
                .get_path(Path::new("images").join(arch).join(&env_filename))
                .and_then(|e| e.as_file())
            {
                env.copy(&mut create_file(image_path.join(&env_filename)).await?)
                    .await?;
            }
            let json_filename = Path::new(image.as_ref()).with_extension("json");
            if let Some(json) = s9pk
                .as_archive()
                .contents()
                .get_path(Path::new("images").join(arch).join(&json_filename))
                .and_then(|e| e.as_file())
            {
                json.copy(&mut create_file(image_path.join(&json_filename)).await?)
                    .await?;
            }
        }
        let net_service = ctx
            .net_controller
            .create_service(s9pk.as_manifest().id.clone(), lxc_container.ip().await?)
            .await?;
        Ok(Self {
            s9pk,
            lxc_container: OnceCell::new_with(Some(lxc_container)),
            rpc_client,
            rpc_server: watch::channel(None).0,
            // procedures: Default::default(),
            js_mount,
            volumes,
            assets,
            images,
            overlays: Arc::new(Mutex::new(BTreeMap::new())),
            state: Arc::new(watch::channel(ServiceState::new(start)).0),
            net_service: Mutex::new(net_service),
            destroyed: false,
        })
    }

    #[instrument(skip_all)]
    pub async fn mount_backup(
        &self,
        backup_path: impl AsRef<Path>,
        mount_type: MountType,
    ) -> Result<MountGuard, Error> {
        let backup_path = backup_path.as_ref();
        let mountpoint = self
            .lxc_container
            .get()
            .ok_or_else(|| {
                Error::new(
                    eyre!("PersistentContainer has been destroyed"),
                    ErrorKind::Incoherent,
                )
            })?
            .rootfs_dir()
            .join("media/startos/backup");
        tokio::fs::create_dir_all(&mountpoint).await?;
        Command::new("chown")
            .arg("100000:100000")
            .arg(mountpoint.as_os_str())
            .invoke(ErrorKind::Filesystem)
            .await?;
        tokio::fs::create_dir_all(backup_path).await?;
        Command::new("chown")
            .arg("100000:100000")
            .arg(backup_path)
            .invoke(ErrorKind::Filesystem)
            .await?;
        let bind = Bind::new(backup_path);
        MountGuard::mount(&bind, &mountpoint, mount_type).await
    }

    #[instrument(skip_all)]
    pub async fn init(&self, seed: Weak<Service>) -> Result<(), Error> {
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
            let chown_status = async {
                let res = server.run_unix(&path, |err| {
                    tracing::error!("error on unix socket {}: {err}", path.display())
                })?;
                Command::new("chown")
                    .arg("100000:100000")
                    .arg(&path)
                    .invoke(ErrorKind::Filesystem)
                    .await?;
                Ok::<_, Error>(res)
            };
            let (shutdown, fut) = match chown_status.await {
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
    fn destroy(&mut self) -> Option<impl Future<Output = Result<(), Error>> + 'static> {
        if self.destroyed {
            return None;
        }
        let rpc_client = self.rpc_client.clone();
        let rpc_server = self.rpc_server.send_replace(None);
        let js_mount = self.js_mount.take();
        let volumes = std::mem::take(&mut self.volumes);
        let assets = std::mem::take(&mut self.assets);
        let images = std::mem::take(&mut self.images);
        let overlays = self.overlays.clone();
        let lxc_container = self.lxc_container.take();
        self.destroyed = true;
        Some(
            async move {
                dbg!(
                    async move {
                        let mut errs = ErrorCollection::new();
                        if let Some((hdl, shutdown)) = rpc_server {
                            errs.handle(rpc_client.request(rpc::Exit, Empty {}).await);
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
                        for (_, images) in images {
                            errs.handle(images.unmount().await);
                        }
                        errs.handle(js_mount.unmount(true).await);
                        if let Some(lxc_container) = lxc_container {
                            errs.handle(lxc_container.exit().await);
                        }
                        dbg!(errs.into_result())
                    }
                    .await
                )
            }
            .map(|a| dbg!(a)),
        )
    }

    #[instrument(skip_all)]
    pub async fn exit(mut self) -> Result<(), Error> {
        if let Some(destroy) = self.destroy() {
            dbg!(destroy.await)?;
        }
        tracing::info!("Service for {} exited", self.s9pk.as_manifest().id);

        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn start(&self) -> Result<(), Error> {
        self.execute(
            Guid::new(),
            ProcedureName::StartMain,
            Value::Null,
            Some(Duration::from_secs(5)), // TODO
        )
        .await?;
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn stop(&self) -> Result<Duration, Error> {
        let timeout: Option<crate::util::serde::Duration> = self
            .execute(Guid::new(), ProcedureName::StopMain, Value::Null, None)
            .await?;
        Ok(timeout.map(|a| *a).unwrap_or(Duration::from_secs(30)))
    }

    #[instrument(skip_all)]
    pub async fn execute<O>(
        &self,
        id: Guid,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<O, Error>
    where
        O: DeserializeOwned,
    {
        self._execute(id, name, input, timeout)
            .await
            .and_then(from_value)
    }

    #[instrument(skip_all)]
    pub async fn sanboxed<O>(
        &self,
        id: Guid,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<O, Error>
    where
        O: DeserializeOwned,
    {
        self._sandboxed(id, name, input, timeout)
            .await
            .and_then(from_value)
    }

    #[instrument(skip_all)]
    async fn _execute(
        &self,
        id: Guid,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Value, Error> {
        let fut = self.rpc_client.request(
            rpc::Execute,
            rpc::ExecuteParams::new(id, name, input, timeout),
        );

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
        id: Guid,
        name: ProcedureName,
        input: Value,
        timeout: Option<Duration>,
    ) -> Result<Value, Error> {
        let fut = self.rpc_client.request(
            rpc::Sandbox,
            rpc::ExecuteParams::new(id, name, input, timeout),
        );

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
        if let Some(destroy) = self.destroy() {
            tokio::spawn(async move { destroy.await.unwrap() });
        }
    }
}
