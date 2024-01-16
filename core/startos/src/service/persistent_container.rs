use std::path::Path;
use std::time::Duration;

use helpers::UnixRpcClient;
use models::ProcedureName;
use serde::de::DeserializeOwned;
use tokio::sync::{watch, Mutex};
use tracing::instrument;

use crate::context::RpcContext;
use crate::disk::mount::filesystem::loop_dev::LoopDev;
use crate::disk::mount::filesystem::ReadOnly;
use crate::disk::mount::guard::MountGuard;
use crate::lxc::{LxcConfig, LxcContainer};
use crate::prelude::*;
use crate::s9pk::S9pk;
use crate::service::rpc::{self, convert_rpc_error, StopParams};
use crate::service::start_stop::StartStop;
use crate::service::RunningStatus;
use crate::ARCH;

const RPC_CONNECT_TIMEOUT: Duration = Duration::from_secs(10);

struct ProcedureId(u64);

// @DRB On top of this we need to also have  the procedures to have the effects and get the results back for them, maybe lock them to the running instance?
/// This contains the LXC container running the javascript init system
/// that can be used via a JSON RPC Client connected to a unix domain
/// socket served by the container
pub struct PersistentContainer {
    s9pk: S9pk,
    lxc_container: LxcContainer,
    rpc_client: UnixRpcClient,
    procedures: Mutex<Vec<(ProcedureName, ProcedureId)>>,
    image_mounts: Vec<MountGuard>,
    js_mount: MountGuard,
    pub(super) current_state: watch::Sender<StartStop>,
    pub(super) desired_state: watch::Receiver<StartStop>,
    pub(super) temp_desired_state: watch::Receiver<Option<StartStop>>,
    pub(super) running_status: watch::Sender<Option<RunningStatus>>,
}

impl PersistentContainer {
    #[instrument(skip_all)]
    pub async fn init(
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
            lxc_container.rootfs_dir().join("usr/lib/javascript"),
            ReadOnly,
        )
        .await?;
        let mut image_mounts = Vec::new();
        for (image_id, contents) in s9pk
            .as_archive()
            .contents()
            .get_path(Path::new("images").join(&*ARCH))
            .and_then(|f| f.as_directory())
            .or_not_found("images for current architecture")?
            .iter()
            .filter_map(|(name, contents)| name.strip_suffix(".squashfs").map(|id| (id, contents)))
        {
            image_mounts.push(
                MountGuard::mount(
                    &LoopDev::from(&**contents.as_file().ok_or_else(|| {
                        Error::new(
                            eyre!("images/{}/{}.squashfs is not a file", &*ARCH, image_id),
                            ErrorKind::ParseS9pk,
                        )
                    })?),
                    lxc_container
                        .rootfs_dir()
                        .join("media/images")
                        .join(image_id),
                    ReadOnly,
                )
                .await?,
            );
        }
        let rpc_client = lxc_container.connect_rpc(Some(RPC_CONNECT_TIMEOUT)).await?;
        rpc_client
            .request(rpc::Init, ())
            .await
            .map_err(convert_rpc_error)?;

        Ok(Self {
            s9pk,
            lxc_container,
            rpc_client,
            procedures: Default::default(),
            image_mounts,
            js_mount,
            current_state: watch::channel(StartStop::Stop).0,
            desired_state,
            temp_desired_state,
            running_status: watch::channel(None).0,
        })
    }

    pub async fn exit(self) -> Result<(), Error> {
        let Self {
            rpc_client,
            js_mount,
            image_mounts,
            lxc_container,
            ..
        } = self;
        rpc_client
            .request(rpc::Exit, ())
            .await
            .map_err(convert_rpc_error)?;
        js_mount.unmount(true).await?;
        for image_mount in image_mounts {
            image_mount.unmount(true).await?;
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
