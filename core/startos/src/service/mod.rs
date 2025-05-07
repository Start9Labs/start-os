use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsString;
use std::io::IsTerminal;
use std::ops::Deref;
use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::sync::{Arc, Weak};
use std::time::Duration;

use axum::extract::ws::WebSocket;
use chrono::{DateTime, Utc};
use clap::Parser;
use futures::future::BoxFuture;
use futures::stream::FusedStream;
use futures::{FutureExt, SinkExt, StreamExt, TryStreamExt};
use helpers::NonDetachingJoinHandle;
use imbl_value::{json, InternedString};
use itertools::Itertools;
use models::{ActionId, HostId, ImageId, PackageId, ProcedureName};
use nix::sys::signal::Signal;
use persistent_container::{PersistentContainer, Subcontainer};
use rpc_toolkit::{from_fn_async, CallRemoteHandler, Empty, HandlerArgs, HandlerFor};
use serde::{Deserialize, Serialize};
use service_actor::ServiceActor;
use start_stop::StartStop;
use termion::raw::IntoRawMode;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::process::Command;
use tokio::sync::Notify;
use tokio_tungstenite::tungstenite::protocol::frame::coding::CloseCode;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::db::model::package::{
    InstalledState, PackageDataEntry, PackageState, PackageStateMatchModelRef, UpdatingState,
};
use crate::disk::mount::guard::GenericMountGuard;
use crate::install::PKG_ARCHIVE_DIR;
use crate::lxc::ContainerId;
use crate::prelude::*;
use crate::progress::{NamedProgress, Progress};
use crate::rpc_continuations::{Guid, RpcContinuation};
use crate::s9pk::S9pk;
use crate::service::action::update_requested_actions;
use crate::service::service_map::InstallProgressHandles;
use crate::util::actor::concurrent::ConcurrentActor;
use crate::util::io::{create_file, AsyncReadStream, TermSize};
use crate::util::net::WebSocketExt;
use crate::util::serde::{NoOutput, Pem};
use crate::util::Never;
use crate::volume::data_dir;
use crate::{CAP_1_KiB, DATA_DIR, PACKAGE_DATA};

pub mod action;
pub mod cli;
mod control;
pub mod effects;
pub mod persistent_container;
mod rpc;
mod service_actor;
pub mod service_map;
pub mod start_stop;
mod transition;
mod util;

pub use service_map::ServiceMap;

pub const HEALTH_CHECK_COOLDOWN_SECONDS: u64 = 15;
pub const HEALTH_CHECK_GRACE_PERIOD_SECONDS: u64 = 5;
pub const SYNC_RETRY_COOLDOWN_SECONDS: u64 = 10;

pub type Task<'a> = BoxFuture<'a, Result<(), Error>>;

/// TODO
pub enum BackupReturn {
    TODO,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LoadDisposition {
    Retry,
    Undo,
}

struct RootCommand(pub String);

#[derive(Clone, Debug, Serialize, Deserialize, Default, TS)]
pub struct MiB(pub u64);

impl MiB {
    fn new(value: u64) -> Self {
        Self(value / 1024 / 1024)
    }
    fn from_MiB(value: u64) -> Self {
        Self(value)
    }
}

impl std::fmt::Display for MiB {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} MiB", self.0)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, Default, TS)]
pub struct ServiceStats {
    pub container_id: Arc<ContainerId>,
    pub package_id: PackageId,
    pub memory_usage: MiB,
    pub memory_limit: MiB,
}

pub struct ServiceRef(Arc<Service>);
impl ServiceRef {
    pub fn weak(&self) -> Weak<Service> {
        Arc::downgrade(&self.0)
    }
    pub async fn uninstall(
        self,
        target_version: Option<models::VersionString>,
        soft: bool,
        force: bool,
    ) -> Result<(), Error> {
        let uninit_res = self
            .seed
            .persistent_container
            .execute::<NoOutput>(
                Guid::new(),
                ProcedureName::PackageUninit,
                to_value(&target_version)?,
                None,
            ) // TODO timeout
            .await;
        if force {
            uninit_res.log_err();
        } else {
            uninit_res?;
        }
        let id = self.seed.persistent_container.s9pk.as_manifest().id.clone();
        let ctx = self.seed.ctx.clone();
        self.shutdown().await?;

        if target_version.is_none() {
            if let Some(pde) = ctx
                .db
                .mutate(|d| {
                    if let Some(pde) = d
                        .as_public_mut()
                        .as_package_data_mut()
                        .remove(&id)?
                        .map(|d| d.de())
                        .transpose()?
                    {
                        d.as_private_mut().as_available_ports_mut().mutate(|p| {
                            p.free(
                                pde.hosts
                                    .0
                                    .values()
                                    .flat_map(|h| h.bindings.values())
                                    .flat_map(|b| {
                                        b.net
                                            .assigned_port
                                            .into_iter()
                                            .chain(b.net.assigned_ssl_port)
                                    }),
                            );
                            Ok(())
                        })?;
                        d.as_private_mut().as_package_stores_mut().remove(&id)?;
                        Ok(Some(pde))
                    } else {
                        Ok(None)
                    }
                })
                .await
                .result?
            {
                let state = pde.state_info.expect_removing()?;
                if !soft {
                    for volume_id in &state.manifest.volumes {
                        let path = data_dir(DATA_DIR, &state.manifest.id, volume_id);
                        if tokio::fs::metadata(&path).await.is_ok() {
                            tokio::fs::remove_dir_all(&path).await?;
                        }
                    }
                    let logs_dir = Path::new(PACKAGE_DATA)
                        .join("logs")
                        .join(&state.manifest.id);
                    if tokio::fs::metadata(&logs_dir).await.is_ok() {
                        tokio::fs::remove_dir_all(&logs_dir).await?;
                    }
                    let archive_path = Path::new(PACKAGE_DATA)
                        .join("archive")
                        .join("installed")
                        .join(&state.manifest.id);
                    if tokio::fs::metadata(&archive_path).await.is_ok() {
                        tokio::fs::remove_file(&archive_path).await?;
                    }
                }
            }
        }
        Ok(())
    }
    pub async fn shutdown(self) -> Result<(), Error> {
        if let Some((hdl, shutdown)) = self.seed.persistent_container.rpc_server.send_replace(None)
        {
            self.seed
                .persistent_container
                .rpc_client
                .request(rpc::Exit, Empty {})
                .await?;
            shutdown.shutdown();
            tokio::time::timeout(Duration::from_secs(30), hdl)
                .await
                .with_kind(ErrorKind::Timeout)?
                .with_kind(ErrorKind::Cancelled)?;
        }
        let service = Arc::try_unwrap(self.0).map_err(|_| {
            Error::new(
                eyre!("ServiceActor held somewhere after actor shutdown"),
                ErrorKind::Unknown,
            )
        })?;
        service
            .actor
            .shutdown(crate::util::actor::PendingMessageStrategy::FinishAll { timeout: None }) // TODO timeout
            .await;
        Arc::try_unwrap(service.seed)
            .map_err(|_| {
                Error::new(
                    eyre!("ServiceActorSeed held somewhere after actor shutdown"),
                    ErrorKind::Unknown,
                )
            })?
            .persistent_container
            .exit()
            .await?;
        Ok(())
    }
}
impl Deref for ServiceRef {
    type Target = Service;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl From<Service> for ServiceRef {
    fn from(value: Service) -> Self {
        Self(Arc::new(value))
    }
}

pub struct Service {
    actor: ConcurrentActor<ServiceActor>,
    seed: Arc<ServiceActorSeed>,
}
impl Service {
    #[instrument(skip_all)]
    async fn new(ctx: RpcContext, s9pk: S9pk, start: StartStop) -> Result<ServiceRef, Error> {
        let id = s9pk.as_manifest().id.clone();
        let persistent_container = PersistentContainer::new(
            &ctx, s9pk,
            start,
            // desired_state.subscribe(),
            // temp_desired_state.subscribe(),
        )
        .await?;
        let seed = Arc::new(ServiceActorSeed {
            id,
            persistent_container,
            ctx,
            synchronized: Arc::new(Notify::new()),
        });
        let service: ServiceRef = Self {
            actor: ConcurrentActor::new(ServiceActor(seed.clone())),
            seed,
        }
        .into();
        service
            .seed
            .persistent_container
            .init(service.weak())
            .await?;
        Ok(service)
    }

    #[instrument(skip_all)]
    pub async fn load(
        ctx: &RpcContext,
        id: &PackageId,
        disposition: LoadDisposition,
    ) -> Result<Option<ServiceRef>, Error> {
        let handle_installed = {
            let ctx = ctx.clone();
            move |s9pk: S9pk, i: Model<PackageDataEntry>| async move {
                for volume_id in &s9pk.as_manifest().volumes {
                    let path = data_dir(DATA_DIR, &s9pk.as_manifest().id, volume_id);
                    if tokio::fs::metadata(&path).await.is_err() {
                        tokio::fs::create_dir_all(&path).await?;
                    }
                }
                let start_stop = if i.as_status().de()?.running() {
                    StartStop::Start
                } else {
                    StartStop::Stop
                };
                Self::new(ctx, s9pk, start_stop).await.map(Some)
            }
        };
        let s9pk_dir = Path::new(DATA_DIR).join(PKG_ARCHIVE_DIR).join("installed"); // TODO: make this based on hash
        let s9pk_path = s9pk_dir.join(id).with_extension("s9pk");
        let Some(entry) = ctx
            .db
            .peek()
            .await
            .into_public()
            .into_package_data()
            .into_idx(id)
        else {
            return Ok(None);
        };
        match entry.as_state_info().as_match() {
            PackageStateMatchModelRef::Installing(_) => {
                if disposition == LoadDisposition::Retry {
                    if let Ok(s9pk) = S9pk::open(s9pk_path, Some(id)).await.map_err(|e| {
                        tracing::error!("Error opening s9pk for install: {e}");
                        tracing::debug!("{e:?}")
                    }) {
                        if let Ok(service) =
                            Self::install(ctx.clone(), s9pk, None, None::<Never>, None)
                                .await
                                .map_err(|e| {
                                    tracing::error!("Error installing service: {e}");
                                    tracing::debug!("{e:?}")
                                })
                        {
                            return Ok(Some(service));
                        }
                    }
                }
                // TODO: delete s9pk?
                ctx.db
                    .mutate(|v| v.as_public_mut().as_package_data_mut().remove(id))
                    .await
                    .result?;
                Ok(None)
            }
            PackageStateMatchModelRef::Updating(s) => {
                if disposition == LoadDisposition::Retry
                    && s.as_installing_info()
                        .as_progress()
                        .de()?
                        .phases
                        .iter()
                        .any(|NamedProgress { name, progress }| {
                            name.eq_ignore_ascii_case("download")
                                && progress == &Progress::Complete(true)
                        })
                {
                    if let Ok(s9pk) = S9pk::open(&s9pk_path, Some(id)).await.map_err(|e| {
                        tracing::error!("Error opening s9pk for update: {e}");
                        tracing::debug!("{e:?}")
                    }) {
                        if let Ok(service) = Self::install(
                            ctx.clone(),
                            s9pk,
                            Some(s.as_manifest().as_version().de()?),
                            None::<Never>,
                            None,
                        )
                        .await
                        .map_err(|e| {
                            tracing::error!("Error installing service: {e}");
                            tracing::debug!("{e:?}")
                        }) {
                            return Ok(Some(service));
                        }
                    }
                }
                let s9pk = S9pk::open(s9pk_path, Some(id)).await?;
                ctx.db
                    .mutate({
                        |db| {
                            db.as_public_mut()
                                .as_package_data_mut()
                                .as_idx_mut(id)
                                .or_not_found(id)?
                                .as_state_info_mut()
                                .map_mutate(|s| {
                                    if let PackageState::Updating(UpdatingState {
                                        manifest, ..
                                    }) = s
                                    {
                                        Ok(PackageState::Installed(InstalledState { manifest }))
                                    } else {
                                        Err(Error::new(eyre!("Race condition detected - package state changed during load"), ErrorKind::Database))
                                    }
                                })
                        }
                    })
                    .await.result?;
                handle_installed(s9pk, entry).await
            }
            PackageStateMatchModelRef::Removing(_) | PackageStateMatchModelRef::Restoring(_) => {
                if let Ok(s9pk) = S9pk::open(s9pk_path, Some(id)).await.map_err(|e| {
                    tracing::error!("Error opening s9pk for removal: {e}");
                    tracing::debug!("{e:?}")
                }) {
                    if let Ok(service) = Self::new(ctx.clone(), s9pk, StartStop::Stop)
                        .await
                        .map_err(|e| {
                            tracing::error!("Error loading service for removal: {e}");
                            tracing::debug!("{e:?}")
                        })
                    {
                        match service.uninstall(None, false, false).await {
                            Err(e) => {
                                tracing::error!("Error uninstalling service: {e}");
                                tracing::debug!("{e:?}")
                            }
                            Ok(()) => return Ok(None),
                        }
                    }
                }

                ctx.db
                    .mutate(|v| v.as_public_mut().as_package_data_mut().remove(id))
                    .await
                    .result?;

                Ok(None)
            }
            PackageStateMatchModelRef::Installed(_) => {
                handle_installed(S9pk::open(s9pk_path, Some(id)).await?, entry).await
            }
            PackageStateMatchModelRef::Error(e) => Err(Error::new(
                eyre!("Failed to parse PackageDataEntry, found {e:?}"),
                ErrorKind::Deserialization,
            )),
        }
    }

    #[instrument(skip_all)]
    pub async fn install(
        ctx: RpcContext,
        s9pk: S9pk,
        mut src_version: Option<models::VersionString>,
        recovery_source: Option<impl GenericMountGuard>,
        progress: Option<InstallProgressHandles>,
    ) -> Result<ServiceRef, Error> {
        let manifest = s9pk.as_manifest().clone();
        let developer_key = s9pk.as_archive().signer();
        let icon = s9pk.icon_data_url().await?;
        let service = Self::new(ctx.clone(), s9pk, StartStop::Stop).await?;

        if let Some(recovery_source) = recovery_source {
            service
                .actor
                .send(
                    Guid::new(),
                    transition::restore::Restore {
                        path: recovery_source.path().to_path_buf(),
                    },
                )
                .await??;
            recovery_source.unmount().await?;
            src_version = Some(
                service
                    .seed
                    .persistent_container
                    .s9pk
                    .as_manifest()
                    .version
                    .clone(),
            );
        }

        let procedure_id = Guid::new();
        service
            .seed
            .persistent_container
            .execute::<NoOutput>(
                procedure_id.clone(),
                ProcedureName::PackageInit,
                to_value(&src_version)?,
                None,
            ) // TODO timeout
            .await
            .with_kind(if src_version.is_some() {
                ErrorKind::UpdateFailed
            } else {
                ErrorKind::InstallFailed
            })?; // TODO: handle cancellation

        if let Some(mut progress) = progress {
            progress.finalization_progress.complete();
            progress.progress.complete();
            tokio::task::yield_now().await;
        }

        let peek = ctx.db.peek().await;
        let mut action_input: BTreeMap<ActionId, Value> = BTreeMap::new();
        let requested_actions: BTreeSet<_> = peek
            .as_public()
            .as_package_data()
            .as_entries()?
            .into_iter()
            .map(|(_, pde)| {
                Ok(pde
                    .as_requested_actions()
                    .as_entries()?
                    .into_iter()
                    .map(|(_, r)| {
                        Ok::<_, Error>(if r.as_request().as_package_id().de()? == manifest.id {
                            Some(r.as_request().as_action_id().de()?)
                        } else {
                            None
                        })
                    })
                    .filter_map_ok(|a| a))
            })
            .flatten_ok()
            .map(|a| a.and_then(|a| a))
            .try_collect()?;
        for action_id in requested_actions {
            if let Some(input) = service
                .get_action_input(procedure_id.clone(), action_id.clone())
                .await?
                .and_then(|i| i.value)
            {
                action_input.insert(action_id, input);
            }
        }
        ctx.db
            .mutate(|db| {
                for (action_id, input) in &action_input {
                    for (_, pde) in db.as_public_mut().as_package_data_mut().as_entries_mut()? {
                        pde.as_requested_actions_mut().mutate(|requested_actions| {
                            Ok(update_requested_actions(
                                requested_actions,
                                &manifest.id,
                                action_id,
                                input,
                                false,
                            ))
                        })?;
                    }
                }
                let entry = db
                    .as_public_mut()
                    .as_package_data_mut()
                    .as_idx_mut(&manifest.id)
                    .or_not_found(&manifest.id)?;
                entry
                    .as_state_info_mut()
                    .ser(&PackageState::Installed(InstalledState { manifest }))?;
                entry.as_developer_key_mut().ser(&Pem::new(developer_key))?;
                entry.as_icon_mut().ser(&icon)?;
                // TODO: marketplace url
                // TODO: dependency info

                Ok(())
            })
            .await
            .result?;

        Ok(service)
    }

    #[instrument(skip_all)]
    pub async fn backup(&self, guard: impl GenericMountGuard) -> Result<(), Error> {
        let id = &self.seed.id;
        let mut file = create_file(guard.path().join(id).with_extension("s9pk")).await?;
        self.seed
            .persistent_container
            .s9pk
            .clone()
            .serialize(&mut file, true)
            .await?;
        drop(file);
        self.actor
            .send(
                Guid::new(),
                transition::backup::Backup {
                    path: guard.path().to_path_buf(),
                },
            )
            .await??
            .await?;
        Ok(())
    }

    pub fn container_id(&self) -> Result<ContainerId, Error> {
        let id = &self.seed.id;
        let container_id = (*self
            .seed
            .persistent_container
            .lxc_container
            .get()
            .or_not_found(format!("container for {id}"))?
            .guid)
            .clone();
        Ok(container_id)
    }
    #[instrument(skip_all)]
    pub async fn stats(&self) -> Result<ServiceStats, Error> {
        let container = &self.seed.persistent_container;
        let lxc_container = container.lxc_container.get().or_not_found("container")?;
        let (total, used) = lxc_container
            .command(&["free", "-m"])
            .await?
            .split("\n")
            .map(|x| x.split_whitespace().collect::<Vec<_>>())
            .skip(1)
            .filter_map(|x| {
                Some((
                    x.get(1)?.parse::<u64>().ok()?,
                    x.get(2)?.parse::<u64>().ok()?,
                ))
            })
            .fold((0, 0), |acc, (total, used)| (acc.0 + total, acc.1 + used));
        Ok(ServiceStats {
            container_id: lxc_container.guid.clone(),
            package_id: self.seed.id.clone(),
            memory_limit: MiB::from_MiB(total),
            memory_usage: MiB::from_MiB(used),
        })
    }

    pub async fn sync_host(&self, host_id: HostId) -> Result<(), Error> {
        self.seed
            .persistent_container
            .net_service
            .sync_host(host_id)
            .await
    }
}

#[derive(Debug, Clone)]
pub struct RunningStatus {
    started: DateTime<Utc>,
}

struct ServiceActorSeed {
    ctx: RpcContext,
    id: PackageId,
    /// Needed to interact with the container for the service
    persistent_container: PersistentContainer,
    /// This is notified every time the background job created in ServiceActor::init responds to a change
    synchronized: Arc<Notify>,
}

impl ServiceActorSeed {
    /// Used to indicate that we have finished the task of starting the service
    pub fn started(&self) {
        self.persistent_container.state.send_modify(|state| {
            state.running_status =
                Some(
                    state
                        .running_status
                        .take()
                        .unwrap_or_else(|| RunningStatus {
                            started: Utc::now(),
                        }),
                );
        });
    }
    /// Used to indicate that we have finished the task of stopping the service
    pub fn stopped(&self) {
        self.persistent_container.state.send_modify(|state| {
            state.running_status = None;
        });
    }
}

#[derive(Deserialize, Serialize, Parser, TS)]
pub struct RebuildParams {
    pub id: PackageId,
}
pub async fn rebuild(ctx: RpcContext, RebuildParams { id }: RebuildParams) -> Result<(), Error> {
    ctx.services.load(&ctx, &id, LoadDisposition::Retry).await?;
    Ok(())
}

#[derive(Deserialize, Serialize, Parser, TS)]
pub struct ConnectParams {
    pub id: PackageId,
}

pub async fn connect_rpc(
    ctx: RpcContext,
    ConnectParams { id }: ConnectParams,
) -> Result<Guid, Error> {
    let id_ref = &id;
    crate::lxc::connect(
        &ctx,
        ctx.services
            .get(&id)
            .await
            .as_ref()
            .or_not_found(lazy_format!("service for {id_ref}"))?
            .seed
            .persistent_container
            .lxc_container
            .get()
            .or_not_found(lazy_format!("container for {id_ref}"))?,
    )
    .await
}

pub async fn connect_rpc_cli(
    HandlerArgs {
        context,
        parent_method,
        method,
        params,
        inherited_params,
        raw_params,
    }: HandlerArgs<CliContext, ConnectParams>,
) -> Result<(), Error> {
    let ctx = context.clone();
    let guid = CallRemoteHandler::<CliContext, _, _>::new(from_fn_async(connect_rpc))
        .handle_async(HandlerArgs {
            context,
            parent_method,
            method,
            params: rpc_toolkit::util::Flat(params, Empty {}),
            inherited_params,
            raw_params,
        })
        .await?;

    crate::lxc::connect_cli(&ctx, guid).await
}

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct AttachParams {
    pub id: PackageId,
    #[ts(type = "string[]")]
    pub command: Vec<OsString>,
    pub tty: bool,
    pub stderr_tty: bool,
    #[ts(skip)]
    #[serde(rename = "__auth_session")]
    session: Option<InternedString>,
    #[ts(type = "string | null")]
    subcontainer: Option<InternedString>,
    #[ts(type = "string | null")]
    name: Option<InternedString>,
    #[ts(type = "string | null")]
    image_id: Option<ImageId>,
}
pub async fn attach(
    ctx: RpcContext,
    AttachParams {
        id,
        command,
        tty,
        stderr_tty,
        session,
        subcontainer,
        image_id,
        name,
    }: AttachParams,
) -> Result<Guid, Error> {
    let (container_id, subcontainer_id, image_id, workdir, root_command) = {
        let id = &id;

        let service = ctx.services.get(id).await;

        let service_ref = service.as_ref().or_not_found(id)?;

        let container = &service_ref.seed.persistent_container;
        let root_dir = container
            .lxc_container
            .get()
            .map(|x| x.rootfs_dir().to_owned())
            .or_not_found(format!("container for {id}"))?;

        let subcontainer = subcontainer.map(|x| AsRef::<str>::as_ref(&x).to_uppercase());
        let name = name.map(|x| AsRef::<str>::as_ref(&x).to_uppercase());
        let image_id = image_id.map(|x| AsRef::<Path>::as_ref(&x).to_string_lossy().to_uppercase());

        let subcontainers = container.subcontainers.lock().await;
        let subcontainer_ids: Vec<_> = subcontainers
            .iter()
            .filter(|(x, wrapper)| {
                if let Some(subcontainer) = subcontainer.as_ref() {
                    AsRef::<str>::as_ref(x).contains(AsRef::<str>::as_ref(subcontainer))
                } else if let Some(name) = name.as_ref() {
                    AsRef::<str>::as_ref(&wrapper.name)
                        .to_uppercase()
                        .contains(AsRef::<str>::as_ref(name))
                } else if let Some(image_id) = image_id.as_ref() {
                    let Some(wrapper_image_id) = AsRef::<Path>::as_ref(&wrapper.image_id).to_str()
                    else {
                        return false;
                    };
                    wrapper_image_id
                        .to_uppercase()
                        .contains(AsRef::<str>::as_ref(&image_id))
                } else {
                    true
                }
            })
            .collect();
        let format_subcontainer_pair = |(guid, wrapper): (&Guid, &Subcontainer)| {
            format!(
                "{guid} imageId: {image_id} name: \"{name}\"",
                name = &wrapper.name,
                image_id = &wrapper.image_id
            )
        };
        let Some((subcontainer_id, image_id)) = subcontainer_ids
            .first()
            .map::<(Guid, ImageId), _>(|&x| (x.0.clone(), x.1.image_id.clone()))
        else {
            drop(subcontainers);
            let subcontainers = container
                .subcontainers
                .lock()
                .await
                .iter()
                .map(format_subcontainer_pair)
                .join("\n");
            return Err(Error::new(
                eyre!("no matching subcontainers are running for {id}; some possible choices are:\n{subcontainers}"),
                ErrorKind::NotFound,
            ));
        };

        let passwd = root_dir
            .join("media/startos/subcontainers")
            .join(subcontainer_id.as_ref())
            .join("etc")
            .join("passwd");

        let root_command = get_passwd_root_command(passwd).await;

        let workdir = attach_workdir(&image_id, &root_dir).await?;

        if subcontainer_ids.len() > 1 {
            let subcontainer_ids = subcontainer_ids
                .into_iter()
                .map(format_subcontainer_pair)
                .join("\n");
            return Err(Error::new(
                eyre!("multiple subcontainers found for {id}: \n{subcontainer_ids}"),
                ErrorKind::InvalidRequest,
            ));
        }

        (
            service_ref.container_id()?,
            subcontainer_id,
            image_id,
            workdir,
            root_command,
        )
    };

    let guid = Guid::new();
    async fn handler(
        ws: &mut WebSocket,
        container_id: ContainerId,
        subcontainer_id: Guid,
        command: Vec<OsString>,
        tty: bool,
        stderr_tty: bool,
        image_id: ImageId,
        workdir: Option<String>,
        root_command: &RootCommand,
    ) -> Result<(), Error> {
        use axum::extract::ws::Message;

        let mut ws = ws.fuse();

        let mut cmd = Command::new("lxc-attach");
        let root_path = Path::new("/media/startos/subcontainers").join(subcontainer_id.as_ref());
        cmd.kill_on_drop(true);

        cmd.arg(&*container_id)
            .arg("--")
            .arg("start-cli")
            .arg("subcontainer")
            .arg("exec")
            .arg("--env")
            .arg(
                Path::new("/media/startos/images")
                    .join(image_id)
                    .with_extension("env"),
            );

        if let Some(workdir) = workdir {
            cmd.arg("--workdir").arg(workdir);
        }

        if tty {
            cmd.arg("--force-tty");
        }

        if stderr_tty {
            cmd.arg("--force-stderr-tty");
        }

        cmd.arg(&root_path).arg("--");

        if command.is_empty() {
            cmd.arg(&root_command.0);
        } else {
            cmd.args(&command);
        }

        let mut child = cmd
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        let pid = nix::unistd::Pid::from_raw(child.id().or_not_found("child pid")? as i32);

        let mut stdin = Some(child.stdin.take().or_not_found("child stdin")?);

        let mut current_in = "stdin".to_owned();
        let mut current_out = "stdout";
        ws.send(Message::Text(current_out.into()))
            .await
            .with_kind(ErrorKind::Network)?;
        let mut stdout = AsyncReadStream::new(
            child.stdout.take().or_not_found("child stdout")?,
            4 * CAP_1_KiB,
        )
        .fuse();
        let mut stderr = AsyncReadStream::new(
            child.stderr.take().or_not_found("child stderr")?,
            4 * CAP_1_KiB,
        )
        .fuse();

        loop {
            futures::select_biased! {
                out = stdout.try_next() => {
                    if let Some(out) = out? {
                        if current_out != "stdout" {
                            ws.send(Message::Text("stdout".into()))
                                .await
                                .with_kind(ErrorKind::Network)?;
                            current_out = "stdout";
                        }
                        ws.send(Message::Binary(out))
                            .await
                            .with_kind(ErrorKind::Network)?;
                    } else {
                        ws.send(Message::Text("close-stdout".into()))
                                .await
                                .with_kind(ErrorKind::Network)?;
                    }
                }
                err = stderr.try_next() => {
                    if let Some(err) = err? {
                        if current_out != "stderr" {
                            ws.send(Message::Text("stderr".into()))
                                .await
                                .with_kind(ErrorKind::Network)?;
                            current_out = "stderr";
                        }
                        ws.send(Message::Binary(err))
                            .await
                            .with_kind(ErrorKind::Network)?;
                    } else {
                        ws.send(Message::Text("close-stderr".into()))
                                .await
                                .with_kind(ErrorKind::Network)?;
                    }
                }
                msg = ws.try_next() => {
                    if let Some(msg) = msg.with_kind(ErrorKind::Network)? {
                        match msg {
                            Message::Text(in_ty) => {
                                current_in = in_ty;
                            }
                            Message::Binary(data) => {
                                match &*current_in {
                                    "stdin" => {
                                        if let Some(stdin) = &mut stdin {
                                            stdin.write_all(&data).await?;
                                        }
                                    }
                                    "close-stdin" => {
                                        stdin.take();
                                    }
                                    "signal" => {
                                        if data.len() != 4 {
                                            return Err(Error::new(
                                                eyre!("invalid byte length for signal: {}", data.len()),
                                                ErrorKind::InvalidRequest
                                            ));
                                        }
                                        let mut sig_buf = [0u8; 4];
                                        sig_buf.clone_from_slice(&data);
                                        nix::sys::signal::kill(
                                            pid,
                                            Signal::try_from(i32::from_be_bytes(sig_buf))
                                                .with_kind(ErrorKind::InvalidRequest)?
                                        ).with_kind(ErrorKind::Filesystem)?;
                                    }
                                    _ => (),
                                }
                            }
                            _ => ()
                        }
                    } else {
                        return Ok(())
                    }
                }
            }
            if stdout.is_terminated() && stderr.is_terminated() {
                break;
            }
        }

        let exit = child.wait().await?;
        ws.send(Message::Text("exit".into()))
            .await
            .with_kind(ErrorKind::Network)?;
        ws.send(Message::Binary(i32::to_be_bytes(exit.into_raw()).to_vec()))
            .await
            .with_kind(ErrorKind::Network)?;

        Ok(())
    }
    ctx.rpc_continuations
        .add(
            guid.clone(),
            RpcContinuation::ws_authed(
                &ctx,
                session,
                move |mut ws| async move {
                    if let Err(e) = handler(
                        &mut ws,
                        container_id,
                        subcontainer_id,
                        command,
                        tty,
                        stderr_tty,
                        image_id,
                        workdir,
                        &root_command,
                    )
                    .await
                    {
                        tracing::error!("Error in attach websocket: {e}");
                        tracing::debug!("{e:?}");
                        ws.close_result(Err::<&str, _>(e)).await.log_err();
                    } else {
                        ws.normal_close("exit").await.log_err();
                    }
                },
                Duration::from_secs(30),
            ),
        )
        .await;

    Ok(guid)
}

async fn attach_workdir(image_id: &ImageId, root_dir: &Path) -> Result<Option<String>, Error> {
    let path_str = root_dir.join("media/startos/images/");

    let mut subcontainer_json =
        tokio::fs::File::open(path_str.join(image_id).with_extension("json")).await?;
    let mut contents = vec![];
    subcontainer_json.read_to_end(&mut contents).await?;
    let subcontainer_json: serde_json::Value =
        serde_json::from_slice(&contents).with_kind(ErrorKind::Filesystem)?;
    Ok(subcontainer_json["workdir"].as_str().map(|x| x.to_string()))
}

async fn get_passwd_root_command(etc_passwd_path: PathBuf) -> RootCommand {
    async {
        let mut file = tokio::fs::File::open(etc_passwd_path).await?;

        let mut contents = vec![];
        file.read_to_end(&mut contents).await?;

        let contents = String::from_utf8_lossy(&contents);

        for line in contents.split('\n') {
            let line_information = line.split(':').collect::<Vec<_>>();
            if let (Some(&"root"), Some(shell)) =
                (line_information.first(), line_information.last())
            {
                return Ok(shell.to_string());
            }
        }
        Err(Error::new(
            eyre!("Could not parse /etc/passwd for shell: {}", contents),
            ErrorKind::Filesystem,
        ))
    }
    .await
    .map(RootCommand)
    .unwrap_or_else(|e| {
        tracing::error!("Could not get the /etc/passwd: {e}");
        tracing::debug!("{e:?}");
        RootCommand("/bin/sh".to_string())
    })
}

#[derive(Deserialize, Serialize, Parser)]
pub struct CliAttachParams {
    pub id: PackageId,
    #[arg(long)]
    pub force_tty: bool,
    #[arg(trailing_var_arg = true)]
    pub command: Vec<OsString>,
    #[arg(long, short)]
    subcontainer: Option<InternedString>,
    #[arg(long, short)]
    name: Option<InternedString>,
    #[arg(long, short)]
    image_id: Option<ImageId>,
}
#[instrument[skip_all]]
pub async fn cli_attach(
    HandlerArgs {
        context,
        parent_method,
        method,
        params,
        ..
    }: HandlerArgs<CliContext, CliAttachParams>,
) -> Result<(), Error> {
    use std::io::Write;

    use tokio_tungstenite::tungstenite::Message;

    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let stderr = std::io::stderr();

    let tty = params.force_tty || (stdin.is_terminal() && stdout.is_terminal());

    let raw = if stdin.is_terminal() && stdout.is_terminal() {
        Some(termion::get_tty()?.into_raw_mode()?)
    } else {
        None
    };

    let (kill, thread_kill) = tokio::sync::oneshot::channel();
    let (thread_send, recv) = tokio::sync::mpsc::channel(4 * CAP_1_KiB);
    let stdin_thread: NonDetachingJoinHandle<()> = tokio::task::spawn_blocking(move || {
        use std::io::Read;
        let mut stdin = stdin.lock().bytes();

        while thread_kill.is_empty() {
            if let Some(b) = stdin.next() {
                thread_send.blocking_send(b).unwrap();
            } else {
                break;
            }
        }
    })
    .into();
    let mut stdin = Some(recv);

    let guid: Guid = from_value(
        context
            .call_remote::<RpcContext>(
                &parent_method.into_iter().chain(method).join("."),
                json!({
                    "id": params.id,
                    "command": params.command,
                    "tty": tty,
                    "stderrTty": stderr.is_terminal(),
                    "ptySize": if tty { TermSize::get_current() } else { None },
                    "subcontainer": params.subcontainer,
                    "imageId": params.image_id,
                    "name": params.name,
                }),
            )
            .await?,
    )?;
    let mut ws = context.ws_continuation(guid).await?;

    let mut current_in = "stdin";
    let mut current_out = "stdout".to_owned();
    ws.send(Message::Text(current_in.into()))
        .await
        .with_kind(ErrorKind::Network)?;
    let mut stdout = Some(stdout);
    let mut stderr = Some(stderr);
    loop {
        futures::select_biased! {
            // signal = tokio:: => {
            //     let exit = exit?;
            //     if current_out != "exit" {
            //         ws.send(Message::Text("exit".into()))
            //             .await
            //             .with_kind(ErrorKind::Network)?;
            //         current_out = "exit";
            //     }
            //     ws.send(Message::Binary(
            //         i32::to_be_bytes(exit.into_raw()).to_vec()
            //     )).await.with_kind(ErrorKind::Network)?;
            // }
            input = stdin.as_mut().map_or(
                futures::future::Either::Left(futures::future::pending()),
                |s| futures::future::Either::Right(s.recv())
            ).fuse() => {
                if let (Some(input), Some(stdin)) = (input.transpose()?, &mut stdin) {
                    let mut input = vec![input];
                    while let Ok(b) = stdin.try_recv() {
                        input.push(b?);
                    }
                    if current_in != "stdin" {
                        ws.send(Message::Text("stdin".into()))
                            .await
                            .with_kind(ErrorKind::Network)?;
                        current_in = "stdin";
                    }
                    ws.send(Message::Binary(input))
                        .await
                        .with_kind(ErrorKind::Network)?;
                } else {
                    ws.send(Message::Text("close-stdin".into()))
                        .await
                        .with_kind(ErrorKind::Network)?;
                    stdin.take();
                }
            }
            msg = ws.try_next() => {
                if let Some(msg) = msg.with_kind(ErrorKind::Network)? {
                    match msg {
                        Message::Text(out_ty) => {
                            current_out = out_ty;
                        }
                        Message::Binary(data) => {
                            match &*current_out {
                                "stdout" => {
                                    if let Some(stdout) = &mut stdout {
                                        stdout.write_all(&data)?;
                                        stdout.flush()?;
                                    }
                                }
                                "stderr" => {
                                    if let Some(stderr) = &mut stderr {
                                        stderr.write_all(&data)?;
                                        stderr.flush()?;
                                    }
                                }
                                "close-stdout" => {
                                    stdout.take();
                                }
                                "close-stderr" => {
                                    stderr.take();
                                }
                                "exit" => {
                                    if data.len() != 4 {
                                        return Err(Error::new(
                                            eyre!("invalid byte length for exit code: {}", data.len()),
                                            ErrorKind::InvalidRequest
                                        ));
                                    }
                                    let mut exit_buf = [0u8; 4];
                                    exit_buf.clone_from_slice(&data);
                                    let code = i32::from_be_bytes(exit_buf);
                                    drop(raw);
                                    std::process::exit(code);
                                }
                                _ => (),
                            }
                        }
                        Message::Close(Some(close)) => {
                            if close.code != CloseCode::Normal {
                                return Err(Error::new(
                                    color_eyre::eyre::Report::msg(close.reason),
                                    ErrorKind::Network
                                ));
                            }
                        }
                        _ => ()
                    }
                } else {
                    kill.send(()).ok();
                    stdin_thread.wait_for_abort().await.log_err();
                    return Ok(())
                }
            }
        }
    }
}
