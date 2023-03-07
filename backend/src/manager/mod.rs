use std::collections::{BTreeMap, BTreeSet};
use std::net::Ipv4Addr;
use std::sync::Arc;
use std::task::Poll;
use std::time::Duration;

use color_eyre::eyre::eyre;
use color_eyre::Report;
use embassy_container_init::ProcessGroupId;
use futures::future::BoxFuture;
use futures::{Future, FutureExt, TryFutureExt};
use helpers::UnixRpcClient;
use models::{ErrorKind, PackageId, ProcedureName};
use nix::sys::signal::Signal;
use persistent_container::PersistentContainer;
use rand::SeedableRng;
use reqwest::Url;
use sqlx::Connection;
use start_stop::StartStop;
use tokio::sync::watch::{self, Sender};
use tokio::sync::{oneshot, Mutex};
use tracing::instrument;
use transition_state::TransitionState;

use crate::backup::target::PackageBackupInfo;
use crate::backup::PackageBackupReport;
use crate::config::action::ConfigRes;
use crate::config::ConfigureContext;
use crate::container::{DockerContainer, LongRunning};
use crate::context::RpcContext;
use crate::db::model::{CurrentDependencies, CurrentDependencyInfo};
use crate::dependencies::{
    break_transitive, heal_all_dependents_transitive, DependencyError, DependencyErrors,
    TaggedDependencyError,
};
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::guard::TmpMountGuard;
use crate::net::net_controller::NetService;
use crate::prelude::*;
use crate::s9pk::manifest::Manifest;
use crate::script::NoOutput;
use crate::status::MainStatus;
use crate::util::NonDetachingJoinHandle;
use crate::volume::Volume;

mod js_api;
mod manager_container;
mod manager_map;
pub mod manager_seed;
mod persistent_container;
mod start_stop;
mod transition_state;

pub use manager_map::ManagerMap;

use self::manager_container::{get_status, ManageContainer};
use self::manager_seed::ManagerSeed;

pub const HEALTH_CHECK_COOLDOWN_SECONDS: u64 = 15;
pub const HEALTH_CHECK_GRACE_PERIOD_SECONDS: u64 = 5;

type BackupGuard = Arc<Mutex<BackupMountGuard<TmpMountGuard>>>;
pub enum BackupReturn {
    Error(Error),
    AlreadyRunning(PackageBackupReport),
    Ran {
        report: PackageBackupReport,
        res: Result<PackageBackupInfo, Error>,
    },
}

pub struct Gid {
    next_gid: watch::Sender<u32>,
    main_gid: watch::Sender<ProcessGroupId>,
}

impl Default for Gid {
    fn default() -> Self {
        Self {
            next_gid: watch::channel(1).0,
            main_gid: watch::channel(ProcessGroupId(1)).0,
        }
    }
}
impl Gid {
    pub fn new_gid(&self) -> ProcessGroupId {
        let mut previous = 0;
        self.next_gid.send_modify(|x| {
            previous = *x;
            *x = previous + 1;
        });
        ProcessGroupId(previous)
    }

    pub fn new_main_gid(&self) -> ProcessGroupId {
        let gid = self.new_gid();
        self.main_gid.send_modify(|x| *x = gid);
        gid
    }
}

pub struct Manager {
    seed: Arc<ManagerSeed>,

    persistent_container: Arc<PersistentContainer>, // ManagerSeed
    manage_container: Arc<ManageContainer>,         // ManagerSeed + ManagerPersistentContainer
    transition: watch::Sender<Arc<TransitionState>>, // ManageContainer

    pub gid: Gid,
}
impl Manager {
    pub async fn new(
        ctx: RpcContext,
        manifest: Manifest,
        marketplace_url: Option<Url>,
    ) -> Result<Self, Error> {
        let seed = Arc::new(ManagerSeed {
            ctx,
            container_name: DockerContainer::container_name(&manifest.id, None),
            manifest,
            marketplace_url,
        });

        let persistent_container = Arc::new(PersistentContainer::init(&seed).await?);
        let manage_container = Arc::new(
            manager_container::ManageContainer::new(seed.clone(), persistent_container.clone())
                .await?,
        );
        let (transition, _) = watch::channel(Default::default());
        Ok(Self {
            seed,
            manage_container,
            transition,
            persistent_container,
            gid: Default::default(),
        })
    }

    pub fn start(&self) {
        self._transition_abort();
        self.manage_container.to_desired(StartStop::Start);
    }
    pub fn stop(&self) {
        self._transition_abort();
        self.manage_container.to_desired(StartStop::Stop);
    }
    pub async fn restart(&self) {
        if self._is_transition_restart() {
            return;
        }
        self._transition_replace(self._transition_restart());
    }
    pub async fn configure(
        &self,
        configure_context: ConfigureContext,
    ) -> Result<BTreeMap<PackageId, TaggedDependencyError>, Error> {
        if self._is_transition_configure() {
            return Ok(configure_context.breakages);
        }
        let context = self.seed.ctx.clone();
        let id = self.seed.manifest.id.clone();

        let (transition_state, done) = configure(context, id, configure_context).remote_handle();
        self._transition_replace({
            let manage_container = self.manage_container.clone();

            TransitionState::Configuring(
                tokio::spawn(async move {
                    let desired_state = manage_container.desired_state();
                    let state_reverter = DesiredStateReverter::new(manage_container.clone());
                    let mut current_state = manage_container.current_state();
                    manage_container.to_desired(StartStop::Stop);
                    while current_state.borrow().is_start() {
                        current_state.changed().await.unwrap();
                    }

                    transition_state.await;

                    state_reverter.revert().await;
                })
                .into(),
            )
        });
        done.await
    }
    pub async fn backup(&self, backup_guard: BackupGuard) -> BackupReturn {
        if self._is_transition_backup() {
            return BackupReturn::AlreadyRunning(PackageBackupReport {
                error: Some("Can't do backup because service is in a backing up state".to_owned()),
            });
        }
        let (transition_state, done) = self._transition_backup(backup_guard);
        self._transition_replace(transition_state);
        done.await
    }
    pub async fn exit(&self) {
        self.stop();
        let mut current_status = self.manage_container.current_state();

        while current_status.borrow().is_start() {
            current_status.changed().await.unwrap();
        }
    }

    pub async fn signal(&self, signal: Signal) -> Result<(), Error> {
        let rpc_client = self.rpc_client();
        let seed = self.seed.clone();
        let gid = self.gid.clone();
        send_signal(self, gid, signal).await
    }

    pub fn rpc_client(&self) -> Option<Arc<UnixRpcClient>> {
        (*self.persistent_container)
            .as_ref()
            .map(|x| x.rpc_client())
    }

    fn _transition_abort(&self) {
        if let Some(transition) = self
            .transition
            .send_replace(Default::default())
            .join_handle()
        {
            transition.abort();
        }
    }
    fn _transition_replace(&self, transition_state: TransitionState) {
        self.transition
            .send_replace(Arc::new(transition_state))
            .abort();
    }

    pub(super) fn perform_restart(&self) -> impl Future<Output = ()> + 'static {
        let manage_container = self.manage_container.clone();
        async move {
            let _ = manage_container.set_override(Some(MainStatus::Restarting));
            manage_container.wait_for_desired(StartStop::Stop).await;
            manage_container.wait_for_desired(StartStop::Start).await;
        }
    }
    fn _transition_restart(&self) -> TransitionState {
        let transition = self.transition.clone();
        let restart = self.perform_restart();
        TransitionState::Restarting(
            tokio::spawn(async move {
                restart.await;
                transition.send_replace(Default::default());
            })
            .into(),
        )
    }
    fn perform_backup(
        &self,
        backup_guard: BackupGuard,
    ) -> impl Future<Output = Result<Result<PackageBackupInfo, Error>, Error>> + 'static {
        let manage_container = self.manage_container.clone();
        let seed = self.seed.clone();
        async move {
            let state_reverter = DesiredStateReverter::new(manage_container.clone());
            let _ = manage_container.set_override(Some(
                get_status(&seed.ctx.db, &seed.manifest).await.backing_up(),
            ));
            manage_container.wait_for_desired(StartStop::Stop).await;

            let backup_guard = backup_guard.lock().await;
            let guard = backup_guard.mount_package_backup(&seed.manifest.id).await?;

            let res = seed
                .manifest
                .backup
                .create(
                    &seed.ctx,
                    &seed.manifest.id,
                    &seed.manifest.title,
                    &seed.manifest.version,
                    &seed.manifest.interfaces,
                    &seed.manifest.volumes,
                    seed.marketplace_url.clone(),
                )
                .await;
            guard.unmount().await?;
            drop(backup_guard);

            let return_value = res;
            state_reverter.revert().await;
            Ok::<_, Error>(return_value)
        }
    }
    fn _transition_backup(
        &self,
        backup_guard: BackupGuard,
    ) -> (TransitionState, BoxFuture<BackupReturn>) {
        let (send, done) = oneshot::channel();
        (
            TransitionState::BackingUp(
                tokio::spawn(
                    self.perform_backup(backup_guard)
                        .then(finnish_up_backup_task(self.transition.clone(), send)),
                )
                .into(),
            ),
            done.map_err(|err| Error::new(eyre!("Oneshot error: {err:?}"), ErrorKind::Unknown))
                .map(flatten_backup_error)
                .boxed(),
        )
    }
    fn _is_transition_restart(&self) -> bool {
        let transition = self.transition.borrow();
        matches!(**transition, TransitionState::Restarting(_))
    }
    fn _is_transition_backup(&self) -> bool {
        let transition = self.transition.borrow();
        matches!(**transition, TransitionState::BackingUp(_))
    }
    fn _is_transition_configure(&self) -> bool {
        let transition = self.transition.borrow();
        matches!(**transition, TransitionState::Configuring(_))
    }
}

// #[instrument(skip(ctx, configure_context))]
fn configure(
    ctx: RpcContext,
    id: PackageId,
    mut configure_context: ConfigureContext,
) -> BoxFuture<'static, Result<BTreeMap<PackageId, TaggedDependencyError>, Error>> {
    async move {
        let id = &id;
        let ctx = &ctx;
        let db = ctx.db.peek().await?;
        let overrides = &mut configure_context.overrides;
        let package = db.package_data().idx(id).expect()?;
        let manifest = package.into_manifest()?;
        // fetch data from db
        let action = manifest.config().de()?;
        let dependencies = todo!("BLUJ Dependencies");
        let volumes = manifest.volumes().de()?;
        let version = manifest.version().de()?;

        // get current config and current spec
        let ConfigRes {
            input: old_input,
            spec,
        } = action.get(ctx, id, &version, &volumes).await?;

        // determine new config to use
        let mut config =
            if let Some(config) = configure_context.config.or_else(|| old_config.clone()) {
                config
            } else {
                spec.gen(
                    &mut rand::rngs::StdRng::from_entropy(),
                    &configure_context.timeout,
                )?
            };

        if !configure_context.dry_run {
            // run config action
            let res = action
                .set(ctx, id, &version, &dependencies, &volumes, &config)
                .await?;

            ctx.call_config_hooks(id.clone(), &serde_json::Value::Object(config.clone()))
                .await;

            res.signal;

            ctx.db
                .mutate(|db| {
                    db.package_data()
                        .idx(&id)
                        .installed()
                        .status()
                        .configured()
                        .ser(true)?;
                    Ok(())
                })
                .await?;
        }
        // BLUJ TODO
        // if configure_context.dry_run {
        //     tx.abort().await?;
        // } else {
        //     tx.commit().await?;
        // }

        Ok(configure_context.breakages)
    }
    .boxed()
}

struct DesiredStateReverter {
    manage_container: Option<Arc<ManageContainer>>,
    starting_state: StartStop,
}
impl DesiredStateReverter {
    fn new(manage_container: Arc<ManageContainer>) -> Self {
        let starting_state = *manage_container.desired_state().borrow();
        let manage_container = Some(manage_container);
        Self {
            starting_state,
            manage_container,
        }
    }
    async fn revert(mut self) {
        if let Some(mut current_state) = self._revert() {
            while *current_state.borrow() != self.starting_state {
                current_state.changed().await.unwrap();
            }
        }
    }
    fn _revert(&mut self) -> Option<watch::Receiver<StartStop>> {
        if let Some(manage_container) = self.manage_container.take() {
            manage_container.to_desired(self.starting_state);

            return Some(manage_container.desired_state());
        }
        None
    }
}
impl Drop for DesiredStateReverter {
    fn drop(&mut self) {
        self._revert();
    }
}

type BackupDoneSender = oneshot::Sender<Result<PackageBackupInfo, Error>>;

fn finnish_up_backup_task(
    transition: Arc<Sender<Arc<TransitionState>>>,
    send: BackupDoneSender,
) -> impl FnOnce(Result<Result<PackageBackupInfo, Error>, Error>) -> BoxFuture<'static, ()> {
    move |result| {
        async move {
            transition.send_replace(Default::default());
            send.send(match result {
                Ok(a) => a,
                Err(e) => Err(e),
            })
            .unwrap_or_default();
        }
        .boxed()
    }
}

fn response_to_report(response: &Result<PackageBackupInfo, Error>) -> PackageBackupReport {
    PackageBackupReport {
        error: response.as_ref().err().map(|e| e.to_string()),
    }
}
fn flatten_backup_error(input: Result<Result<PackageBackupInfo, Error>, Error>) -> BackupReturn {
    match input {
        Ok(a) => BackupReturn::Ran {
            report: response_to_report(&a),
            res: a,
        },
        Err(err) => BackupReturn::Error(err),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Status {
    Starting,
    Running,
    Stopped,
    Paused,
    Shutdown,
}

#[derive(Debug, Clone, Copy)]
pub enum OnStop {
    Restart,
    Sleep,
    Exit,
}

type RunMainResult = Result<(), Error>;

#[instrument(skip(seed, started))]
async fn _run_main(seed: Arc<ManagerSeed>, started: Arc<impl Fn()>) -> RunMainResult {
    let mut runtime = NonDetachingJoinHandle::from(tokio::spawn(start_up_image(seed.clone())));

    let res = tokio::select! {
        a = runtime => a.map_err(|_| Error::new(eyre!("Manager runtime panicked!"), ErrorKind::Docker)).and_then(|a| a),
        _ = health => Err(Error::new(eyre!("Health check daemon exited!"), ErrorKind::Unknown))
    };
    if let Some(svc) = svc {
        remove_network_for_main(svc).await?;
    }
    res
}
