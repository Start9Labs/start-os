use std::collections::{BTreeMap, BTreeSet};
use std::net::Ipv4Addr;
use std::sync::Arc;
use std::task::Poll;
use std::time::Duration;

use color_eyre::eyre::eyre;
use embassy_container_init::ProcessGroupId;
use futures::future::BoxFuture;
use futures::{Future, FutureExt, TryFutureExt};
use helpers::UnixRpcClient;
use models::{ErrorKind, OptionExt, PackageId};
use nix::sys::signal::Signal;
use persistent_container::PersistentContainer;
use rand::SeedableRng;
use sqlx::Connection;
use start_stop::StartStop;
use tokio::sync::watch::{self, Sender};
use tokio::sync::{oneshot, Mutex};
use tracing::instrument;
use transition_state::TransitionState;

use crate::backup::target::PackageBackupInfo;
use crate::backup::PackageBackupReport;
use crate::config::action::ConfigRes;
use crate::config::spec::ValueSpecPointer;
use crate::config::ConfigureContext;
use crate::context::RpcContext;
use crate::db::model::{CurrentDependencies, CurrentDependencyInfo};
use crate::dependencies::{
    add_dependent_to_current_dependents_lists, compute_dependency_config_errs,
};
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::guard::TmpMountGuard;
use crate::install::cleanup::remove_from_current_dependents_lists;
use crate::net::net_controller::NetService;
use crate::net::vhost::AlpnInfo;
use crate::prelude::*;
use crate::procedure::docker::{DockerContainer, DockerProcedure, LongRunning};
use crate::procedure::{NoOutput, ProcedureName};
use crate::s9pk::manifest::Manifest;
use crate::status::MainStatus;
use crate::util::docker::{get_container_ip, kill_container};
use crate::util::NonDetachingJoinHandle;
use crate::volume::Volume;
use crate::Error;

pub mod health;
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

type ManagerPersistentContainer = Arc<Option<PersistentContainer>>;
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
    next_gid: (watch::Sender<u32>, watch::Receiver<u32>),
    main_gid: (
        watch::Sender<ProcessGroupId>,
        watch::Receiver<ProcessGroupId>,
    ),
}

impl Default for Gid {
    fn default() -> Self {
        Self {
            next_gid: watch::channel(1),
            main_gid: watch::channel(ProcessGroupId(1)),
        }
    }
}
impl Gid {
    pub fn new_gid(&self) -> ProcessGroupId {
        let mut previous = 0;
        self.next_gid.0.send_modify(|x| {
            previous = *x;
            *x = previous + 1;
        });
        ProcessGroupId(previous)
    }

    pub fn new_main_gid(&self) -> ProcessGroupId {
        let gid = self.new_gid();
        self.main_gid.0.send(gid).unwrap_or_default();
        gid
    }
}

/// This is the controller of the services. Here is where we can control a service with a start, stop, restart, etc.
#[derive(Clone)]
pub struct Manager {
    seed: Arc<ManagerSeed>,

    manage_container: Arc<manager_container::ManageContainer>,
    transition: Arc<watch::Sender<TransitionState>>,
    persistent_container: ManagerPersistentContainer,

    pub gid: Arc<Gid>,
}
impl Manager {
    pub async fn new(ctx: RpcContext, manifest: Manifest) -> Result<Self, Error> {
        let seed = Arc::new(ManagerSeed {
            ctx,
            container_name: DockerProcedure::container_name(&manifest.id, None),
            manifest,
        });

        let persistent_container = Arc::new(PersistentContainer::init(&seed).await?);
        let manage_container = Arc::new(
            manager_container::ManageContainer::new(seed.clone(), persistent_container.clone())
                .await?,
        );
        let (transition, _) = watch::channel(Default::default());
        let transition = Arc::new(transition);
        Ok(Self {
            seed,
            manage_container,
            transition,
            persistent_container,
            gid: Default::default(),
        })
    }

    /// awaiting this does not wait for the start to complete
    pub async fn start(&self) {
        if self._is_transition_restart() {
            return;
        }
        self._transition_abort().await;
        self.manage_container.to_desired(StartStop::Start);
    }

    /// awaiting this does not wait for the stop to complete
    pub async fn stop(&self) {
        self._transition_abort().await;
        self.manage_container.to_desired(StartStop::Stop);
    }
    /// awaiting this does not wait for the restart to complete
    pub async fn restart(&self) {
        if self._is_transition_restart()
            && *self.manage_container.desired_state().borrow() == StartStop::Stop
        {
            return;
        }
        if self.manage_container.desired_state().borrow().is_start() {
            self._transition_replace(self._transition_restart()).await;
        }
    }
    /// awaiting this does not wait for the restart to complete
    pub async fn configure(
        &self,
        configure_context: ConfigureContext,
    ) -> Result<BTreeMap<PackageId, String>, Error> {
        if self._is_transition_restart() {
            self._transition_abort().await;
        } else if self._is_transition_backup() {
            return Err(Error::new(
                eyre!("Can't configure because service is backing up"),
                ErrorKind::InvalidRequest,
            ));
        }
        let context = self.seed.ctx.clone();
        let id = self.seed.manifest.id.clone();

        let breakages = configure(context, id, configure_context).await?;

        self.restart().await;

        Ok(breakages)
    }

    /// awaiting this does not wait for the backup to complete
    pub async fn backup(&self, backup_guard: BackupGuard) -> BackupReturn {
        if self._is_transition_backup() {
            return BackupReturn::AlreadyRunning(PackageBackupReport {
                error: Some("Can't do backup because service is already backing up".to_owned()),
            });
        }
        let (transition_state, done) = self._transition_backup(backup_guard);
        self._transition_replace(transition_state).await;
        done.await
    }
    pub async fn exit(&self) {
        self._transition_abort().await;
        self.manage_container
            .wait_for_desired(StartStop::Stop)
            .await;
    }

    /// A special exit that is overridden the start state, should only be called in the shutdown, where we remove other containers
    async fn shutdown(&self) -> Result<(), Error> {
        self.manage_container.lock_state_forever(&self.seed).await?;

        self.exit().await;
        Ok(())
    }

    /// Used when we want to shutdown the service
    pub async fn signal(&self, signal: Signal) -> Result<(), Error> {
        let gid = self.gid.clone();
        send_signal(self, gid, signal).await
    }

    /// Used as a getter, but also used in procedure
    pub fn rpc_client(&self) -> Option<Arc<UnixRpcClient>> {
        (*self.persistent_container)
            .as_ref()
            .map(|x| x.rpc_client())
    }

    async fn _transition_abort(&self) {
        self.transition
            .send_replace(Default::default())
            .abort()
            .await;
    }
    async fn _transition_replace(&self, transition_state: TransitionState) {
        self.transition.send_replace(transition_state).abort().await;
    }

    pub(super) fn perform_restart(&self) -> impl Future<Output = Result<(), Error>> + 'static {
        let manage_container = self.manage_container.clone();
        async move {
            let restart_override = manage_container.set_override(MainStatus::Restarting)?;
            manage_container.wait_for_desired(StartStop::Stop).await;
            manage_container.wait_for_desired(StartStop::Start).await;
            restart_override.drop();
            Ok(())
        }
    }
    fn _transition_restart(&self) -> TransitionState {
        let transition = self.transition.clone();
        let restart = self.perform_restart();
        TransitionState::Restarting(
            tokio::spawn(async move {
                if let Err(err) = restart.await {
                    tracing::error!("Error restarting service: {}", err);
                }
                transition.send_replace(Default::default());
            })
            .into(),
        )
    }
    fn perform_backup(
        &self,
        backup_guard: BackupGuard,
    ) -> impl Future<Output = Result<Result<PackageBackupInfo, Error>, Error>> {
        let manage_container = self.manage_container.clone();
        let seed = self.seed.clone();
        async move {
            let peek = seed.ctx.db.peek().await?;
            let state_reverter = DesiredStateReverter::new(manage_container.clone());
            let override_guard =
                manage_container.set_override(get_status(peek, &seed.manifest).backing_up())?;
            manage_container.wait_for_desired(StartStop::Stop).await;
            let backup_guard = backup_guard.lock().await;
            let guard = backup_guard.mount_package_backup(&seed.manifest.id).await?;

            let return_value = seed.manifest.backup.create(seed.clone()).await;
            guard.unmount().await?;
            drop(backup_guard);

            let manifest_id = seed.manifest.id.clone();
            seed.ctx
                .db
                .mutate(|db| {
                    if let Some(progress) = db
                        .as_server_info_mut()
                        .as_status_info_mut()
                        .as_backup_progress_mut()
                        .transpose_mut()
                        .and_then(|p| p.as_idx_mut(&manifest_id))
                    {
                        progress.as_complete_mut().ser(&true)?;
                    }
                    Ok(())
                })
                .await?;

            state_reverter.revert().await;

            override_guard.drop();
            Ok::<_, Error>(return_value)
        }
    }
    fn _transition_backup(
        &self,
        backup_guard: BackupGuard,
    ) -> (TransitionState, BoxFuture<BackupReturn>) {
        let (send, done) = oneshot::channel();

        let transition_state = self.transition.clone();
        (
            TransitionState::BackingUp(
                tokio::spawn(
                    self.perform_backup(backup_guard)
                        .then(finish_up_backup_task(transition_state, send)),
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
        matches!(*transition, TransitionState::Restarting(_))
    }
    fn _is_transition_backup(&self) -> bool {
        let transition = self.transition.borrow();
        matches!(*transition, TransitionState::BackingUp(_))
    }
}

#[instrument(skip_all)]
async fn configure(
    ctx: RpcContext,
    id: PackageId,
    mut configure_context: ConfigureContext,
) -> Result<BTreeMap<PackageId, String>, Error> {
    let db = ctx.db.peek().await?;
    let id = &id;
    let ctx = &ctx;
    let overrides = &mut configure_context.overrides;
    // fetch data from db
    let manifest = db
        .as_package_data()
        .as_idx(id)
        .or_not_found(id)?
        .as_manifest()
        .de()?;

    // get current config and current spec
    let ConfigRes {
        config: old_config,
        spec,
    } = manifest
        .config
        .as_ref()
        .or_not_found("Manifest config")?
        .get(ctx, id, &manifest.version, &manifest.volumes)
        .await?;

    // determine new config to use
    let mut config = if let Some(config) = configure_context.config.or_else(|| old_config.clone()) {
        config
    } else {
        spec.gen(
            &mut rand::rngs::StdRng::from_entropy(),
            &configure_context.timeout,
        )?
    };

    spec.validate(&manifest)?;
    spec.matches(&config)?; // check that new config matches spec

    // TODO Commit or not?
    spec.update(ctx, &manifest, overrides, &mut config).await?; // dereference pointers in the new config

    let manifest = db
        .as_package_data()
        .as_idx(id)
        .or_not_found(id)?
        .as_installed()
        .or_not_found(id)?
        .as_manifest()
        .de()?;

    let dependencies = &manifest.dependencies;
    let mut current_dependencies: CurrentDependencies = CurrentDependencies(
        dependencies
            .0
            .iter()
            .filter_map(|(id, info)| {
                if info.requirement.required() {
                    Some((id.clone(), CurrentDependencyInfo::default()))
                } else {
                    None
                }
            })
            .collect(),
    );
    for ptr in spec.pointers(&config)? {
        match ptr {
            ValueSpecPointer::Package(pkg_ptr) => {
                if let Some(info) = current_dependencies.0.get_mut(pkg_ptr.package_id()) {
                    info.pointers.insert(pkg_ptr);
                } else {
                    let id = pkg_ptr.package_id().to_owned();
                    let mut pointers = BTreeSet::new();
                    pointers.insert(pkg_ptr);
                    current_dependencies.0.insert(
                        id,
                        CurrentDependencyInfo {
                            pointers,
                            health_checks: BTreeSet::new(),
                        },
                    );
                }
            }
            ValueSpecPointer::System(_) => (),
        }
    }

    let action = manifest.config.as_ref().or_not_found(id)?;
    let version = &manifest.version;
    let volumes = &manifest.volumes;
    if !configure_context.dry_run {
        // run config action
        let res = action
            .set(ctx, id, version, &dependencies, volumes, &config)
            .await?;

        // track dependencies with no pointers
        for (package_id, health_checks) in res.depends_on.into_iter() {
            if let Some(current_dependency) = current_dependencies.0.get_mut(&package_id) {
                current_dependency.health_checks.extend(health_checks);
            } else {
                current_dependencies.0.insert(
                    package_id,
                    CurrentDependencyInfo {
                        pointers: BTreeSet::new(),
                        health_checks,
                    },
                );
            }
        }

        // track dependency health checks
        current_dependencies = current_dependencies.map(|x| {
            x.into_iter()
                .filter(|(dep_id, _)| {
                    if dep_id != id && !manifest.dependencies.0.contains_key(dep_id) {
                        tracing::warn!("Illegal dependency specified: {}", dep_id);
                        false
                    } else {
                        true
                    }
                })
                .collect()
        });
    }

    let dependency_config_errs =
        compute_dependency_config_errs(&ctx, &db, &manifest, &current_dependencies, overrides)
            .await?;

    // cache current config for dependents
    configure_context
        .overrides
        .insert(id.clone(), config.clone());

    // handle dependents

    let dependents = db
        .as_package_data()
        .as_idx(id)
        .or_not_found(id)?
        .as_installed()
        .or_not_found(id)?
        .as_current_dependents()
        .de()?;
    for (dependent, _dep_info) in dependents.0.iter().filter(|(dep_id, _)| dep_id != &id) {
        // check if config passes dependent check
        if let Some(cfg) = db
            .as_package_data()
            .as_idx(dependent)
            .or_not_found(dependent)?
            .as_installed()
            .or_not_found(dependent)?
            .as_manifest()
            .as_dependencies()
            .as_idx(id)
            .or_not_found(id)?
            .as_config()
            .de()?
        {
            let manifest = db
                .as_package_data()
                .as_idx(dependent)
                .or_not_found(dependent)?
                .as_installed()
                .or_not_found(dependent)?
                .as_manifest()
                .de()?;
            if let Err(error) = cfg
                .check(
                    ctx,
                    dependent,
                    &manifest.version,
                    &manifest.volumes,
                    id,
                    &config,
                )
                .await?
            {
                configure_context.breakages.insert(dependent.clone(), error);
            }
        }
    }

    if !configure_context.dry_run {
        return ctx
            .db
            .mutate(move |db| {
                remove_from_current_dependents_lists(db, id, &current_dependencies)?;
                add_dependent_to_current_dependents_lists(db, id, &current_dependencies)?;
                current_dependencies.0.remove(id);
                for (dep, errs) in db
                    .as_package_data_mut()
                    .as_entries_mut()?
                    .into_iter()
                    .filter_map(|(id, pde)| {
                        pde.as_installed_mut()
                            .map(|i| (id, i.as_status_mut().as_dependency_config_errors_mut()))
                    })
                {
                    errs.remove(id)?;
                    if let Some(err) = configure_context.breakages.get(&dep) {
                        errs.insert(id, err)?;
                    }
                }
                let installed = db
                    .as_package_data_mut()
                    .as_idx_mut(id)
                    .or_not_found(id)?
                    .as_installed_mut()
                    .or_not_found(id)?;
                installed
                    .as_current_dependencies_mut()
                    .ser(&current_dependencies)?;
                let status = installed.as_status_mut();
                status.as_configured_mut().ser(&true)?;
                status
                    .as_dependency_config_errors_mut()
                    .ser(&dependency_config_errs)?;
                Ok(configure_context.breakages)
            })
            .await; // add new
    }

    Ok(configure_context.breakages)
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

fn finish_up_backup_task(
    transition: Arc<Sender<TransitionState>>,
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

type RunMainResult = Result<Result<NoOutput, (i32, String)>, Error>;

#[instrument(skip_all)]
async fn run_main(
    seed: Arc<ManagerSeed>,
    persistent_container: ManagerPersistentContainer,
    started: Arc<impl Fn()>,
) -> RunMainResult {
    let mut runtime = NonDetachingJoinHandle::from(tokio::spawn(start_up_image(seed.clone())));
    let ip = match persistent_container.is_some() {
        false => Some(match get_running_ip(&seed, &mut runtime).await {
            GetRunningIp::Ip(x) => x,
            GetRunningIp::Error(e) => return Err(e),
            GetRunningIp::EarlyExit(x) => return Ok(x),
        }),
        true => None,
    };

    let svc = if let Some(ip) = ip {
        let net = add_network_for_main(&seed, ip).await?;
        started();
        Some(net)
    } else {
        None
    };

    let health = main_health_check_daemon(seed.clone());
    let res = tokio::select! {
        a = runtime => a.map_err(|_| Error::new(eyre!("Manager runtime panicked!"), crate::ErrorKind::Docker)).and_then(|a| a),
        _ = health => Err(Error::new(eyre!("Health check daemon exited!"), crate::ErrorKind::Unknown))
    };
    if let Some(svc) = svc {
        remove_network_for_main(svc).await?;
    }
    res
}

/// We want to start up the manifest, but in this case we want to know that we have generated the certificates.
/// Note for _generated_certificate: Needed to know that before we start the state we have generated the certificate
async fn start_up_image(seed: Arc<ManagerSeed>) -> Result<Result<NoOutput, (i32, String)>, Error> {
    seed.manifest
        .main
        .execute::<(), NoOutput>(
            &seed.ctx,
            &seed.manifest.id,
            &seed.manifest.version,
            ProcedureName::Main,
            &seed.manifest.volumes,
            None,
            None,
        )
        .await
}

async fn long_running_docker(
    seed: &ManagerSeed,
    container: &DockerContainer,
) -> Result<(LongRunning, UnixRpcClient), Error> {
    container
        .long_running_execute(
            &seed.ctx,
            &seed.manifest.id,
            &seed.manifest.version,
            &seed.manifest.volumes,
        )
        .await
}

enum GetRunningIp {
    Ip(Ipv4Addr),
    Error(Error),
    EarlyExit(Result<NoOutput, (i32, String)>),
}

async fn get_long_running_ip(seed: &ManagerSeed, runtime: &mut LongRunning) -> GetRunningIp {
    loop {
        match get_container_ip(&seed.container_name).await {
            Ok(Some(ip_addr)) => return GetRunningIp::Ip(ip_addr),
            Ok(None) => (),
            Err(e) if e.kind == ErrorKind::NotFound => (),
            Err(e) => return GetRunningIp::Error(e),
        }
        if let Poll::Ready(res) = futures::poll!(&mut runtime.running_output) {
            match res {
                Ok(_) => return GetRunningIp::EarlyExit(Ok(NoOutput)),
                Err(_e) => {
                    return GetRunningIp::Error(Error::new(
                        eyre!("Manager runtime panicked!"),
                        crate::ErrorKind::Docker,
                    ))
                }
            }
        }
    }
}

#[instrument(skip(seed))]
async fn add_network_for_main(
    seed: &ManagerSeed,
    ip: std::net::Ipv4Addr,
) -> Result<NetService, Error> {
    let mut svc = seed
        .ctx
        .net_controller
        .create_service(seed.manifest.id.clone(), ip)
        .await?;
    // DEPRECATED
    let mut secrets = seed.ctx.secret_store.acquire().await?;
    let mut tx = secrets.begin().await?;
    for (id, interface) in &seed.manifest.interfaces.0 {
        for (external, internal) in interface.lan_config.iter().flatten() {
            svc.add_lan(
                &mut tx,
                id.clone(),
                external.0,
                internal.internal,
                Err(AlpnInfo::Specified(vec![])),
            )
            .await?;
        }
        for (external, internal) in interface.tor_config.iter().flat_map(|t| &t.port_mapping) {
            svc.add_tor(&mut tx, id.clone(), external.0, internal.0)
                .await?;
        }
    }
    for volume in seed.manifest.volumes.values() {
        if let Volume::Certificate { interface_id } = volume {
            svc.export_cert(&mut tx, interface_id, ip.into()).await?;
        }
    }
    tx.commit().await?;
    Ok(svc)
}

#[instrument(skip(svc))]
async fn remove_network_for_main(svc: NetService) -> Result<(), Error> {
    svc.remove_all().await
}

async fn main_health_check_daemon(seed: Arc<ManagerSeed>) {
    tokio::time::sleep(Duration::from_secs(HEALTH_CHECK_GRACE_PERIOD_SECONDS)).await;
    loop {
        if let Err(e) = health::check(&seed.ctx, &seed.manifest.id).await {
            tracing::error!(
                "Failed to run health check for {}: {}",
                &seed.manifest.id,
                e
            );
            tracing::debug!("{:?}", e);
        }
        tokio::time::sleep(Duration::from_secs(HEALTH_CHECK_COOLDOWN_SECONDS)).await;
    }
}

type RuntimeOfCommand = NonDetachingJoinHandle<Result<Result<NoOutput, (i32, String)>, Error>>;

#[instrument(skip(seed, runtime))]
async fn get_running_ip(seed: &ManagerSeed, mut runtime: &mut RuntimeOfCommand) -> GetRunningIp {
    loop {
        match get_container_ip(&seed.container_name).await {
            Ok(Some(ip_addr)) => return GetRunningIp::Ip(ip_addr),
            Ok(None) => (),
            Err(e) if e.kind == ErrorKind::NotFound => (),
            Err(e) => return GetRunningIp::Error(e),
        }
        if let Poll::Ready(res) = futures::poll!(&mut runtime) {
            match res {
                Ok(Ok(response)) => return GetRunningIp::EarlyExit(response),
                Err(e) => {
                    return GetRunningIp::Error(Error::new(
                        match e.try_into_panic() {
                            Ok(e) => {
                                eyre!(
                                    "Manager runtime panicked: {}",
                                    e.downcast_ref::<&'static str>().unwrap_or(&"UNKNOWN")
                                )
                            }
                            _ => eyre!("Manager runtime cancelled!"),
                        },
                        crate::ErrorKind::Docker,
                    ))
                }
                Ok(Err(e)) => {
                    return GetRunningIp::Error(Error::new(
                        eyre!("Manager runtime returned error: {}", e),
                        crate::ErrorKind::Docker,
                    ))
                }
            }
        }
    }
}

async fn send_signal(manager: &Manager, gid: Arc<Gid>, signal: Signal) -> Result<(), Error> {
    // stop health checks from committing their results
    // shared
    //     .commit_health_check_results
    //     .store(false, Ordering::SeqCst);

    if let Some(rpc_client) = manager.rpc_client() {
        let main_gid = *gid.main_gid.0.borrow();
        let next_gid = gid.new_gid();
        #[cfg(feature = "js_engine")]
        if let Err(e) = crate::procedure::js_scripts::JsProcedure::default()
            .execute::<_, NoOutput>(
                &manager.seed.ctx.datadir,
                &manager.seed.manifest.id,
                &manager.seed.manifest.version,
                ProcedureName::Signal,
                &manager.seed.manifest.volumes,
                Some(embassy_container_init::SignalGroupParams {
                    gid: main_gid,
                    signal: signal as u32,
                }),
                None, // TODO
                next_gid,
                Some(rpc_client),
            )
            .await?
        {
            tracing::error!("Failed to send js signal: {}", e.1);
            tracing::debug!("{:?}", e);
        }
    } else {
        // send signal to container
        kill_container(&manager.seed.container_name, Some(signal))
            .await
            .or_else(|e| {
                if e.kind == ErrorKind::NotFound {
                    Ok(())
                } else {
                    Err(e)
                }
            })?;
    }

    Ok(())
}
