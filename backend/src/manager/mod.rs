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
use models::{ErrorKind, PackageId};
use nix::sys::signal::Signal;
use patch_db::DbHandle;
use persistent_container::PersistentContainer;
use rand::SeedableRng;
use sqlx::Connection;
use start_stop::StartStop;
use tokio::sync::oneshot;
use tokio::sync::{
    watch::{self, Sender},
    Mutex,
};
use tracing::instrument;
use transition_state::TransitionState;

use crate::backup::target::PackageBackupInfo;
use crate::backup::PackageBackupReport;
use crate::config::action::ConfigRes;
use crate::config::spec::ValueSpecPointer;
use crate::config::{not_found, ConfigReceipts, ConfigureContext};
use crate::context::RpcContext;
use crate::db::model::{CurrentDependencies, CurrentDependencyInfo};
use crate::dependencies::{
    add_dependent_to_current_dependents_lists, break_transitive, heal_all_dependents_transitive,
    DependencyError, DependencyErrors, TaggedDependencyError,
};
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::guard::TmpMountGuard;
use crate::install::cleanup::remove_from_current_dependents_lists;
use crate::net::net_controller::NetService;
use crate::net::vhost::AlpnInfo;
use crate::procedure::docker::{DockerContainer, DockerProcedure, LongRunning};
use crate::procedure::{NoOutput, ProcedureName};
use crate::s9pk::manifest::Manifest;
use crate::status::MainStatus;
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
    transition: Arc<watch::Sender<Arc<TransitionState>>>,
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

        let breakages = configure(context, id, configure_context).await?;
        self._transition_replace({
            let manage_container = self.manage_container.clone();

            TransitionState::Configuring(
                tokio::spawn(async move {
                    let state_reverter = DesiredStateReverter::new(manage_container.clone());
                    manage_container.wait_for_desired(StartStop::Stop).await;

                    state_reverter.revert().await;
                })
                .into(),
            )
        });
        Ok(breakages)
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
        self._transition_abort();
        self.manage_container
            .wait_for_desired(StartStop::Stop)
            .await;
    }

    /// A special exit that is overridden the start state, should only be called in the shutdown, where we remove other containers
    async fn shutdown(&self) {
        self.manage_container.lock_state_forever(&self.seed).await;

        self.exit().await;
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
            let restart_override = manage_container.set_override(Some(MainStatus::Restarting));
            manage_container.wait_for_desired(StartStop::Stop).await;
            manage_container.wait_for_desired(StartStop::Start).await;
            drop(restart_override);
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
            let mut tx = seed.ctx.db.handle();
            let override_guard = manage_container
                .set_override(Some(get_status(&mut tx, &seed.manifest).await.backing_up()));
            manage_container.wait_for_desired(StartStop::Stop).await;
            let backup_guard = backup_guard.lock().await;
            let guard = backup_guard.mount_package_backup(&seed.manifest.id).await?;

            let res = seed
                .manifest
                .backup
                .create(
                    &seed.ctx,
                    &mut tx,
                    &seed.manifest.id,
                    &seed.manifest.title,
                    &seed.manifest.version,
                    &seed.manifest.interfaces,
                    &seed.manifest.volumes,
                )
                .await;
            guard.unmount().await?;
            drop(backup_guard);

            let return_value = res;
            state_reverter.revert().await;
            drop(override_guard);
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

#[instrument(skip_all)]
async fn configure(
    ctx: RpcContext,
    id: PackageId,
    mut configure_context: ConfigureContext,
) -> Result<BTreeMap<PackageId, TaggedDependencyError>, Error> {
    let mut db = ctx.db.handle();
    let mut tx = db.begin().await?;
    let db = &mut tx;

    let receipts = ConfigReceipts::new(db).await?;
    let id = &id;
    let ctx = &ctx;
    let overrides = &mut configure_context.overrides;
    // fetch data from db
    let action = receipts
        .config_actions
        .get(db, id)
        .await?
        .ok_or_else(|| not_found!(id))?;
    let dependencies = receipts
        .dependencies
        .get(db, id)
        .await?
        .ok_or_else(|| not_found!(id))?;
    let volumes = receipts
        .volumes
        .get(db, id)
        .await?
        .ok_or_else(|| not_found!(id))?;
    let version = receipts
        .version
        .get(db, id)
        .await?
        .ok_or_else(|| not_found!(id))?;

    // get current config and current spec
    let ConfigRes {
        config: old_config,
        spec,
    } = action.get(ctx, id, &version, &volumes).await?;

    // determine new config to use
    let mut config = if let Some(config) = configure_context.config.or_else(|| old_config.clone()) {
        config
    } else {
        spec.gen(
            &mut rand::rngs::StdRng::from_entropy(),
            &configure_context.timeout,
        )?
    };

    let manifest = receipts
        .manifest
        .get(db, id)
        .await?
        .ok_or_else(|| not_found!(id))?;

    spec.validate(&manifest)?;
    spec.matches(&config)?; // check that new config matches spec
    spec.update(
        ctx,
        db,
        &manifest,
        overrides,
        &mut config,
        &receipts.config_receipts,
    )
    .await?; // dereference pointers in the new config

    // create backreferences to pointers
    let mut sys = receipts
        .system_pointers
        .get(db, id)
        .await?
        .ok_or_else(|| not_found!(id))?;
    sys.truncate(0);
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
                if let Some(current_dependency) =
                    current_dependencies.0.get_mut(pkg_ptr.package_id())
                {
                    current_dependency.pointers.push(pkg_ptr);
                } else {
                    current_dependencies.0.insert(
                        pkg_ptr.package_id().to_owned(),
                        CurrentDependencyInfo {
                            pointers: vec![pkg_ptr],
                            health_checks: BTreeSet::new(),
                        },
                    );
                }
            }
            ValueSpecPointer::System(s) => sys.push(s),
        }
    }
    receipts.system_pointers.set(db, sys, id).await?;

    if !configure_context.dry_run {
        // run config action
        let res = action
            .set(ctx, id, &version, &dependencies, &volumes, &config)
            .await?;

        // track dependencies with no pointers
        for (package_id, health_checks) in res.depends_on.into_iter() {
            if let Some(current_dependency) = current_dependencies.0.get_mut(&package_id) {
                current_dependency.health_checks.extend(health_checks);
            } else {
                current_dependencies.0.insert(
                    package_id,
                    CurrentDependencyInfo {
                        pointers: Vec::new(),
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

    // update dependencies
    let prev_current_dependencies = receipts
        .current_dependencies
        .get(db, id)
        .await?
        .unwrap_or_default();
    remove_from_current_dependents_lists(
        db,
        id,
        &prev_current_dependencies,
        &receipts.current_dependents,
    )
    .await?; // remove previous
    add_dependent_to_current_dependents_lists(
        db,
        id,
        &current_dependencies,
        &receipts.current_dependents,
    )
    .await?; // add new
    current_dependencies.0.remove(id);
    receipts
        .current_dependencies
        .set(db, current_dependencies.clone(), id)
        .await?;

    let errs = receipts
        .dependency_errors
        .get(db, id)
        .await?
        .ok_or_else(|| not_found!(id))?;
    tracing::warn!("Dependency Errors: {:?}", errs);
    let errs = DependencyErrors::init(
        ctx,
        db,
        &manifest,
        &current_dependencies,
        &receipts.dependency_receipt.try_heal,
    )
    .await?;
    receipts.dependency_errors.set(db, errs, id).await?;

    // cache current config for dependents
    configure_context
        .overrides
        .insert(id.clone(), config.clone());

    // handle dependents
    let dependents = receipts
        .current_dependents
        .get(db, id)
        .await?
        .ok_or_else(|| not_found!(id))?;
    for (dependent, _dep_info) in dependents.0.iter().filter(|(dep_id, _)| dep_id != &id) {
        let dependent_container = receipts.docker_containers.get(db, dependent).await?;
        let dependent_container = &dependent_container;
        // check if config passes dependent check
        if let Some(cfg) = receipts
            .manifest_dependencies_config
            .get(db, (dependent, id))
            .await?
        {
            let manifest = receipts
                .manifest
                .get(db, dependent)
                .await?
                .ok_or_else(|| not_found!(id))?;
            if let Err(error) = cfg
                .check(
                    ctx,
                    dependent_container,
                    dependent,
                    &manifest.version,
                    &manifest.volumes,
                    id,
                    &config,
                )
                .await?
            {
                let dep_err = DependencyError::ConfigUnsatisfied { error };
                break_transitive(
                    db,
                    dependent,
                    id,
                    dep_err,
                    &mut configure_context.breakages,
                    &receipts.break_transitive_receipts,
                )
                .await?;
            }

            heal_all_dependents_transitive(ctx, db, id, &receipts.dependency_receipt).await?;
        }
    }

    receipts.configured.set(db, true, id).await?;

    if configure_context.dry_run {
        tx.abort().await?;
    } else {
        tx.commit().await?;
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
        match container_inspect(seed).await {
            Ok(res) => {
                match res
                    .network_settings
                    .and_then(|ns| ns.networks)
                    .and_then(|mut n| n.remove("start9"))
                    .and_then(|es| es.ip_address)
                    .filter(|ip| !ip.is_empty())
                    .map(|ip| ip.parse())
                    .transpose()
                {
                    Ok(Some(ip_addr)) => return GetRunningIp::Ip(ip_addr),
                    Ok(None) => (),
                    Err(e) => return GetRunningIp::Error(e.into()),
                }
            }
            Err(bollard::errors::Error::DockerResponseServerError {
                status_code: 404, // NOT FOUND
                ..
            }) => (),
            Err(e) => return GetRunningIp::Error(e.into()),
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
async fn container_inspect(
    seed: &ManagerSeed,
) -> Result<bollard::models::ContainerInspectResponse, bollard::errors::Error> {
    seed.ctx
        .docker
        .inspect_container(&seed.container_name, None)
        .await
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
        let mut db = seed.ctx.db.handle();
        if let Err(e) = health::check(&seed.ctx, &mut db, &seed.manifest.id).await {
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
        match container_inspect(seed).await {
            Ok(res) => {
                match res
                    .network_settings
                    .and_then(|ns| ns.networks)
                    .and_then(|mut n| n.remove("start9"))
                    .and_then(|es| es.ip_address)
                    .filter(|ip| !ip.is_empty())
                    .map(|ip| ip.parse())
                    .transpose()
                {
                    Ok(Some(ip_addr)) => return GetRunningIp::Ip(ip_addr),
                    Ok(None) => (),
                    Err(e) => return GetRunningIp::Error(e.into()),
                }
            }
            Err(bollard::errors::Error::DockerResponseServerError {
                status_code: 404, // NOT FOUND
                ..
            }) => (),
            Err(e) => return GetRunningIp::Error(e.into()),
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
        manager
            .seed
            .ctx
            .docker
            .kill_container(
                &manager.seed.container_name,
                Some(bollard::container::KillContainerOptions {
                    signal: signal.to_string(),
                }),
            )
            .await
            .or_else(|e| {
                if matches!(
                    e,
                    bollard::errors::Error::DockerResponseServerError {
                        status_code: 409, // CONFLICT
                        ..
                    } | bollard::errors::Error::DockerResponseServerError {
                        status_code: 404, // NOT FOUND
                        ..
                    }
                ) {
                    Ok(())
                } else {
                    Err(e)
                }
            })?;
    }

    Ok(())
}
