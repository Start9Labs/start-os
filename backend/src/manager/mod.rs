use std::collections::BTreeMap;
use std::net::Ipv4Addr;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::Arc;
use std::task::Poll;
use std::time::Duration;

use color_eyre::eyre::eyre;
use embassy_container_init::ProcessGroupId;
use futures::future::BoxFuture;
use futures::{FutureExt, TryFutureExt};
use helpers::UnixRpcClient;
use models::ErrorKind;
use nix::sys::signal::Signal;
use persistent_container::PersistentContainer;
use start_stop::StartStop;
use tokio::sync::{oneshot, Notify};
use tokio::sync::{
    watch::{self, Receiver, Sender},
    Mutex,
};
use torut::onion::TorSecretKeyV3;
use tracing::instrument;
use transition_state::TransitionState;

use crate::backup::target::PackageBackupInfo;
use crate::backup::PackageBackupReport;
use crate::context::RpcContext;
use crate::disk::mount::backup::BackupMountGuard;
use crate::disk::mount::guard::TmpMountGuard;
use crate::net::interface::InterfaceId;
use crate::net::GeneratedCertificateMountPoint;
use crate::procedure::docker::{DockerContainer, DockerProcedure, LongRunning};
use crate::procedure::{NoOutput, PackageProcedure, ProcedureName};
use crate::s9pk::manifest::Manifest;
use crate::status::MainStatus;
use crate::util::NonDetachingJoinHandle;
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
type BackupTask = Result<PackageBackupInfo, Error>;
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
        let previous;
        self.next_gid.send_modify(|x| {
            previous = *x;
            *x = previous + 1;
        });
        ProcessGroupId(previous)
    }

    pub fn new_main_gid(&self) -> ProcessGroupId {
        let gid = self.new_gid();
        self.main_gid.send(gid);
        gid
    }
}

#[derive(Clone)]
struct Manager {
    seed: Arc<ManagerSeed>,

    manage_container: Arc<manager_container::ManageContainer>,
    transition: Arc<watch::Sender<Arc<TransitionState>>>,
    persistent_container: ManagerPersistentContainer,

    pub gid: Arc<Gid>,
}
impl Manager {
    pub async fn new(
        ctx: RpcContext,
        manifest: Manifest,
        tor_keys: BTreeMap<InterfaceId, TorSecretKeyV3>,
    ) -> Result<Self, Error> {
        let seed = Arc::new(ManagerSeed {
            ctx,
            container_name: DockerProcedure::container_name(&manifest.id, None),
            manifest,
            tor_keys,
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

    pub fn start(&self) -> Result<(), Error> {
        self._transition_abort();
        self.manage_container.to_desired(StartStop::Start);
        Ok(())
    }
    pub fn stop(&self) -> Result<(), Error> {
        self._transition_abort();
        self.manage_container.to_desired(StartStop::Stop);
        Ok(())
    }
    pub async fn restart(&self) {
        if self._is_transition_restart() {
            return;
        }
        self._transition_replace(self._transition_restart());
    }
    pub async fn configure(&self) -> Result<(), Error> {
        todo!("BLUJ")
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
            current_status.changed().await;
        }
    }

    pub async fn signal(&self, signal: Signal) -> Result<(), Error> {
        let rpc_client = self.rpc_client();
        let seed = self.seed.clone();
        let git = self.gid.clone();
        send_signal(rpc_client, seed, git, signal).await
    }

    pub fn rpc_client(&self) -> Option<Arc<UnixRpcClient>> {
        self.persistent_container.clone().map(|x| x.rpc_client())
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
    fn _transition_restart(&self) -> TransitionState {
        let transition = self.transition.clone();
        let manage_container = self.manage_container.clone();
        TransitionState::Restarting(tokio::spawn(async move {
            let mut desired_state = manage_container.desired_state();
            let mut current_state = manage_container.current_state();
            let _ = manage_container.set_override(Some(MainStatus::Restarting));
            manage_container.to_desired(StartStop::Stop);
            while current_state.changed().await.is_ok() && current_state.borrow().is_start() {}
            manage_container.to_desired(StartStop::Start);
            while current_state.changed().await.is_ok() && current_state.borrow().is_stop() {}
            transition.send_replace(Default::default());
        }))
    }
    fn _transition_backup(
        &self,
        mut backup_guard: BackupGuard,
    ) -> (TransitionState, BoxFuture<BackupReturn>) {
        let transition = self.transition.clone();
        let manage_container = self.manage_container.clone();
        let seed = self.seed.clone();
        let (send, done) = oneshot::channel();
        (
            TransitionState::BackingUp(tokio::spawn(
                async move {
                    let mut desired_state = manage_container.desired_state();
                    let state_reverter = DesiredStateReverter::new(manage_container.clone());
                    let starting_desired = desired_state.borrow().clone();
                    let mut current_state = manage_container.current_state();
                    let mut tx = seed.ctx.db.handle();
                    let _ = manage_container.set_override(Some(
                        get_status(&mut tx, &seed.manifest).await?.backing_up(),
                    ));
                    manage_container.to_desired(StartStop::Stop);
                    while current_state.changed().await.is_ok() && current_state.borrow().is_start()
                    {
                    }

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
                    Ok::<_, Error>(return_value)
                }
                .then(finnish_up_backup_task(self.transition.clone(), send)), //
            )),
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
}

struct DesiredStateReverter {
    manage_container: Option<Arc<ManageContainer>>,
    starting_state: StartStop,
}
impl DesiredStateReverter {
    fn new(manage_container: Arc<ManageContainer>) -> Self {
        let starting_state = manage_container.desired_state().borrow().clone();
        let manage_container = Some(manage_container);
        Self {
            starting_state,
            manage_container,
        }
    }
    async fn revert(mut self) {
        if let Some(mut current_state) = self._revert() {
            while current_state.changed().await.is_ok()
                && &*current_state.borrow() != &self.starting_state
            {}
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
            });
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
        Ok(a) | Ok(a) => BackupReturn::Ran {
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
pub struct ManagerSharedState {
    seed: Arc<ManagerSeed>,
    persistent_container: Option<PersistentContainer>,
    status: (Sender<Status>, Receiver<Status>),
    killer: Notify,
    on_stop: Sender<OnStop>,
    synchronized: Notify,
    synchronize_now: Notify,
    commit_health_check_results: AtomicBool,
    next_gid: AtomicU32,
    main_gid: (Sender<ProcessGroupId>, Receiver<ProcessGroupId>),
}

#[derive(Debug, Clone, Copy)]
pub enum OnStop {
    Restart,
    Sleep,
    Exit,
}

type RunMainResult = Result<Result<NoOutput, (i32, String)>, Error>;

#[instrument(skip(seed, persistent_container, started))]
async fn run_main(
    seed: Arc<ManagerSeed>,
    persistent_container: ManagerPersistentContainer,
    started: Arc<impl Fn()>,
) -> RunMainResult {
    let interfaces = main_interfaces(&seed)?;
    let generated_certificate = generate_certificate(&seed, &interfaces).await?;

    let mut runtime = NonDetachingJoinHandle::from(tokio::spawn(start_up_image(
        seed.clone(),
        generated_certificate,
    )));
    let ip = match persistent_container.is_some() {
        false => Some(match get_running_ip(&seed, &mut runtime).await {
            GetRunningIp::Ip(x) => x,
            GetRunningIp::Error(e) => return Err(e),
            GetRunningIp::EarlyExit(x) => return Ok(x),
        }),
        true => None,
    };

    if let Some(ip) = ip {
        add_network_for_main(&seed, ip, interfaces, generated_certificate).await?;
    }
    started();
    let health = main_health_check_daemon(seed.clone());
    let res = tokio::select! {
        a = runtime => a.map_err(|_| Error::new(eyre!("Manager runtime panicked!"), crate::ErrorKind::Docker)).and_then(|a| a),
        _ = health => Err(Error::new(eyre!("Health check daemon exited!"), crate::ErrorKind::Unknown))
    };
    if let Some(ip) = ip {
        remove_network_for_main(&seed, ip).await?;
    }
    res
}

/// We want to start up the manifest, but in this case we want to know that we have generated the certificates.
/// Note for _generated_certificate: Needed to know that before we start the state we have generated the certificate
async fn start_up_image(
    seed: Arc<ManagerSeed>,
    _generated_certificate: GeneratedCertificateMountPoint,
) -> Result<Result<NoOutput, (i32, String)>, Error> {
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

fn main_interfaces(
    seed: &ManagerSeed,
) -> Result<
    Vec<(
        InterfaceId,
        &crate::net::interface::Interface,
        TorSecretKeyV3,
    )>,
    Error,
> {
    seed.manifest
        .interfaces
        .0
        .iter()
        .map(|(id, info)| {
            Ok((
                id.clone(),
                info,
                seed.tor_keys
                    .get(id)
                    .ok_or_else(|| {
                        Error::new(eyre!("interface {} missing key", id), crate::ErrorKind::Tor)
                    })?
                    .clone(),
            ))
        })
        .collect::<Result<Vec<_>, Error>>()
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

async fn generate_certificate(
    seed: &ManagerSeed,
    interfaces: &Vec<(
        InterfaceId,
        &crate::net::interface::Interface,
        TorSecretKeyV3,
    )>,
) -> Result<GeneratedCertificateMountPoint, Error> {
    seed.ctx
        .net_controller
        .generate_certificate_mountpoint(&seed.manifest.id, interfaces)
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

async fn container_inspect(
    seed: &ManagerSeed,
) -> Result<bollard::models::ContainerInspectResponse, bollard::errors::Error> {
    seed.ctx
        .docker
        .inspect_container(&seed.container_name, None)
        .await
}

async fn add_network_for_main(
    seed: &ManagerSeed,
    ip: std::net::Ipv4Addr,
    generated_certificate: GeneratedCertificateMountPoint,
) -> Result<(), Error> {
    // seed.ctx
    //     .net_controller
    //     .add(
    //         &mut seed.ctx.secret_store.acquire().await?,
    //         &seed.manifest.id,
    //         ip,
    //         interfaces,
    //         generated_certificate,
    //     )
    //     .await?;
    todo!()
}

async fn remove_network_for_main(seed: &ManagerSeed, ip: std::net::Ipv4Addr) -> Result<(), Error> {
    Ok(())
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

fn set_commit_health_true(state: &Arc<ManagerSharedState>) {
    state
        .commit_health_check_results
        .store(true, Ordering::SeqCst);
}

async fn add_network_for_main(
    seed: &ManagerSeed,
    ip: std::net::Ipv4Addr,
    interfaces: Vec<(
        InterfaceId,
        &crate::net::interface::Interface,
        TorSecretKeyV3,
    )>,
    generated_certificate: GeneratedCertificateMountPoint,
) -> Result<(), Error> {
    // seed.ctx
    //     .net_controller
    //     .add(
    //         &mut seed.ctx.secret_store.acquire().await?,
    //         &seed.manifest.id,
    //         ip,
    //         interfaces,
    //         generated_certificate,
    //     )
    //     .await?;
    todo!()
}

async fn remove_network_for_main(seed: &ManagerSeed, ip: std::net::Ipv4Addr) -> Result<(), Error> {
    Ok(())
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

fn set_commit_health_true(state: &Arc<ManagerSharedState>) {
    state
        .commit_health_check_results
        .store(true, Ordering::SeqCst);
}

type RuntimeOfCommand = NonDetachingJoinHandle<Result<Result<NoOutput, (i32, String)>, Error>>;

async fn get_running_ip(seed: &ManagerSeed, mut runtime: &mut RuntimeOfCommand) -> GetRunningIp {
    loop {
        match container_inspect(&seed).await {
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

async fn wait_for_status(shared: &ManagerSharedState, status: Status) {
    let mut recv = shared.status.0.subscribe();
    while {
        let s = *recv.borrow();
        s != status
    } {
        if recv.changed().await.is_ok() {
            break;
        }
    }
}

fn sigterm_timeout(manifest: &Manifest) -> Option<Duration> {
    if let PackageProcedure::Docker(d) = &manifest.main {
        d.sigterm_timeout.map(|d| *d)
    } else if let Some(c) = &manifest.containers {
        c.main.sigterm_timeout.map(|d| *d)
    } else {
        None
    }
}

async fn send_signal(
    rpc_client: Option<Arc<UnixRpcClient>>,
    seed: Arc<ManagerSeed>,
    gid: Arc<Gid>,
    signal: Signal,
) -> Result<(), Error> {
    // stop health checks from committing their results
    // shared
    //     .commit_health_check_results
    //     .store(false, Ordering::SeqCst);

    if let Some(rpc_client) = rpc_client {
        let main_gid = gid.main_gid.borrow().clone();
        let next_gid = gid.new_gid();
        #[cfg(feature = "js_engine")]
        if let Err(e) = crate::procedure::js_scripts::JsProcedure::default()
            .execute::<_, NoOutput>(
                &seed.ctx.datadir,
                &seed.manifest.id,
                &seed.manifest.version,
                ProcedureName::Signal,
                &seed.manifest.volumes,
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
        seed.ctx
            .docker
            .kill_container(
                &seed.container_name,
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
