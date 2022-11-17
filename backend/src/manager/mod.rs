use std::collections::BTreeMap;
use std::future::Future;
use std::net::Ipv4Addr;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::task::Poll;
use std::time::Duration;

use bollard::container::{KillContainerOptions, StopContainerOptions};
use color_eyre::eyre::eyre;
use embassy_container_init::{InputJsonRpc, RpcId};
use models::{ExecCommand, SendKillSignal};
use nix::sys::signal::Signal;
use patch_db::DbHandle;
use sqlx::{Executor, Postgres};
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::watch::error::RecvError;
use tokio::sync::watch::{channel, Receiver, Sender};
use tokio::sync::{oneshot, Mutex, Notify, RwLock};
use tokio_stream::wrappers::UnboundedReceiverStream;
use torut::onion::TorSecretKeyV3;
use tracing::instrument;

use crate::context::RpcContext;
use crate::manager::sync::synchronizer;
use crate::net::interface::InterfaceId;
use crate::net::GeneratedCertificateMountPoint;
use crate::notifications::NotificationLevel;
use crate::procedure::docker::{DockerContainer, DockerProcedure, LongRunning};
use crate::procedure::{NoOutput, PackageProcedure, ProcedureName};
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::status::MainStatus;
use crate::util::{Container, NonDetachingJoinHandle, Version};
use crate::Error;

pub mod health;
mod sync;

pub const HEALTH_CHECK_COOLDOWN_SECONDS: u64 = 15;
pub const HEALTH_CHECK_GRACE_PERIOD_SECONDS: u64 = 5;

#[derive(Default)]
pub struct ManagerMap(RwLock<BTreeMap<(PackageId, Version), Arc<Manager>>>);
impl ManagerMap {
    #[instrument(skip(self, ctx, db, secrets))]
    pub async fn init<Db: DbHandle, Ex>(
        &self,
        ctx: &RpcContext,
        db: &mut Db,
        secrets: &mut Ex,
    ) -> Result<(), Error>
    where
        for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
    {
        let mut res = BTreeMap::new();
        for package in crate::db::DatabaseModel::new()
            .package_data()
            .keys(db, true)
            .await?
        {
            let man: Manifest = if let Some(manifest) = crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(&package)
                .and_then(|pkg| pkg.installed())
                .map(|m| m.manifest())
                .get(db, true)
                .await?
                .to_owned()
            {
                manifest
            } else {
                continue;
            };

            let tor_keys = man.interfaces.tor_keys(secrets, &package).await?;
            res.insert(
                (package, man.version.clone()),
                Arc::new(Manager::create(ctx.clone(), man, tor_keys).await?),
            );
        }
        *self.0.write().await = res;
        Ok(())
    }

    #[instrument(skip(self, ctx))]
    pub async fn add(
        &self,
        ctx: RpcContext,
        manifest: Manifest,
        tor_keys: BTreeMap<InterfaceId, TorSecretKeyV3>,
    ) -> Result<(), Error> {
        let mut lock = self.0.write().await;
        let id = (manifest.id.clone(), manifest.version.clone());
        if let Some(man) = lock.remove(&id) {
            if !man.thread.is_empty().await {
                man.exit().await?;
            }
        }
        lock.insert(
            id,
            Arc::new(Manager::create(ctx, manifest, tor_keys).await?),
        );
        Ok(())
    }

    #[instrument(skip(self))]
    pub async fn remove(&self, id: &(PackageId, Version)) {
        if let Some(man) = self.0.write().await.remove(id) {
            if let Err(e) = man.exit().await {
                tracing::error!("Error shutting down manager: {}", e);
                tracing::debug!("{:?}", e);
            }
        }
    }

    #[instrument(skip(self))]
    pub async fn empty(&self) -> Result<(), Error> {
        let res =
            futures::future::join_all(std::mem::take(&mut *self.0.write().await).into_iter().map(
                |((id, version), man)| async move {
                    tracing::debug!("Manager for {}@{} shutting down", id, version);
                    man.exit().await?;
                    tracing::debug!("Manager for {}@{} is shutdown", id, version);
                    if let Err(e) = Arc::try_unwrap(man) {
                        tracing::trace!(
                            "Manager for {}@{} still has {} other open references",
                            id,
                            version,
                            Arc::strong_count(&e) - 1
                        );
                    }
                    Ok::<_, Error>(())
                },
            ))
            .await;
        res.into_iter().fold(Ok(()), |res, x| match (res, x) {
            (Ok(()), x) => x,
            (Err(e), Ok(())) => Err(e),
            (Err(e1), Err(e2)) => Err(Error::new(eyre!("{}, {}", e1.source, e2.source), e1.kind)),
        })
    }

    #[instrument(skip(self))]
    pub async fn get(&self, id: &(PackageId, Version)) -> Option<Arc<Manager>> {
        self.0.read().await.get(id).cloned()
    }
}

pub struct Manager {
    shared: Arc<ManagerSharedState>,
    thread: Container<NonDetachingJoinHandle<()>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Status {
    Starting,
    Running,
    Stopped,
    Paused,
    Shutdown,
}

struct ManagerSeed {
    ctx: RpcContext,
    manifest: Manifest,
    container_name: String,
    tor_keys: BTreeMap<InterfaceId, TorSecretKeyV3>,
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
}

#[derive(Debug, Clone, Copy)]
pub enum OnStop {
    Restart,
    Sleep,
    Exit,
}

#[instrument(skip(state))]
async fn run_main(
    state: &Arc<ManagerSharedState>,
) -> Result<Result<NoOutput, (i32, String)>, Error> {
    let rt_state = state.clone();
    let interfaces = main_interfaces(&*state.seed)?;
    let generated_certificate = generate_certificate(&*state.seed, &interfaces).await?;

    let mut runtime = NonDetachingJoinHandle::from(tokio::spawn(start_up_image(
        rt_state,
        generated_certificate,
    )));
    let ip = match state.persistent_container.is_some() {
        false => Some(match get_running_ip(state, &mut runtime).await {
            GetRunningIp::Ip(x) => x,
            GetRunningIp::Error(e) => return Err(e),
            GetRunningIp::EarlyExit(x) => return Ok(x),
        }),
        true => None,
    };

    if let Some(ip) = ip {
        add_network_for_main(&*state.seed, ip, interfaces, generated_certificate).await?;
    }

    set_commit_health_true(state);
    let health = main_health_check_daemon(state.clone());
    fetch_starting_to_running(state);
    let res = tokio::select! {
        a = runtime => a.map_err(|_| Error::new(eyre!("Manager runtime panicked!"), crate::ErrorKind::Docker)).and_then(|a| a),
        _ = health => Err(Error::new(eyre!("Health check daemon exited!"), crate::ErrorKind::Unknown)),
        _ = state.killer.notified() => Ok(Err((137, "Killed".to_string())))
    };
    if let Some(ip) = ip {
        remove_network_for_main(&*state.seed, ip).await?;
    }
    res
}

/// We want to start up the manifest, but in this case we want to know that we have generated the certificates.
/// Note for _generated_certificate: Needed to know that before we start the state we have generated the certificate
async fn start_up_image(
    rt_state: Arc<ManagerSharedState>,
    _generated_certificate: GeneratedCertificateMountPoint,
) -> Result<Result<NoOutput, (i32, String)>, Error> {
    rt_state
        .seed
        .manifest
        .main
        .execute::<(), NoOutput>(
            &rt_state.seed.ctx,
            &rt_state.seed.manifest.id,
            &rt_state.seed.manifest.version,
            ProcedureName::Main,
            &rt_state.seed.manifest.volumes,
            None,
            None,
        )
        .await
}

impl Manager {
    #[instrument(skip(ctx))]
    async fn create(
        ctx: RpcContext,
        manifest: Manifest,
        tor_keys: BTreeMap<InterfaceId, TorSecretKeyV3>,
    ) -> Result<Self, Error> {
        let (on_stop, recv) = channel(OnStop::Sleep);
        let seed = Arc::new(ManagerSeed {
            ctx,
            container_name: DockerProcedure::container_name(&manifest.id, None),
            manifest,
            tor_keys,
        });
        let persistent_container = PersistentContainer::init(&seed).await?;
        let shared = Arc::new(ManagerSharedState {
            seed,
            persistent_container,
            status: channel(Status::Stopped),
            killer: Notify::new(),
            on_stop,
            synchronized: Notify::new(),
            synchronize_now: Notify::new(),
            commit_health_check_results: AtomicBool::new(true),
        });
        shared.synchronize_now.notify_one();
        let thread_shared = shared.clone();
        let thread = NonDetachingJoinHandle::from(tokio::spawn(async move {
            tokio::select! {
                _ = manager_thread_loop(recv, &thread_shared) => (),
                _ = synchronizer(&*thread_shared) => (),
            }
        }));
        Ok(Manager {
            shared,
            thread: Container::new(Some(thread)),
        })
    }

    pub async fn signal(&self, signal: &Signal) -> Result<(), Error> {
        // stop health checks from committing their results
        self.shared
            .commit_health_check_results
            .store(false, Ordering::SeqCst);

        // send signal to container
        self.shared
            .seed
            .ctx
            .docker
            .kill_container(
                &self.shared.seed.container_name,
                Some(KillContainerOptions {
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
        Ok(())
    }

    #[instrument(skip(self))]
    async fn exit(&self) -> Result<(), Error> {
        self.shared
            .commit_health_check_results
            .store(false, Ordering::SeqCst);
        let _ = self.shared.on_stop.send(OnStop::Exit);

        match self
            .shared
            .seed
            .ctx
            .docker
            .stop_container(
                &self.shared.seed.container_name,
                Some(StopContainerOptions {
                    t: sigterm_timeout(&self.shared.seed.manifest)
                        .map(|d| d.as_secs())
                        .unwrap_or(30) as i64,
                }),
            )
            .await
        {
            Err(bollard::errors::Error::DockerResponseServerError {
                status_code: 404, // NOT FOUND
                ..
            })
            | Err(bollard::errors::Error::DockerResponseServerError {
                status_code: 409, // CONFLICT
                ..
            })
            | Err(bollard::errors::Error::DockerResponseServerError {
                status_code: 304, // NOT MODIFIED
                ..
            }) => (), // Already stopped
            a => a?,
        };

        if let Some(thread) = self.thread.take().await {
            thread.await.map_err(|e| {
                Error::new(
                    eyre!("Manager thread panicked: {}", e),
                    crate::ErrorKind::Docker,
                )
            })?;
        }
        Ok(())
    }
    /// this will depend on locks to main status. if you hold any locks when calling this function that conflict, this will deadlock
    pub async fn synchronize(&self) {
        self.shared.synchronize_now.notify_waiters();
        self.shared.synchronized.notified().await
    }

    pub fn exec_command(&self) -> Option<ExecCommand> {
        self.shared
            .persistent_container
            .as_ref()
            .map(|p| p.exec_command())
    }
    pub fn term_command(&self) -> Option<SendKillSignal> {
        self.shared
            .persistent_container
            .as_ref()
            .map(|p| p.term_command())
    }
}

async fn manager_thread_loop(mut recv: Receiver<OnStop>, thread_shared: &Arc<ManagerSharedState>) {
    loop {
        fn handle_stop_action<'a>(
            recv: &'a mut Receiver<OnStop>,
        ) -> (
            OnStop,
            Option<impl Future<Output = Result<(), RecvError>> + 'a>,
        ) {
            let val = *recv.borrow_and_update();
            match val {
                OnStop::Sleep => (OnStop::Sleep, Some(recv.changed())),
                a => (a, None),
            }
        }
        let (stop_action, fut) = handle_stop_action(&mut recv);
        match stop_action {
            OnStop::Sleep => {
                if let Some(fut) = fut {
                    let _ = thread_shared.status.0.send(Status::Stopped);
                    fut.await.unwrap();
                    continue;
                }
            }
            OnStop::Exit => {
                let _ = thread_shared.status.0.send(Status::Shutdown);
                break;
            }
            OnStop::Restart => {
                let _ = thread_shared.status.0.send(Status::Running);
            }
        }
        match run_main(thread_shared).await {
            Ok(Ok(NoOutput)) => (), // restart
            Ok(Err(e)) => {
                if cfg!(feature = "unstable") {
                    let mut db = thread_shared.seed.ctx.db.handle();
                    let started = crate::db::DatabaseModel::new()
                        .package_data()
                        .idx_model(&thread_shared.seed.manifest.id)
                        .and_then(|pde| pde.installed())
                        .map::<_, MainStatus>(|i| i.status().main())
                        .get(&mut db, false)
                        .await;
                    match started.as_deref() {
                        Ok(Some(MainStatus::Running { .. })) => {
                            let res = thread_shared.seed.ctx.notification_manager
                                .notify(
                                    &mut db,
                                    Some(thread_shared.seed.manifest.id.clone()),
                                    NotificationLevel::Warning,
                                    String::from("Service Crashed"),
                                    format!("The service {} has crashed with the following exit code: {}\nDetails: {}", thread_shared.seed.manifest.id.clone(), e.0, e.1),
                                    (),
                                    Some(3600) // 1 hour
                                )
                                .await;
                            if let Err(e) = res {
                                tracing::error!("Failed to issue notification: {}", e);
                                tracing::debug!("{:?}", e);
                            }
                        }
                        _ => {
                            tracing::error!("service just started. not issuing crash notification")
                        }
                    }
                }
                tracing::error!("service crashed: {}: {}", e.0, e.1);
                tokio::time::sleep(Duration::from_secs(15)).await;
            }
            Err(e) => {
                tracing::error!("failed to start service: {}", e);
                tracing::debug!("{:?}", e);
            }
        }
    }
}

struct LongRunningHandle(NonDetachingJoinHandle<()>);
pub struct CommandInserter {
    command_counter: AtomicUsize,
    input: UnboundedSender<InputJsonRpc>,
    outputs: Arc<Mutex<BTreeMap<RpcId, UnboundedSender<embassy_container_init::Output>>>>,
}
impl Drop for CommandInserter {
    fn drop(&mut self) {
        use embassy_container_init::{Input, JsonRpc};
        let CommandInserter {
            command_counter,
            input,
            outputs: _,
        } = self;
        let upper: usize = command_counter.load(Ordering::Relaxed);
        for i in 0..upper {
            let _ignored_result = input.send(JsonRpc::new(RpcId::UInt(i as u32), Input::Kill()));
        }
    }
}
impl CommandInserter {
    fn new(
        long_running: LongRunning,
        input: UnboundedSender<InputJsonRpc>,
    ) -> (LongRunningHandle, Self) {
        let LongRunning {
            mut output,
            running_output,
        } = long_running;
        let command_counter = AtomicUsize::new(0);
        let outputs: Arc<Mutex<BTreeMap<RpcId, UnboundedSender<embassy_container_init::Output>>>> =
            Default::default();
        let handle = LongRunningHandle(running_output);
        tokio::spawn({
            let outputs = outputs.clone();
            async move {
                while let Some(output) = output.recv().await {
                    let (id, output) = output.into_pair();
                    let mut outputs = outputs.lock().await;
                    let output_sender = outputs.get_mut(&id);
                    if let Some(output_sender) = output_sender {
                        if let Err(err) = output_sender.send(output) {
                            tracing::warn!("Could no longer send an output");
                            tracing::debug!("{err:?}");
                            outputs.remove(&id);
                        }
                    }
                }
            }
        });

        (
            handle,
            Self {
                command_counter,
                input,
                outputs,
            },
        )
    }

    pub async fn exec_command(
        &self,
        command: String,
        args: Vec<String>,
        sender: UnboundedSender<embassy_container_init::Output>,
        timeout: Option<Duration>,
    ) -> Option<RpcId> {
        use embassy_container_init::{Input, JsonRpc};
        let mut outputs = self.outputs.lock().await;
        let command_counter = self.command_counter.fetch_add(1, Ordering::SeqCst) as u32;
        let command_id = RpcId::UInt(command_counter);
        outputs.insert(command_id, sender);
        if let Some(timeout) = timeout {
            tokio::spawn({
                let input = self.input.clone();
                let command_id = command_id;
                async move {
                    tokio::time::sleep(timeout).await;
                    let _ignored_output = input.send(JsonRpc::new(command_id, Input::Kill()));
                }
            });
        }
        if let Err(err) = self
            .input
            .send(JsonRpc::new(command_id, Input::Command { command, args }))
        {
            tracing::warn!("Trying to send to input but can't");
            tracing::debug!("{err:?}");
            return None;
        }

        Some(command_id)
    }
    pub async fn send_kill_command(&self, id: RpcId, command: u32) {
        use embassy_container_init::{Input, JsonRpc};
        self.outputs.lock().await.remove(&id);
        let _ignored_term = self
            .input
            .send(JsonRpc::new(id, Input::SendSignal(command)));
    }

    pub async fn term_all(&self) {
        for i in 0..self.command_counter.load(Ordering::Relaxed) {
            self.send_kill_command(RpcId::UInt(i as u32), 15).await;
        }
    }
}

pub struct PersistentContainer {
    container_name: String,
    running_docker: NonDetachingJoinHandle<()>,
    command_inserter: Receiver<Arc<CommandInserter>>,
}

impl PersistentContainer {
    #[instrument(skip(seed))]
    async fn init(seed: &Arc<ManagerSeed>) -> Result<Option<Self>, Error> {
        Ok(if let Some(containers) = &seed.manifest.containers {
            let (running_docker, command_inserter) =
                spawn_persistent_container(seed.clone(), containers.main.clone()).await?;
            Some(Self {
                container_name: DockerProcedure::container_name(&seed.manifest.id, None),
                running_docker,
                command_inserter,
            })
        } else {
            None
        })
    }

    #[instrument(skip(self))]
    async fn exit(&self) {
        let container_name = &self.container_name;
        use tokio::process::Command;
        if let Err(_err) = Command::new("docker")
            .args(["stop", "-t", "30", container_name])
            .output()
            .await
        {}
    }

    fn term_command(&self) -> SendKillSignal {
        let cloned = self.command_inserter.clone();
        Arc::new(move |id, signal| {
            let cloned = cloned.clone();
            Box::pin(async move {
                let command_inserter = { cloned.borrow().clone() };
                command_inserter.send_kill_command(id, signal).await;
                Ok::<(), String>(())
            })
        })
    }

    fn exec_command(&self) -> ExecCommand {
        use std::collections::BTreeSet;
        use tokio::sync::oneshot;
        let cloned = self.command_inserter.clone();

        let (trigger_drop, drop) = oneshot::channel::<BTreeSet<RpcId>>();
        tokio::spawn({
            let command_inserter = self.command_inserter.clone();
            async move {
                let ids = match drop.await {
                    Err(err) => {
                        tracing::warn!("Channel cleanup issue {err:?}");
                        return;
                    }
                    Ok(a) => a,
                };
                let command_inserter_lock = command_inserter.lock().await;
                let command_inserter = match &*command_inserter_lock {
                    Some(a) => a,
                    None => {
                        return;
                    }
                };
                for id in ids {
                    command_inserter.send_kill_command(id, 9).await;
                }
            }
        });

        /// A handle that on drop will clean all the ids that are inserter in the fn.
        struct Cleaner {
            command_inserter: Arc<Mutex<Option<CommandInserter>>>,
            ids: BTreeSet<RpcId>,
            trigger_drop: Option<oneshot::Sender<BTreeSet<RpcId>>>,
        }
        impl Drop for Cleaner {
            fn drop(&mut self) {
                if let Some(trigger_drop) = self.trigger_drop.take() {
                    trigger_drop.send(::std::mem::take(&mut self.ids));
                }
            }
        }
        let cleaner = Arc::new(Mutex::new(Cleaner {
            command_inserter: cloned.clone(),
            ids: Default::default(),
            trigger_drop: Some(trigger_drop),
        }));
        Arc::new(move |command, args, sender, timeout| {
            let cloned = cloned.clone();
            let cleaner = cleaner.clone();
            Box::pin(async move {
                let lock = cloned.lock().await;
                let id = match &*lock {
                    Some(command_inserter) => {
                        if let Some(id) = command_inserter
                            .exec_command(command.clone(), args.clone(), sender, timeout)
                            .await
                        {
                            let mut cleaner = cleaner.lock().await;
                            cleaner.ids.insert(id);
                            id
                        } else {
                            return Err("Couldn't get command started ".to_string());
                        }
                    }
                    None => {
                        return Err("Expecting containers.main in the package manifest".to_string())
                    }
                };
                Ok::<RpcId, String>(id)
            })
        })
    }
}

async fn spawn_persistent_container(
    seed: Arc<ManagerSeed>,
    container: DockerContainer,
) -> Result<(NonDetachingJoinHandle<()>, Receiver<Arc<CommandInserter>>), Error> {
    let (send_inserter, inserter) = oneshot::channel();
    Ok((
        tokio::task::spawn(async move {
            let mut inserter_send: Option<Sender<Arc<CommandInserter>>> = None;
            let mut send_inserter: Option<oneshot::Sender<Receiver<Arc<CommandInserter>>>> = Some(send_inserter);
            loop {
                if let Err(e) = async {
                    let interfaces = main_interfaces(&*seed)?;
                    let generated_certificate = generate_certificate(&*seed, &interfaces).await?;
                    let (mut runtime, inserter) =
                        long_running_docker(&seed, &container).await?;

                    let ip = match get_long_running_ip(&*seed, &mut runtime).await {
                        GetRunningIp::Ip(x) => x,
                        GetRunningIp::Error(e) => return Err(e),
                        GetRunningIp::EarlyExit(e) => {
                            tracing::error!("Early Exit");
                            tracing::debug!("{:?}", e);
                            return Ok(());
                        }
                    };
                    add_network_for_main(&*seed, ip, interfaces, generated_certificate).await?;

                    if let Some(inserter_send) = inserter_send.as_mut() {
                        let _ = inserter_send.send(Arc::new(inserter));
                    } else {
                        let (s, r) = channel(Arc::new(inserter));
                        inserter_send = Some(s);
                        if let Some(send_inserter) = send_inserter.take() {
                            let _ = send_inserter.send(r);
                        }
                    }

                    let res = tokio::select! {
                        a = runtime.0 => a.map_err(|_| Error::new(eyre!("Manager runtime panicked!"), crate::ErrorKind::Docker)).map(|_| ()),
                    };

                    remove_network_for_main(&*seed, ip).await?;

                    res
                }.await {
                    tracing::error!("Error in persistent container: {}", e);
                    tracing::debug!("{:?}", e);
                } else {
                    break;
                }
            }
        })
        .into(),
        inserter.await.map_err(|_| Error::new(eyre!("Container handle dropped before inserter sent"), crate::ErrorKind::Unknown))?,
    ))
}

async fn long_running_docker(
    seed: &ManagerSeed,
    container: &DockerContainer,
) -> Result<(LongRunningHandle, CommandInserter), Error> {
    let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();
    let long_running = container
        .long_running_execute(
            &seed.ctx,
            &seed.manifest.id,
            &seed.manifest.version,
            &seed.manifest.volumes,
            UnboundedReceiverStream::new(receiver),
        )
        .await?;
    Ok(CommandInserter::new(long_running, sender))
}

async fn remove_network_for_main(seed: &ManagerSeed, ip: std::net::Ipv4Addr) -> Result<(), Error> {
    seed.ctx
        .net_controller
        .remove(
            &seed.manifest.id,
            ip,
            seed.manifest.interfaces.0.keys().cloned(),
        )
        .await?;
    Ok(())
}

fn fetch_starting_to_running(state: &Arc<ManagerSharedState>) {
    let _ = state.status.0.send_modify(|x| {
        if *x == Status::Starting {
            *x = Status::Running;
        }
    });
}

async fn main_health_check_daemon(state: Arc<ManagerSharedState>) {
    tokio::time::sleep(Duration::from_secs(HEALTH_CHECK_GRACE_PERIOD_SECONDS)).await;
    loop {
        let mut db = state.seed.ctx.db.handle();
        if let Err(e) = health::check(
            &state.seed.ctx,
            &mut db,
            &state.seed.manifest.id,
            &state.commit_health_check_results,
        )
        .await
        {
            tracing::error!(
                "Failed to run health check for {}: {}",
                &state.seed.manifest.id,
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
    seed.ctx
        .net_controller
        .add(&seed.manifest.id, ip, interfaces, generated_certificate)
        .await?;
    Ok(())
}

enum GetRunningIp {
    Ip(Ipv4Addr),
    Error(Error),
    EarlyExit(Result<NoOutput, (i32, String)>),
}

type RuntimeOfCommand = NonDetachingJoinHandle<Result<Result<NoOutput, (i32, String)>, Error>>;

async fn get_running_ip(
    state: &Arc<ManagerSharedState>,
    mut runtime: &mut RuntimeOfCommand,
) -> GetRunningIp {
    loop {
        match container_inspect(&*state.seed).await {
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
                Err(_) | Ok(Err(_)) => {
                    return GetRunningIp::Error(Error::new(
                        eyre!("Manager runtime panicked!"),
                        crate::ErrorKind::Docker,
                    ))
                }
            }
        }
    }
}

async fn get_long_running_ip(seed: &ManagerSeed, runtime: &mut LongRunningHandle) -> GetRunningIp {
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
        if let Poll::Ready(res) = futures::poll!(&mut runtime.0) {
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

async fn wait_for_status(shared: &ManagerSharedState, status: Status) {
    let mut recv = shared.status.0.subscribe();
    while *recv.borrow() != status {
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

#[instrument(skip(shared))]
async fn stop(shared: &ManagerSharedState) -> Result<(), Error> {
    shared
        .commit_health_check_results
        .store(false, Ordering::SeqCst);
    shared.on_stop.send(OnStop::Sleep).map_err(|_| {
        Error::new(
            eyre!("Manager has already been shutdown"),
            crate::ErrorKind::Docker,
        )
    })?;
    if *shared.status.1.borrow() == Status::Paused {
        resume(shared).await?;
    }
    // shared.signal.send(SigTerm);
    let _ = tokio::time::timeout(
        sigterm_timeout(&shared.seed.manifest).unwrap_or(Duration::from_secs(30)),
        wait_for_status(shared, Status::Stopped),
    )
    .await;
    shared.killer.notify_waiters();

    Ok(())
}

#[instrument(skip(shared))]
async fn start(shared: &ManagerSharedState) -> Result<(), Error> {
    shared.on_stop.send(OnStop::Restart).map_err(|_| {
        Error::new(
            eyre!("Manager has already been shutdown"),
            crate::ErrorKind::Docker,
        )
    })?;
    let _ = shared.status.0.send_modify(|x| {
        if *x != Status::Running {
            *x = Status::Starting
        }
    });
    Ok(())
}

#[instrument(skip(shared))]
async fn pause(shared: &ManagerSharedState) -> Result<(), Error> {
    if let Err(e) = shared
        .seed
        .ctx
        .docker
        .pause_container(&shared.seed.container_name)
        .await
    {
        tracing::error!("failed to pause container. stopping instead. {}", e);
        tracing::debug!("{:?}", e);
        return stop(shared).await;
    }
    let _ = shared.status.0.send(Status::Paused);
    Ok(())
}

#[instrument(skip(shared))]
async fn resume(shared: &ManagerSharedState) -> Result<(), Error> {
    shared
        .seed
        .ctx
        .docker
        .unpause_container(&shared.seed.container_name)
        .await?;
    let _ = shared.status.0.send(Status::Running);
    Ok(())
}
