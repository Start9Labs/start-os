use std::collections::BTreeMap;
use std::convert::TryInto;
use std::future::Future;
use std::net::Ipv4Addr;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::task::Poll;
use std::time::Duration;

use bollard::container::{KillContainerOptions, StopContainerOptions};
use chrono::Utc;
use color_eyre::eyre::eyre;
use embassy_container_init::{InputJsonRpc, RpcId};
use models::{ExecCommand, TermCommand};
use nix::sys::signal::Signal;
use num_enum::TryFromPrimitive;
use patch_db::DbHandle;
use sqlx::{Executor, Postgres};
use tokio::sync::watch::error::RecvError;
use tokio::sync::watch::{channel, Receiver, Sender};
use tokio::sync::{Mutex, Notify, RwLock};
use tokio::{sync::mpsc::UnboundedSender, task::JoinHandle};
use tokio_stream::wrappers::UnboundedReceiverStream;
use torut::onion::TorSecretKeyV3;
use tracing::instrument;

use crate::context::RpcContext;
use crate::manager::sync::synchronizer;
use crate::net::interface::InterfaceId;
use crate::net::GeneratedCertificateMountPoint;
use crate::notifications::NotificationLevel;
use crate::procedure::docker::{DockerContainer, DockerProcedure, LongRunning};
#[cfg(feature = "js_engine")]
use crate::procedure::js_scripts::JsProcedure;
use crate::procedure::{NoOutput, PackageProcedure, ProcedureName};
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::status::MainStatus;
use crate::util::{Container, NonDetachingJoinHandle, Version};
use crate::Error;

pub mod health;
mod sync;

pub const HEALTH_CHECK_COOLDOWN_SECONDS: u64 = 60;

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
    persistant_container: Arc<PersistantContainer>,
}

#[derive(TryFromPrimitive)]
#[repr(usize)]
pub enum Status {
    Starting = 0,
    Running = 1,
    Stopped = 2,
    Paused = 3,
    Shutdown = 4,
}

pub struct ManagerSharedState {
    ctx: RpcContext,
    status: AtomicUsize,
    on_stop: Sender<OnStop>,
    manifest: Manifest,
    container_name: String,
    tor_keys: BTreeMap<InterfaceId, TorSecretKeyV3>,
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

#[instrument(skip(state, persistant))]
async fn run_main(
    state: &Arc<ManagerSharedState>,
    persistant: Arc<PersistantContainer>,
) -> Result<Result<NoOutput, (i32, String)>, Error> {
    let rt_state = state.clone();
    let interfaces = states_main_interfaces(state)?;
    let generated_certificate = generate_certificate(state, &interfaces).await?;

    persistant.wait_for_persistant().await;
    let is_injectable_main = check_is_injectable_main(state);
    let mut runtime = match injectable_main(state) {
        InjectableMain::None => {
            tokio::spawn(async move { start_up_image(rt_state, generated_certificate).await })
        }
        #[cfg(feature = "js_engine")]
        InjectableMain::Script(_) => {
            tokio::spawn(async move { start_up_image(rt_state, generated_certificate).await })
        }
    };
    let ip = match is_injectable_main {
        false => Some(match get_running_ip(state, &mut runtime).await {
            GetRunninIp::Ip(x) => x,
            GetRunninIp::Error(e) => return Err(e),
            GetRunninIp::EarlyExit(x) => return Ok(x),
        }),
        true => None,
    };

    if let Some(ip) = ip {
        add_network_for_main(state, ip, interfaces, generated_certificate).await?;
    }

    set_commit_health_true(state);
    let health = main_health_check_daemon(state.clone());
    fetch_starting_to_running(state);
    let res = tokio::select! {
        a = runtime => a.map_err(|_| Error::new(eyre!("Manager runtime panicked!"), crate::ErrorKind::Docker)).and_then(|a| a),
        _ = health => Err(Error::new(eyre!("Health check daemon exited!"), crate::ErrorKind::Unknown)),

    };
    if let Some(ip) = ip {
        remove_network_for_main(state, ip).await?;
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
        .manifest
        .main
        .execute::<(), NoOutput>(
            &rt_state.ctx,
            &rt_state.manifest.id,
            &rt_state.manifest.version,
            ProcedureName::Main,
            &rt_state.manifest.volumes,
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
        let shared = Arc::new(ManagerSharedState {
            ctx,
            status: AtomicUsize::new(Status::Stopped as usize),
            on_stop,
            container_name: DockerProcedure::container_name(&manifest.id, None),
            manifest,
            tor_keys,
            synchronized: Notify::new(),
            synchronize_now: Notify::new(),
            commit_health_check_results: AtomicBool::new(true),
        });
        shared.synchronize_now.notify_one();
        let thread_shared = shared.clone();
        let persistant_container = PersistantContainer::new(&thread_shared);
        let managers_persistant = persistant_container.clone();
        let thread = tokio::spawn(async move {
            tokio::select! {
                _ = manager_thread_loop(recv, &thread_shared, managers_persistant.clone()) => (),
                _ = synchronizer(&*thread_shared, managers_persistant) => (),
            }
        });
        Ok(Manager {
            shared,
            persistant_container,
            thread: Container::new(Some(thread.into())),
        })
    }

    pub async fn signal(&self, signal: &Signal) -> Result<(), Error> {
        // stop health checks from committing their results
        self.shared
            .commit_health_check_results
            .store(false, Ordering::SeqCst);

        // send signal to container
        self.shared
            .ctx
            .docker
            .kill_container(
                &self.shared.container_name,
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
        let sigterm_timeout: Option<crate::util::serde::Duration> = match self
            .shared
            .manifest
            .containers
            .as_ref()
            .map(|x| x.main.sigterm_timeout)
        {
            Some(a) => a,
            None => match &self.shared.manifest.main {
                PackageProcedure::Docker(DockerProcedure {
                    sigterm_timeout, ..
                }) => *sigterm_timeout,
                #[cfg(feature = "js_engine")]
                PackageProcedure::Script(_) => return Ok(()),
            },
        };
        self.persistant_container.stop().await;

        if !check_is_injectable_main(&self.shared) {
            match self
                .shared
                .ctx
                .docker
                .stop_container(
                    &self.shared.container_name,
                    Some(StopContainerOptions {
                        t: sigterm_timeout
                            .map(|a| *a)
                            .unwrap_or(Duration::from_secs(30))
                            .as_secs_f64() as i64,
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
        } else {
            stop_long_running_processes(
                &*self.shared.container_name,
                self.persistant_container.command_inserter.clone(),
            )
            .await;
        }

        self.shared.status.store(
            Status::Shutdown as usize,
            std::sync::atomic::Ordering::SeqCst,
        );
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

    pub fn exec_command(&self) -> ExecCommand {
        self.persistant_container.exec_command()
    }
    pub fn term_command(&self) -> TermCommand {
        self.persistant_container.term_command()
    }
}

async fn manager_thread_loop(
    mut recv: Receiver<OnStop>,
    thread_shared: &Arc<ManagerSharedState>,
    persistant_container: Arc<PersistantContainer>,
) {
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
                    thread_shared.status.store(
                        Status::Stopped as usize,
                        std::sync::atomic::Ordering::SeqCst,
                    );
                    fut.await.unwrap();
                    continue;
                }
            }
            OnStop::Exit => {
                thread_shared.status.store(
                    Status::Stopped as usize,
                    std::sync::atomic::Ordering::SeqCst,
                );
                break;
            }
            OnStop::Restart => {
                thread_shared.status.store(
                    Status::Running as usize,
                    std::sync::atomic::Ordering::SeqCst,
                );
            }
        }
        match run_main(thread_shared, persistant_container.clone()).await {
            Ok(Ok(NoOutput)) => (), // restart
            Ok(Err(e)) => {
                let mut db = thread_shared.ctx.db.handle();
                let started = crate::db::DatabaseModel::new()
                    .package_data()
                    .idx_model(&thread_shared.manifest.id)
                    .and_then(|pde| pde.installed())
                    .map::<_, MainStatus>(|i| i.status().main())
                    .get(&mut db, false)
                    .await;
                match started.as_deref() {
                    Ok(Some(MainStatus::Running { started, .. }))
                        if cfg!(feature = "unstable")
                            || (Utc::now().signed_duration_since(*started)
                                > chrono::Duration::from_std(Duration::from_secs(60)).unwrap()
                                && !matches!(&*thread_shared.on_stop.borrow(), &OnStop::Exit)) =>
                    {
                        let res = thread_shared.ctx.notification_manager
                    .notify(
                        &mut db,
                        Some(thread_shared.manifest.id.clone()),
                        NotificationLevel::Warning,
                        String::from("Service Crashed"),
                        format!("The service {} has crashed with the following exit code: {}\nDetails: {}", thread_shared.manifest.id.clone(), e.0, e.1),
                        (),
                        Some(3600) // 1 hour
                    )
                    .await;
                        if let Err(e) = res {
                            tracing::error!("Failed to issue notification: {}", e);
                            tracing::debug!("{:?}", e);
                        }
                    }
                    _ => tracing::error!("service just started. not issuing crash notification"),
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
            let _ignored_result = input.send(JsonRpc::new(RpcId::UInt(i as u32), Input::Term()));
        }
    }
}
impl CommandInserter {
    fn new(
        long_running: LongRunning,
        input: UnboundedSender<InputJsonRpc>,
    ) -> (Self, LongRunningHandle) {
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
            Self {
                command_counter,
                input,
                outputs,
            },
            handle,
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
        outputs.insert(command_id.clone(), sender);
        if let Some(timeout) = timeout {
            tokio::spawn({
                let input = self.input.clone();
                let command_id = command_id.clone();
                async move {
                    tokio::time::sleep(timeout).await;
                    let _ignored_output = input.send(JsonRpc::new(command_id, Input::Kill()));
                }
            });
        }
        if let Err(err) = self.input.send(JsonRpc::new(
            command_id.clone(),
            Input::Command { command, args },
        )) {
            tracing::warn!("Trying to send to input but can't");
            tracing::debug!("{err:?}");
            return None;
        }

        Some(command_id)
    }
    pub async fn term(&self, id: RpcId) {
        use embassy_container_init::{Input, JsonRpc};
        self.outputs.lock().await.remove(&id);
        let _ignored_term = self.input.send(JsonRpc::new(id, Input::Term()));
    }

    pub async fn term_all(&self) {
        for i in 0..self.command_counter.load(Ordering::Relaxed) {
            self.term(RpcId::UInt(i as u32)).await;
        }
    }
}

type RunningDocker =
    Arc<Mutex<Option<NonDetachingJoinHandle<Result<Result<NoOutput, (i32, String)>, Error>>>>>;
pub struct PersistantContainer {
    container_name: String,
    running_docker: RunningDocker,
    should_stop_running: Arc<std::sync::atomic::AtomicBool>,
    wait_for_start: (Sender<bool>, Receiver<bool>),
    command_inserter: Arc<Mutex<Option<CommandInserter>>>,
}

impl PersistantContainer {
    #[instrument(skip(thread_shared))]
    fn new(thread_shared: &Arc<ManagerSharedState>) -> Arc<Self> {
        let wait_for_start = channel(false);
        let container = Arc::new(Self {
            container_name: thread_shared.container_name.clone(),
            running_docker: Arc::new(Mutex::new(None)),
            should_stop_running: Arc::new(AtomicBool::new(false)),
            wait_for_start,
            command_inserter: Default::default(),
        });
        tokio::spawn(persistant_container(
            thread_shared.clone(),
            container.clone(),
        ));
        container
    }
    #[instrument(skip(self))]
    async fn stop(&self) {
        let container_name = &self.container_name;
        self.should_stop_running.store(true, Ordering::SeqCst);
        let mut running_docker = self.running_docker.lock().await;
        *running_docker = None;
        use tokio::process::Command;
        if let Err(_err) = Command::new("docker")
            .args(["stop", "-t", "30", container_name])
            .output()
            .await
        {}
    }

    async fn wait_for_persistant(&self) {
        let mut changed_rx = self.wait_for_start.1.clone();
        loop {
            if !*changed_rx.borrow() {
                return;
            }
            changed_rx.changed().await.unwrap();
        }
    }

    async fn start_wait(&self) {
        self.wait_for_start.0.send(true).unwrap();
    }
    async fn done_waiting(&self) {
        self.wait_for_start.0.send(false).unwrap();
    }
    fn term_command(&self) -> TermCommand {
        let cloned = self.command_inserter.clone();
        Arc::new(move |id| {
            let cloned = cloned.clone();
            Box::pin(async move {
                let lock = cloned.lock().await;
                let _id = match &*lock {
                    Some(command_inserter) => command_inserter.term(id).await,
                    None => {
                        return Err("Couldn't get a command inserter in current service".to_string())
                    }
                };
                Ok::<(), String>(())
            })
        })
    }

    fn exec_command(&self) -> ExecCommand {
        let cloned = self.command_inserter.clone();

        /// A handle that on drop will clean all the ids that are inserter in the fn.
        struct Cleaner {
            command_inserter: Arc<Mutex<Option<CommandInserter>>>,
            ids: ::std::collections::BTreeSet<RpcId>,
        }
        impl Drop for Cleaner {
            fn drop(&mut self) {
                let command_inserter = self.command_inserter.clone();
                let ids = ::std::mem::take(&mut self.ids);
                tokio::spawn(async move {
                    let command_inserter_lock = command_inserter.lock().await;
                    let command_inserter = match &*command_inserter_lock {
                        Some(a) => a,
                        None => {
                            return;
                        }
                    };
                    for id in ids {
                        command_inserter.term(id).await;
                    }
                });
            }
        }
        let cleaner = Arc::new(Mutex::new(Cleaner {
            command_inserter: cloned.clone(),
            ids: Default::default(),
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
                            cleaner.ids.insert(id.clone());
                            id
                        } else {
                            return Err("Couldn't get command started ".to_string());
                        }
                    }
                    None => {
                        return Err("Couldn't get a command inserter in current service".to_string())
                    }
                };
                Ok::<RpcId, String>(id)
            })
        })
    }
}
impl Drop for PersistantContainer {
    fn drop(&mut self) {
        self.should_stop_running.store(true, Ordering::SeqCst);
    }
}

async fn persistant_container(
    thread_shared: Arc<ManagerSharedState>,
    container: Arc<PersistantContainer>,
) {
    let main_docker_procedure_for_long = injectable_main(&thread_shared);
    match main_docker_procedure_for_long {
        InjectableMain::None => futures::future::pending().await,
        #[cfg(feature = "js_engine")]
        InjectableMain::Script((container_inject, procedure)) => loop {
            let main = DockerProcedure::main_docker_procedure_js(container_inject, procedure);
            if container.should_stop_running.load(Ordering::SeqCst) {
                return;
            }
            container.start_wait().await;
            match run_persistant_container(&thread_shared, container.clone(), main).await {
                Ok(_) => (),
                Err(e) => {
                    tracing::error!("failed to start persistant container: {}", e);
                    tracing::debug!("{:?}", e);
                }
            }
        },
    }
}

#[cfg(not(feature = "js_engine"))]
enum InjectableMain {
    None,
}

#[cfg(feature = "js_engine")]
enum InjectableMain<'a> {
    None,
    Script((&'a DockerContainer, &'a JsProcedure)),
}

fn injectable_main(thread_shared: &Arc<ManagerSharedState>) -> InjectableMain {
    match (
        &thread_shared.manifest.main,
        &thread_shared.manifest.containers.as_ref().map(|x| &x.main),
    ) {
        #[cfg(feature = "js_engine")]
        (PackageProcedure::Script(inject), Some(container)) => {
            InjectableMain::Script((container, inject))
        }
        _ => InjectableMain::None,
    }
}
fn check_is_injectable_main(thread_shared: &ManagerSharedState) -> bool {
    match &thread_shared.manifest.main {
        PackageProcedure::Docker(_a) => false,
        #[cfg(feature = "js_engine")]
        PackageProcedure::Script(_) => true,
    }
}
async fn run_persistant_container(
    state: &Arc<ManagerSharedState>,
    persistant: Arc<PersistantContainer>,
    docker_procedure: DockerProcedure,
) -> Result<(), Error> {
    let interfaces = states_main_interfaces(state)?;
    let generated_certificate = generate_certificate(state, &interfaces).await?;
    let mut runtime =
        long_running_docker(state.clone(), docker_procedure, persistant.clone()).await?;

    let ip = match get_long_running_ip(state, &mut runtime).await {
        GetRunninIp::Ip(x) => x,
        GetRunninIp::Error(e) => return Err(e),
        GetRunninIp::EarlyExit(e) => {
            tracing::error!("Early Exit");
            tracing::debug!("{:?}", e);
            return Ok(());
        }
    };
    persistant.done_waiting().await;
    add_network_for_main(state, ip, interfaces, generated_certificate).await?;

    fetch_starting_to_running(state);
    let res = tokio::select! {
        a = runtime.0 => a.map_err(|_| Error::new(eyre!("Manager runtime panicked!"), crate::ErrorKind::Docker)).map(|_| ()),
    };
    remove_network_for_main(state, ip).await?;
    res
}

async fn long_running_docker(
    rt_state: Arc<ManagerSharedState>,
    main_status: DockerProcedure,
    container: Arc<PersistantContainer>,
) -> Result<LongRunningHandle, Error> {
    let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();
    let long_running = main_status
        .long_running_execute(
            &rt_state.ctx,
            &rt_state.manifest.id,
            &rt_state.manifest.version,
            ProcedureName::LongRunning,
            &rt_state.manifest.volumes,
            UnboundedReceiverStream::new(receiver),
        )
        .await?;
    let (command_inserter, long_running_handle) = CommandInserter::new(long_running, sender);
    *container.command_inserter.lock().await = Some(command_inserter);
    Ok(long_running_handle)
}

async fn remove_network_for_main(
    state: &Arc<ManagerSharedState>,
    ip: std::net::Ipv4Addr,
) -> Result<(), Error> {
    state
        .ctx
        .net_controller
        .remove(
            &state.manifest.id,
            ip,
            state.manifest.interfaces.0.keys().cloned(),
        )
        .await?;
    Ok(())
}

fn fetch_starting_to_running(state: &Arc<ManagerSharedState>) {
    let _ = state
        .status
        .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |x| {
            if x == Status::Starting as usize {
                Some(Status::Running as usize)
            } else {
                None
            }
        });
}

async fn main_health_check_daemon(state: Arc<ManagerSharedState>) {
    tokio::time::sleep(Duration::from_secs(10)).await;
    loop {
        let mut db = state.ctx.db.handle();
        if let Err(e) = health::check(
            &state.ctx,
            &mut db,
            &state.manifest.id,
            &state.commit_health_check_results,
        )
        .await
        {
            tracing::error!(
                "Failed to run health check for {}: {}",
                &state.manifest.id,
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
    state: &Arc<ManagerSharedState>,
    ip: std::net::Ipv4Addr,
    interfaces: Vec<(
        InterfaceId,
        &crate::net::interface::Interface,
        TorSecretKeyV3,
    )>,
    generated_certificate: GeneratedCertificateMountPoint,
) -> Result<(), Error> {
    state
        .ctx
        .net_controller
        .add(&state.manifest.id, ip, interfaces, generated_certificate)
        .await?;
    Ok(())
}

enum GetRunninIp {
    Ip(Ipv4Addr),
    Error(Error),
    EarlyExit(Result<NoOutput, (i32, String)>),
}

type RuntimeOfCommand = JoinHandle<Result<Result<NoOutput, (i32, String)>, Error>>;

async fn get_running_ip(
    state: &Arc<ManagerSharedState>,
    mut runtime: &mut RuntimeOfCommand,
) -> GetRunninIp {
    loop {
        match container_inspect(state).await {
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
                    Ok(Some(ip_addr)) => return GetRunninIp::Ip(ip_addr),
                    Ok(None) => (),
                    Err(e) => return GetRunninIp::Error(e.into()),
                }
            }
            Err(bollard::errors::Error::DockerResponseServerError {
                status_code: 404, // NOT FOUND
                ..
            }) => (),
            Err(e) => return GetRunninIp::Error(e.into()),
        }
        if let Poll::Ready(res) = futures::poll!(&mut runtime) {
            match res {
                Ok(Ok(response)) => return GetRunninIp::EarlyExit(response),
                Err(_) | Ok(Err(_)) => {
                    return GetRunninIp::Error(Error::new(
                        eyre!("Manager runtime panicked!"),
                        crate::ErrorKind::Docker,
                    ))
                }
            }
        }
    }
}

async fn get_long_running_ip(
    state: &Arc<ManagerSharedState>,
    runtime: &mut LongRunningHandle,
) -> GetRunninIp {
    loop {
        match container_inspect(state).await {
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
                    Ok(Some(ip_addr)) => return GetRunninIp::Ip(ip_addr),
                    Ok(None) => (),
                    Err(e) => return GetRunninIp::Error(e.into()),
                }
            }
            Err(bollard::errors::Error::DockerResponseServerError {
                status_code: 404, // NOT FOUND
                ..
            }) => (),
            Err(e) => return GetRunninIp::Error(e.into()),
        }
        if let Poll::Ready(res) = futures::poll!(&mut runtime.0) {
            match res {
                Ok(_) => return GetRunninIp::EarlyExit(Ok(NoOutput)),
                Err(_e) => {
                    return GetRunninIp::Error(Error::new(
                        eyre!("Manager runtime panicked!"),
                        crate::ErrorKind::Docker,
                    ))
                }
            }
        }
    }
}

async fn container_inspect(
    state: &Arc<ManagerSharedState>,
) -> Result<bollard::models::ContainerInspectResponse, bollard::errors::Error> {
    state
        .ctx
        .docker
        .inspect_container(&state.container_name, None)
        .await
}

async fn generate_certificate(
    state: &Arc<ManagerSharedState>,
    interfaces: &Vec<(
        InterfaceId,
        &crate::net::interface::Interface,
        TorSecretKeyV3,
    )>,
) -> Result<GeneratedCertificateMountPoint, Error> {
    state
        .ctx
        .net_controller
        .generate_certificate_mountpoint(&state.manifest.id, interfaces)
        .await
}

fn states_main_interfaces(
    state: &Arc<ManagerSharedState>,
) -> Result<
    Vec<(
        InterfaceId,
        &crate::net::interface::Interface,
        TorSecretKeyV3,
    )>,
    Error,
> {
    state
        .manifest
        .interfaces
        .0
        .iter()
        .map(|(id, info)| {
            Ok((
                id.clone(),
                info,
                state
                    .tor_keys
                    .get(id)
                    .ok_or_else(|| {
                        Error::new(eyre!("interface {} missing key", id), crate::ErrorKind::Tor)
                    })?
                    .clone(),
            ))
        })
        .collect::<Result<Vec<_>, Error>>()
}

#[instrument(skip(shared, persistant_container))]
async fn stop(
    shared: &ManagerSharedState,
    persistant_container: Arc<PersistantContainer>,
) -> Result<(), Error> {
    shared
        .commit_health_check_results
        .store(false, Ordering::SeqCst);
    shared.on_stop.send(OnStop::Sleep).map_err(|_| {
        Error::new(
            eyre!("Manager has already been shutdown"),
            crate::ErrorKind::Docker,
        )
    })?;
    if matches!(
        shared.status.load(Ordering::SeqCst).try_into().unwrap(),
        Status::Paused
    ) {
        resume(shared).await?;
    }
    match &shared.manifest.main {
        PackageProcedure::Docker(DockerProcedure {
            sigterm_timeout, ..
        }) => {
            if !check_is_injectable_main(shared) {
                match shared
                    .ctx
                    .docker
                    .stop_container(
                        &shared.container_name,
                        Some(StopContainerOptions {
                            t: sigterm_timeout
                                .map(|a| *a)
                                .unwrap_or(Duration::from_secs(30))
                                .as_secs_f64() as i64,
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
            } else {
                stop_long_running_processes(
                    &shared.container_name,
                    persistant_container.command_inserter.clone(),
                )
                .await;
            }
        }
        #[cfg(feature = "js_engine")]
        PackageProcedure::Script(_) => {
            if check_is_injectable_main(shared) {
                stop_long_running_processes(
                    &shared.container_name,
                    persistant_container.command_inserter.clone(),
                )
                .await;
            }
        }
    };
    tracing::debug!("Stopping a docker");
    shared.status.store(
        Status::Stopped as usize,
        std::sync::atomic::Ordering::SeqCst,
    );
    Ok(())
}

/// So the sleep infinity, which is the long running, is pid 1. So we kill the others
async fn stop_long_running_processes(
    container_name: &str,
    command_inserter: Arc<Mutex<Option<CommandInserter>>>,
) {
    if let Some(command_inserter) = &*command_inserter.lock().await {
        command_inserter.term_all().await;
    }

    let _ = tokio::process::Command::new("docker")
        .args([
            "container",
            "exec",
            container_name,
            "sh",
            "-c",
            "ps ax | awk '$1 ~ /^[:0-9:]/ && $1 > 1 {print $1}' | xargs kill",
        ])
        .output()
        .await;
}

#[instrument(skip(shared))]
async fn start(shared: &ManagerSharedState) -> Result<(), Error> {
    shared.on_stop.send(OnStop::Restart).map_err(|_| {
        Error::new(
            eyre!("Manager has already been shutdown"),
            crate::ErrorKind::Docker,
        )
    })?;
    let _ = shared
        .status
        .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |x| {
            if x != Status::Running as usize {
                Some(Status::Starting as usize)
            } else {
                None
            }
        });
    Ok(())
}

#[instrument(skip(shared, persistant_container))]
async fn pause(
    shared: &ManagerSharedState,
    persistant_container: Arc<PersistantContainer>,
) -> Result<(), Error> {
    if let Err(e) = shared
        .ctx
        .docker
        .pause_container(&shared.container_name)
        .await
    {
        tracing::error!("failed to pause container. stopping instead. {}", e);
        tracing::debug!("{:?}", e);
        return stop(shared, persistant_container).await;
    }
    shared
        .status
        .store(Status::Paused as usize, std::sync::atomic::Ordering::SeqCst);
    Ok(())
}

#[instrument(skip(shared))]
async fn resume(shared: &ManagerSharedState) -> Result<(), Error> {
    shared
        .ctx
        .docker
        .unpause_container(&shared.container_name)
        .await?;
    shared.status.store(
        Status::Running as usize,
        std::sync::atomic::Ordering::SeqCst,
    );
    Ok(())
}
