use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};

use tokio::{
    spawn,
    sync::{mpsc, Mutex},
};
use tracing::{debug, error};

use crate::{
    backup::{backup_bulk::backup_all_task, restore},
    config::set_impl_task,
    control,
};
use crate::{config::set_dry_task, install};

/// Use these shapes to send into the task_runner.
pub mod task_shapes {
    use std::time::Duration;

    use reqwest::Url;
    use tokio::sync::oneshot;

    use crate::context::RpcContext;
    use crate::{
        backup::target::BackupTargetId,
        core::rpc_continuations::RequestGuid,
        dependencies::BreakageRes,
        install,
        s9pk::manifest::{Manifest, PackageId},
    };
    use crate::{db::util::WithRevision, Error};

    use super::Task;

    pub struct BackupAll {
        pub(crate) ctx: RpcContext,
        pub(crate) target_id: BackupTargetId,
        pub(crate) old_password: Option<String>,
        pub(crate) password: String,
        pub(crate) done: oneshot::Sender<Result<WithRevision<()>, Error>>,
    }
    impl From<BackupAll> for Task {
        fn from(val: BackupAll) -> Self {
            Task::BackupAll(val)
        }
    }
    pub struct CommandStart {
        pub(crate) ctx: RpcContext,
        pub(crate) id: PackageId,
        pub(crate) done: oneshot::Sender<Result<WithRevision<()>, Error>>,
    }
    impl From<CommandStart> for Task {
        fn from(val: CommandStart) -> Self {
            Task::CommandStart(val)
        }
    }

    pub struct CommandStopDry {
        pub(crate) ctx: RpcContext,
        pub(crate) id: PackageId,
        pub(crate) done: oneshot::Sender<Result<BreakageRes, Error>>,
    }
    impl From<CommandStopDry> for Task {
        fn from(val: CommandStopDry) -> Self {
            Task::CommandStopDry(val)
        }
    }
    pub struct CommandStopImpl {
        pub(crate) ctx: RpcContext,
        pub(crate) id: PackageId,
        pub(crate) done: oneshot::Sender<Result<WithRevision<()>, Error>>,
    }
    impl From<CommandStopImpl> for Task {
        fn from(val: CommandStopImpl) -> Self {
            Task::CommandStopImpl(val)
        }
    }

    pub struct ConfigureSet {
        pub(crate) ctx: RpcContext,
        pub(crate) package_id: PackageId,
        pub(crate) config: Option<crate::Config>,
        pub(crate) timeout: Option<Duration>,
        pub(crate) expire_id: Option<String>,
        pub(crate) done: oneshot::Sender<Result<BreakageRes, Error>>,
    }
    impl From<ConfigureSet> for Task {
        fn from(val: ConfigureSet) -> Self {
            Task::ConfigureSet(val)
        }
    }

    pub struct ConfigureImpl {
        pub(crate) ctx: RpcContext,
        pub(crate) package_id: PackageId,
        pub(crate) config: Option<crate::Config>,
        pub(crate) timeout: Option<Duration>,
        pub(crate) expire_id: Option<String>,
        pub(crate) done: oneshot::Sender<Result<WithRevision<()>, Error>>,
    }
    impl From<ConfigureImpl> for Task {
        fn from(val: ConfigureImpl) -> Self {
            Task::ConfigureImpl(val)
        }
    }

    pub struct UninstallDry {
        pub(crate) ctx: RpcContext,
        pub(crate) id: PackageId,
        pub(crate) done: oneshot::Sender<Result<BreakageRes, Error>>,
    }
    impl From<UninstallDry> for Task {
        fn from(val: UninstallDry) -> Self {
            Task::UninstallDry(val)
        }
    }

    pub struct UninstallImpl {
        pub(crate) ctx: RpcContext,
        pub(crate) id: PackageId,
        pub(crate) done: oneshot::Sender<Result<WithRevision<()>, Error>>,
    }
    impl From<UninstallImpl> for Task {
        fn from(val: UninstallImpl) -> Self {
            Task::UninstallImpl(val)
        }
    }

    pub struct Install {
        pub(crate) ctx: RpcContext,
        pub(crate) id: String,
        pub(crate) marketplace_url: Option<Url>,
        pub(crate) version_spec: Option<String>,
        pub(crate) version_priority: Option<install::MinMax>,
        pub(crate) done: oneshot::Sender<Result<WithRevision<()>, Error>>,
    }
    impl From<Install> for Task {
        fn from(val: Install) -> Self {
            Task::Install(val)
        }
    }

    pub struct Sideload {
        pub(crate) ctx: RpcContext,
        pub(crate) manifest: Manifest,
        pub(crate) done: oneshot::Sender<Result<RequestGuid, Error>>,
    }
    impl From<Sideload> for Task {
        fn from(val: Sideload) -> Self {
            Task::Sideload(Box::new(val))
        }
    }

    pub struct Restore {
        pub(crate) ctx: RpcContext,
        pub(crate) ids: Vec<PackageId>,
        pub(crate) target_id: BackupTargetId,
        pub(crate) old_password: Option<String>,
        pub(crate) password: String,
        pub(crate) done: oneshot::Sender<Result<WithRevision<()>, Error>>,
    }
    impl From<Restore> for Task {
        fn from(val: Restore) -> Self {
            Task::Restore(val)
        }
    }
}
use task_shapes::*;

/// These are what are loaded into the task_runner, and to create one one should use the
/// task_shapes module and use the From trait to convert into a Task.
pub enum Task {
    BackupAll(BackupAll),
    CommandStart(CommandStart),
    CommandStopDry(CommandStopDry),
    CommandStopImpl(CommandStopImpl),
    ConfigureImpl(ConfigureImpl),
    ConfigureSet(ConfigureSet),
    Install(Install),
    Restore(Restore),
    /// https://rust-lang.github.io/rust-clippy/master/index.html#large_enum_variant
    Sideload(Box<Sideload>),
    UninstallDry(UninstallDry),
    UninstallImpl(UninstallImpl),
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
enum TaskType {
    BackupAll,
    Restore,
    ConfigureSet,
    ConfigureImpl,
    CommandStart,
    CommandStopDry,
    CommandStopImpl,
    UninstallDry,
    UninstallImpl,
    Sideload,
    Install,
}

impl From<&Task> for TaskType {
    fn from(task: &Task) -> Self {
        match task {
            Task::BackupAll(_) => TaskType::BackupAll,
            Task::CommandStart(_) => TaskType::CommandStart,
            Task::CommandStopDry(_) => TaskType::CommandStopDry,
            Task::CommandStopImpl(_) => TaskType::CommandStopImpl,
            Task::ConfigureImpl(_) => TaskType::ConfigureImpl,
            Task::ConfigureSet(_) => TaskType::ConfigureSet,
            Task::Install(_) => TaskType::Install,
            Task::Restore(_) => TaskType::Restore,
            Task::Sideload(_) => TaskType::Sideload,
            Task::UninstallDry(_) => TaskType::UninstallDry,
            Task::UninstallImpl(_) => TaskType::UninstallImpl,
        }
    }
}

impl std::fmt::Debug for Task {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BackupAll(_) => f.write_str("BackupAll"),
            Self::CommandStart(_) => f.write_str("CommandStart"),
            Self::CommandStopDry(_) => f.write_str("CommandStopDry"),
            Self::CommandStopImpl(_) => f.write_str("CommandStopImpl"),
            Self::ConfigureImpl(_) => f.write_str("ConfigureImpl"),
            Self::ConfigureSet(_) => f.write_str("ConfigureSet"),
            Self::Install(_) => f.write_str("Install"),
            Self::Restore(_) => f.write_str("Restore"),
            Self::Sideload(_) => f.write_str("Sideload"),
            Self::UninstallDry(_) => f.write_str("UninstallDry"),
            Self::UninstallImpl(_) => f.write_str("UninstallImpl"),
        }
    }
}

impl Task {
    async fn run(self) {
        match self {
            Task::BackupAll(BackupAll {
                ctx,
                done,
                old_password,
                password,
                target_id,
            }) => {
                if let Err(err) =
                    done.send(backup_all_task(ctx, target_id, old_password, password).await)
                {
                    error!("Task could not be notified done");
                    debug!("{:?}", err);
                }
            }

            Task::ConfigureImpl(ConfigureImpl {
                ctx,
                package_id,
                config,
                timeout,
                expire_id,
                done,
            }) => {
                if let Err(err) =
                    done.send(set_impl_task(ctx, (package_id, config, timeout, expire_id)).await)
                {
                    error!("Task could not be notified done");
                    debug!("{:?}", err);
                }
            }

            Task::ConfigureSet(ConfigureSet {
                ctx,
                package_id,
                config,
                timeout,
                expire_id,
                done,
            }) => {
                if let Err(err) =
                    done.send(set_dry_task(ctx, (package_id, config, timeout, expire_id)).await)
                {
                    error!("Task could not be notified done");
                    debug!("{:?}", err);
                }
            }
            Task::CommandStart(CommandStart { ctx, id, done }) => {
                if let Err(err) = done.send(control::start_task(ctx, id).await) {
                    error!("Task could not be notified done");
                    debug!("{:?}", err);
                }
            }
            Task::CommandStopDry(CommandStopDry { ctx, id, done }) => {
                if let Err(err) = done.send(control::stop_dry_task(ctx, id).await) {
                    error!("Task could not be notified done");
                    debug!("{:?}", err);
                }
            }
            Task::CommandStopImpl(CommandStopImpl { ctx, id, done }) => {
                if let Err(err) = done.send(control::stop_impl_task(ctx, id).await) {
                    error!("Task could not be notified done");
                    debug!("{:?}", err);
                }
            }
            Task::UninstallImpl(UninstallImpl { ctx, id, done }) => {
                if let Err(err) = done.send(install::uninstall_impl_task(ctx, id).await) {
                    error!("Task could not be notified done");
                    debug!("{:?}", err);
                }
            }
            Task::UninstallDry(UninstallDry { ctx, id, done }) => {
                if let Err(err) = done.send(install::uninstall_dry_task(ctx, id).await) {
                    error!("Task could not be notified done");
                    debug!("{:?}", err);
                }
            }
            Task::Install(Install {
                ctx,
                done,
                id,
                marketplace_url,
                version_priority,
                version_spec,
            }) => {
                if let Err(err) = done.send(
                    install::install_task(ctx, id, marketplace_url, version_spec, version_priority)
                        .await,
                ) {
                    error!("Task could not be notified done");
                    debug!("{:?}", err);
                }
            }
            Task::Sideload(sideload) => {
                let Sideload {
                    ctx,
                    done,
                    manifest,
                } = *sideload;
                if let Err(err) = done.send(install::sideload_task(ctx, manifest).await) {
                    error!("Task could not be notified done");
                    debug!("{:?}", err);
                }
            }
            Task::Restore(Restore {
                ctx,
                done,
                ids,
                old_password,
                password,
                target_id,
            }) => {
                if let Err(err) = done.send(
                    restore::restore_packages_rpc_task(ctx, ids, target_id, old_password, password)
                        .await,
                ) {
                    error!("Task could not be notified done");
                    debug!("{:?}", err);
                }
            }
        }
    }
}

/// Using the task runner ensures that the sections of code that needed the complicated locking
/// will be seperated, and the things that can be done in parallel will be.
#[derive(Clone)]
pub struct TaskRunner {
    queue: mpsc::UnboundedSender<Task>,
    alive: Arc<AtomicBool>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CurrentlyRunningTask {
    task_type: TaskType,
    package_id: Option<crate::s9pk::manifest::PackageId>,
}

#[derive(Default)]
struct CurrentlyRunningTasks(Vec<CurrentlyRunningTask>);

impl CurrentlyRunningTasks {
    fn try_push(&mut self, task: CurrentlyRunningTask) -> Option<CurrentlyRunningTask> {
        let task_type = task.task_type;
        for running_task in self.0.iter() {
            let safe_to_add = match running_task.task_type {
                TaskType::BackupAll
                | TaskType::ConfigureSet
                | TaskType::ConfigureImpl
                | TaskType::Restore => false,
                TaskType::CommandStart => {
                    matches!(task_type, TaskType::CommandStart)
                }
                TaskType::CommandStopImpl | TaskType::CommandStopDry => {
                    matches!(
                        task_type,
                        TaskType::CommandStopImpl | TaskType::CommandStopDry
                    )
                }
                TaskType::UninstallImpl | TaskType::UninstallDry => {
                    matches!(task_type, TaskType::UninstallImpl | TaskType::UninstallDry)
                }
                TaskType::Sideload | TaskType::Install => {
                    matches!(task_type, TaskType::Sideload | TaskType::Install)
                }
            };
            if !safe_to_add {
                return Some(task);
            }
            match (&task.package_id, &running_task.package_id) {
                (Some(package_id), Some(running_package_id))
                    if package_id == running_package_id =>
                {
                    return Some(task);
                }
                _ => {}
            }
        }
        self.0.push(task);
        None
    }
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn remove_from_running(&mut self, task_type_done: TaskType) {
        if let Some(index) = self
            .0
            .iter()
            .position(|task_type| task_type.task_type == task_type_done)
        {
            self.0.remove(index);
        }
    }
}

impl From<&Task> for CurrentlyRunningTask {
    fn from(task: &Task) -> Self {
        let task_type: TaskType = (task).into();
        CurrentlyRunningTask {
            task_type,
            package_id: match task {
                Task::BackupAll(_) => None,
                Task::CommandStart(CommandStart { id: package_id, .. })
                | Task::CommandStopDry(CommandStopDry { id: package_id, .. })
                | Task::CommandStopImpl(CommandStopImpl { id: package_id, .. })
                | Task::ConfigureImpl(ConfigureImpl { package_id, .. })
                | Task::ConfigureSet(ConfigureSet { package_id, .. })
                | Task::UninstallDry(UninstallDry { id: package_id, .. })
                | Task::UninstallImpl(UninstallImpl { id: package_id, .. }) => {
                    Some(package_id.clone())
                }
                Task::Sideload(side) => Some(side.manifest.id.clone()),
                Task::Install(Install { id, .. }) => id.parse().ok(),
                Task::Restore(_) => None,
            },
        }
    }
}

impl Default for TaskRunner {
    fn default() -> Self {
        let (queue, rx) = mpsc::unbounded_channel();
        let alive = Arc::new(AtomicBool::new(true));
        TaskRunner::start_running_tasks(rx, alive.clone());

        Self { alive, queue }
    }
}

impl TaskRunner {
    /// Add another task to be done. Depending on what is running already determines if this will start away
    /// or be queued.
    #[tracing::instrument(skip(self))]
    pub fn add_task(&self, task: Task) {
        if let Err(err) = self.queue.send(task) {
            error!("Task system can no longer add tasks");
            debug!("{:?}", err);
        }
    }

    /// Used during the rpc cleanup, made stop the task runner.
    pub async fn shutdown(self) {
        self.alive.store(false, Ordering::Relaxed);
    }

    fn start_running_tasks(mut rx: mpsc::UnboundedReceiver<Task>, spawned_alive: Arc<AtomicBool>) {
        tokio::task::spawn(async move {
            let mut todos: Vec<Task> = Vec::new();
            let running_tasks: Arc<Mutex<CurrentlyRunningTasks>> = Default::default();
            let (indicate_task_done, mut task_is_done) = mpsc::unbounded_channel::<TaskType>();

            loop {
                if !spawned_alive.load(Ordering::Relaxed) {
                    break;
                }

                tokio::select! {
                    Some(task) = rx.recv() => {
                        let mut running_tasks = running_tasks.lock().await;
                        if let Some(task) = try_run_task(&mut running_tasks, task, indicate_task_done.clone()) {
                            todos.push(task);
                        }
                    },
                    Some(task_type_done) = task_is_done.recv() => {
                        let mut running_tasks = running_tasks.lock().await;
                        running_tasks.remove_from_running(task_type_done);
                        if running_tasks.is_empty() {
                            run_new_tasks_from_todo(&mut todos, &mut running_tasks, indicate_task_done.clone());
                        }
                    }
                    else => {
                        break;
                    },
                }
            }
            rx.close();

            fn try_run_task(
                running_tasks: &mut CurrentlyRunningTasks,
                task: Task,
                task_is_done: mpsc::UnboundedSender<TaskType>,
            ) -> Option<Task> {
                let running_task = CurrentlyRunningTask::from(&task);
                if running_tasks.try_push(running_task.clone()).is_some() {
                    return Some(task);
                }

                spawn(async move {
                    task.run().await;
                    if let Err(err) = task_is_done.send(running_task.task_type) {
                        error!("Task could not be notified done");
                        debug!("{:?}", err);
                    }
                });
                None
            }

            fn run_new_tasks_from_todo(
                todos: &mut Vec<Task>,
                running_tasks: &mut CurrentlyRunningTasks,
                task_is_done: mpsc::UnboundedSender<TaskType>,
            ) {
                for task in std::mem::take(todos) {
                    if let Some(task) = try_run_task(running_tasks, task, task_is_done.clone()) {
                        todos.push(task)
                    }
                }
            }
        });
    }
}
