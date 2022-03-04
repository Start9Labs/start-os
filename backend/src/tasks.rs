use std::{
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::Duration,
};

use tokio::sync::{mpsc, oneshot};

use futures::{stream, StreamExt};
use tracing::{debug, error};

use crate::{
    backup::{backup_bulk::backup_all_task, target::BackupTargetId},
    config::set_impl_task,
    dependencies::BreakageRes,
    s9pk::manifest::PackageId,
};
use crate::{config::set_dry_task, context::RpcContext};
use crate::{db::util::WithRevision, Error};

// TODO In Progress

// TODO wait for closed done

// TODO Installing
// TODO Resotre
// TODO Uninstall
// TODO Stopping / Starting

// TODO Property?
// TODO Action?
// TODO Dependency?
// TODO Health Checks?

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

pub enum Task {
    BackupAll(BackupAll),
    ConfigureSet(ConfigureSet),
    ConfigureImpl(ConfigureImpl),
}

impl std::fmt::Debug for Task {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BackupAll(_) => f.write_str("BackupAll"),
            Self::ConfigureSet(_) => f.write_str("ConfigureSet"),
            Self::ConfigureImpl(_) => f.write_str("ConfigureImpl"),
        }
    }
}

impl Task {
    fn is_safe_concurrent(&self, _other: &Task) -> bool {
        match self {
            Task::BackupAll(_) => false,
            Task::ConfigureSet(_) => false,
            Task::ConfigureImpl(_) => false,
        }
    }
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
        }
    }
}

#[derive(Clone)]
pub struct TaskRunner {
    queue: mpsc::UnboundedSender<Task>,
    alive: Arc<AtomicBool>,
}

impl Default for TaskRunner {
    fn default() -> Self {
        let (queue, rx) = mpsc::unbounded_channel();
        let alive = Arc::new(AtomicBool::new(true));
        let spawned_alive = alive.clone();
        let spawned = Arc::new(tokio::task::spawn(async move {
            let mut todo: Vec<Task> = Vec::new();
            let mut rx = rx;
            loop {
                if !spawned_alive.load(Ordering::Relaxed) {
                    break;
                }
                let task = match rx.recv().await {
                    Some(a) => a,
                    None => break,
                };

                todo.push(task);
                while let Ok(task) = rx.try_recv() {
                    todo.push(task);
                }
                let mut todos = std::mem::take(&mut todo).into_iter();
                let head = match todos.next() {
                    Some(x) => x,
                    None => continue,
                };

                let (mut tasks_todo, next_todos): (Vec<_>, Vec<_>) =
                    todos.partition(|x| head.is_safe_concurrent(x));

                tasks_todo.push(head);

                stream::iter(tasks_todo)
                    .for_each_concurrent(10, |task| async move { task.run().await })
                    .await;

                todo = next_todos;
            }
            rx.close();
        }));

        Self { alive, queue }
    }
}

impl TaskRunner {
    pub fn add_task(&self, task: Task) {
        if let Err(err) = self.queue.send(task) {
            error!("Task system can no longer add tasks");
            debug!("{:?}", err);
        }
    }
    pub async fn shutdown(self) {
        self.alive.store(false, Ordering::Relaxed);
    }
}
