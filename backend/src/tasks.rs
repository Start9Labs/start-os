use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};

use tokio::sync::{mpsc, oneshot};

use futures::{stream, StreamExt};
use tracing::{debug, error};

use crate::backup::{backup_bulk::backup_all_task, target::BackupTargetId};
use crate::context::RpcContext;
use crate::{db::util::WithRevision, Error};

// TODO In Progress

// TODO wait for closed done

// TODO Config
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
impl Into<Task> for BackupAll {
    fn into(self) -> Task {
        Task::BackupAll(self)
    }
}

pub enum Task {
    BackupAll(BackupAll),
}

impl std::fmt::Debug for Task {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BackupAll(_) => f.write_str("BackupAll"),
        }
    }
}

impl Task {
    fn is_safe_concurrent(&self, _other: &Task) -> bool {
        match self {
            Task::BackupAll(_) => false,
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
