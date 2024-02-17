use std::ops::Deref;
use std::sync::Arc;

use futures::{Future, FutureExt};
use tokio::sync::watch;

use crate::service::start_stop::StartStop;
use crate::util::actor::BackgroundJobs;
use crate::util::future::{CancellationHandle, RemoteCancellable};

pub mod backup;
pub mod restart;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TransitionKind {
    BackingUp,
    Restarting,
}

/// Used only in the manager/mod and is used to keep track of the state of the manager during the
/// transitional states
pub struct TransitionState {
    cancel_handle: CancellationHandle,
    kind: TransitionKind,
}

impl TransitionState {
    pub fn kind(&self) -> TransitionKind {
        self.kind
    }
    pub async fn abort(mut self) {
        self.cancel_handle.cancel_and_wait().await
    }
    fn new(
        task: impl Future<Output = ()> + Send + 'static,
        kind: TransitionKind,
        jobs: &mut BackgroundJobs,
    ) -> Self {
        let task = RemoteCancellable::new(task);
        let cancel_handle = task.cancellation_handle();
        jobs.add_job(task.map(|_| ()));
        Self {
            cancel_handle,
            kind,
        }
    }
}
impl Drop for TransitionState {
    fn drop(&mut self) {
        self.cancel_handle.cancel();
    }
}

#[derive(Clone)]
pub struct TempDesiredState(pub(super) Arc<watch::Sender<Option<StartStop>>>);
impl TempDesiredState {
    pub fn stop(&self) {
        self.0.send_replace(Some(StartStop::Stop));
    }
    pub fn start(&self) {
        self.0.send_replace(Some(StartStop::Start));
    }
}
impl Drop for TempDesiredState {
    fn drop(&mut self) {
        self.0.send_replace(None);
    }
}
impl Deref for TempDesiredState {
    type Target = watch::Sender<Option<StartStop>>;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
