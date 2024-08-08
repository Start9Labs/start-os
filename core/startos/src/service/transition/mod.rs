use std::sync::Arc;

use futures::{Future, FutureExt};
use tokio::sync::watch;

use super::persistent_container::ServiceState;
use crate::service::start_stop::StartStop;
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::future::{CancellationHandle, RemoteCancellable};

pub mod backup;
pub mod restart;
pub mod restore;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TransitionKind {
    BackingUp,
    Restarting,
    Restoring,
}

/// Used only in the manager/mod and is used to keep track of the state of the manager during the
/// transitional states
pub struct TransitionState {
    cancel_handle: CancellationHandle,
    kind: TransitionKind,
}
impl ::std::fmt::Debug for TransitionState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TransitionState")
            .field("kind", &self.kind)
            .finish_non_exhaustive()
    }
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
        jobs: &BackgroundJobQueue,
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

#[derive(Debug, Clone)]
pub struct TempDesiredRestore(pub(super) Arc<watch::Sender<ServiceState>>, StartStop);
impl TempDesiredRestore {
    pub fn new(state: &Arc<watch::Sender<ServiceState>>) -> Self {
        Self(state.clone(), state.borrow().desired_state)
    }
    pub fn stop(&self) {
        self.0
            .send_modify(|s| s.temp_desired_state = Some(StartStop::Stop));
    }
    pub fn restore(&self) -> StartStop {
        let restore_state = self.1;
        self.0
            .send_modify(|s| s.temp_desired_state = Some(restore_state));
        restore_state
    }
}
impl Drop for TempDesiredRestore {
    fn drop(&mut self) {
        self.0.send_modify(|s| {
            s.temp_desired_state.take();
            s.transition_state.take();
        });
    }
}
// impl Deref for TempDesiredState {
//     type Target = watch::Sender<Option<StartStop>>;
//     fn deref(&self) -> &Self::Target {
//         &*self.0
//     }
// }
