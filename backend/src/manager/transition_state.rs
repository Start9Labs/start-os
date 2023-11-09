use helpers::NonDetachingJoinHandle;

/// Used only in the manager/mod and is used to keep track of the state of the manager during the
/// transitional states
pub(super) enum TransitionState {
    BackingUp(NonDetachingJoinHandle<()>),
    Restarting(NonDetachingJoinHandle<()>),
    None,
}

impl TransitionState {
    pub(super) fn take(&mut self) -> Self {
        std::mem::take(self)
    }
    pub(super) fn into_join_handle(self) -> Option<NonDetachingJoinHandle<()>> {
        Some(match self {
            TransitionState::BackingUp(a) => a,
            TransitionState::Restarting(a) => a,
            TransitionState::None => return None,
        })
    }
    pub(super) async fn abort(&mut self) {
        if let Some(s) = self.take().into_join_handle() {
            if s.wait_for_abort().await.is_ok() {
                tracing::trace!("transition completed before abort");
            }
        }
    }
}

impl Default for TransitionState {
    fn default() -> Self {
        TransitionState::None
    }
}
