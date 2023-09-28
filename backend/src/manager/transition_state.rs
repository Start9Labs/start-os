use helpers::NonDetachingJoinHandle;

/// Used only in the manager/mod and is used to keep track of the state of the manager during the
/// transitional states
pub(super) enum TransitionState {
    BackingUp(NonDetachingJoinHandle<()>),
    Restarting(NonDetachingJoinHandle<()>),
    Configuring(NonDetachingJoinHandle<()>),
    None,
}

impl TransitionState {
    pub(super) fn join_handle(&self) -> Option<&NonDetachingJoinHandle<()>> {
        Some(match self {
            TransitionState::BackingUp(a) => a,
            TransitionState::Restarting(a) => a,
            TransitionState::Configuring(a) => a,
            TransitionState::None => return None,
        })
    }
    pub(super) fn abort(&self) {
        self.join_handle().map(|transition| transition.abort());
    }
}

impl Default for TransitionState {
    fn default() -> Self {
        TransitionState::None
    }
}
