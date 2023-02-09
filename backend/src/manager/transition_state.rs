use helpers::NonDetachingJoinHandle;

pub(crate) enum TransitionState {
    BackingUp(NonDetachingJoinHandle<()>),
    Restarting(NonDetachingJoinHandle<()>),
    Configuring(NonDetachingJoinHandle<()>),
    None,
}

impl TransitionState {
    pub(crate) fn join_handle(&self) -> Option<&NonDetachingJoinHandle<()>> {
        Some(match self {
            TransitionState::BackingUp(a) => a,
            TransitionState::Restarting(a) => a,
            TransitionState::Configuring(a) => a,
            TransitionState::None => return None,
        })
    }
    pub(crate) fn abort(&self) {
        self.join_handle().map(|transition| transition.abort());
    }
}

impl Default for TransitionState {
    fn default() -> Self {
        TransitionState::None
    }
}