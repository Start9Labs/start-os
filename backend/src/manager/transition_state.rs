use tokio::task::JoinHandle;

pub(crate) enum TransitionState {
    // Starting(JoinHandle<()>),
    // Stopping(JoinHandle<()>)
    BackingUp(JoinHandle<()>),
    Restarting(JoinHandle<()>),
    Configuring(JoinHandle<()>),
    None,
}

impl TransitionState {
    pub(crate) fn join_handle(&self) -> Option<&JoinHandle<()>> {
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
