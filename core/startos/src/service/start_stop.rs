use crate::status::MainStatus;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StartStop {
    Start,
    Stop,
}

impl StartStop {
    pub(crate) fn is_start(&self) -> bool {
        matches!(self, StartStop::Start)
    }
}
impl From<MainStatus> for StartStop {
    fn from(value: MainStatus) -> Self {
        match value {
            MainStatus::Stopped => StartStop::Stop,
            MainStatus::Restoring => StartStop::Stop,
            MainStatus::Restarting => StartStop::Start,
            MainStatus::Stopping { .. } => StartStop::Stop,
            MainStatus::Starting => StartStop::Start,
            MainStatus::Running {
                started: _,
                health: _,
            } => StartStop::Start,
            MainStatus::BackingUp { started, health: _ } if started.is_some() => StartStop::Start,
            MainStatus::BackingUp {
                started: _,
                health: _,
            } => StartStop::Stop,
        }
    }
}
