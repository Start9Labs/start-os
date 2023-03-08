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
    pub(crate) fn is_stop(&self) -> bool {
        matches!(self, StartStop::Stop)
    }
}
impl From<MainStatus> for StartStop {
    fn from(value: MainStatus) -> Self {
        match value {
            MainStatus::Stopped => StartStop::Stop,
            MainStatus::Restarting => StartStop::Start,
            MainStatus::Stopping => StartStop::Stop,
            MainStatus::Starting => StartStop::Start,
            MainStatus::Running { started, health } => StartStop::Start,
            MainStatus::BackingUp { was_running } => {
                if was_running {
                    StartStop::Start
                } else {
                    StartStop::Stop
                }
            }
        }
    }
}
