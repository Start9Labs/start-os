use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::status::MainStatus;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub enum StartStop {
    Start,
    Stop,
}

impl StartStop {
    pub(crate) fn is_start(&self) -> bool {
        matches!(self, StartStop::Start)
    }
}
// impl From<MainStatus> for StartStop {
//     fn from(value: MainStatus) -> Self {
//         match value {
//             MainStatus::Stopped => StartStop::Stop,
//             MainStatus::Restoring => StartStop::Stop,
//             MainStatus::Restarting => StartStop::Start,
//             MainStatus::Stopping { .. } => StartStop::Stop,
//             MainStatus::Starting => StartStop::Start,
//             MainStatus::Running {
//                 started: _,
//                 health: _,
//             } => StartStop::Start,
//             MainStatus::BackingUp { on_complete } => on_complete,
//         }
//     }
// }
