use serde::{Deserialize, Serialize};
use ts_rs::TS;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub enum StartStop {
    Start,
    Stop,
}

impl StartStop {
    pub fn is_start(&self) -> bool {
        matches!(self, StartStop::Start)
    }
}
