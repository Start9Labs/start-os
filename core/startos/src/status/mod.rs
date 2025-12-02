use std::collections::BTreeMap;

use chrono::{DateTime, Utc};
use models::{ErrorData, HealthCheckId};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::prelude::*;
use crate::service::start_stop::StartStop;
use crate::status::health_check::NamedHealthCheckResult;

pub mod health_check;

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct StatusInfo {
    pub health: BTreeMap<HealthCheckId, NamedHealthCheckResult>,
    pub error: Option<ErrorData>,
    #[ts(type = "string | null")]
    pub started: Option<DateTime<Utc>>,
    pub desired: DesiredStatus,
}
impl StatusInfo {
    pub fn stop(&mut self) {
        self.desired = self.desired.stop();
        self.health.clear();
    }
}
impl Model<StatusInfo> {
    pub fn stop(&mut self) -> Result<(), Error> {
        self.as_desired_mut().map_mutate(|s| Ok(s.stop()))?;
        self.as_health_mut().ser(&Default::default())?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq, TS)]
#[serde(tag = "main")]
#[serde(rename_all = "kebab-case")]
#[serde(rename_all_fields = "camelCase")]
pub enum DesiredStatus {
    Stopped,
    Restarting,
    Running,
    BackingUp { on_complete: StartStop },
}
impl Default for DesiredStatus {
    fn default() -> Self {
        Self::Stopped
    }
}
impl DesiredStatus {
    pub fn running(&self) -> bool {
        match self {
            Self::Running
            | Self::Restarting
            | Self::BackingUp {
                on_complete: StartStop::Start,
            } => true,
            Self::Stopped
            | Self::BackingUp {
                on_complete: StartStop::Stop,
            } => false,
        }
    }
    pub fn run_state(&self) -> StartStop {
        if self.running() {
            StartStop::Start
        } else {
            StartStop::Stop
        }
    }

    pub fn backing_up(&self) -> Self {
        Self::BackingUp {
            on_complete: self.run_state(),
        }
    }

    pub fn stop(&self) -> Self {
        match self {
            Self::BackingUp { .. } => Self::BackingUp {
                on_complete: StartStop::Stop,
            },
            _ => Self::Stopped,
        }
    }

    pub fn start(&self) -> Self {
        match self {
            Self::BackingUp { .. } => Self::BackingUp {
                on_complete: StartStop::Start,
            },
            Self::Stopped => Self::Running,
            x => *x,
        }
    }

    pub fn restart(&self) -> Self {
        match self {
            Self::Running => Self::Restarting,
            x => *x, // no-op: restart is meaningless in any other state
        }
    }
}
