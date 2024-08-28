use std::collections::BTreeMap;

use chrono::{DateTime, Utc};
use imbl::OrdMap;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use self::health_check::HealthCheckId;
use crate::prelude::*;
use crate::service::start_stop::StartStop;
use crate::status::health_check::NamedHealthCheckResult;

pub mod health_check;
#[derive(Clone, Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Status {
    pub configured: bool,
    pub main: MainStatus,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, TS)]
#[serde(tag = "status")]
#[serde(rename_all = "camelCase")]
#[serde(rename_all_fields = "camelCase")]
pub enum MainStatus {
    Stopped,
    Restarting,
    Restoring,
    Stopping,
    Starting {
        #[ts(as = "BTreeMap<HealthCheckId, NamedHealthCheckResult>")]
        health: OrdMap<HealthCheckId, NamedHealthCheckResult>,
    },
    Running {
        #[ts(type = "string")]
        started: DateTime<Utc>,
        #[ts(as = "BTreeMap<HealthCheckId, NamedHealthCheckResult>")]
        health: OrdMap<HealthCheckId, NamedHealthCheckResult>,
    },
    BackingUp {
        on_complete: StartStop,
    },
}
impl MainStatus {
    pub fn running(&self) -> bool {
        match self {
            MainStatus::Starting { .. }
            | MainStatus::Running { .. }
            | MainStatus::Restarting
            | MainStatus::BackingUp {
                on_complete: StartStop::Start,
            } => true,
            MainStatus::Stopped
            | MainStatus::Restoring
            | MainStatus::Stopping { .. }
            | MainStatus::BackingUp {
                on_complete: StartStop::Stop,
            } => false,
        }
    }

    pub fn backing_up(self) -> Self {
        MainStatus::BackingUp {
            on_complete: if self.running() {
                StartStop::Start
            } else {
                StartStop::Stop
            },
        }
    }

    pub fn health(&self) -> Option<&OrdMap<HealthCheckId, NamedHealthCheckResult>> {
        match self {
            MainStatus::Running { health, .. } | MainStatus::Starting { health } => Some(health),
            MainStatus::BackingUp { .. }
            | MainStatus::Stopped
            | MainStatus::Restoring
            | MainStatus::Stopping { .. }
            | MainStatus::Restarting => None,
        }
    }
}
