use std::collections::BTreeMap;

use chrono::{DateTime, Utc};
use imbl::OrdMap;
use models::PackageId;
use serde::{Deserialize, Serialize};

use self::health_check::HealthCheckId;
use crate::prelude::*;
use crate::status::health_check::HealthCheckResult;

pub mod health_check;
#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct Status {
    pub configured: bool,
    pub main: MainStatus,
    pub dependency_config_errors: OrdMap<PackageId, String>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(tag = "status")]
#[serde(rename_all = "kebab-case")]
pub enum MainStatus {
    Stopped,
    Restarting,
    Stopping,
    Starting,
    Running {
        started: DateTime<Utc>,
        health: BTreeMap<HealthCheckId, HealthCheckResult>,
    },
    BackingUp {
        started: Option<DateTime<Utc>>,
        health: BTreeMap<HealthCheckId, HealthCheckResult>,
    },
}
impl MainStatus {
    pub fn running(&self) -> bool {
        match self {
            MainStatus::Starting { .. }
            | MainStatus::Running { .. }
            | MainStatus::BackingUp {
                started: Some(_), ..
            } => true,
            MainStatus::Stopped
            | MainStatus::Stopping
            | MainStatus::Restarting
            | MainStatus::BackingUp { started: None, .. } => false,
        }
    }
    pub fn stop(&mut self) {
        match self {
            MainStatus::Starting { .. } | MainStatus::Running { .. } => {
                *self = MainStatus::Stopping;
            }
            MainStatus::BackingUp { started, .. } => {
                *started = None;
            }
            MainStatus::Stopped | MainStatus::Stopping | MainStatus::Restarting => (),
        }
    }
    pub fn started(&self) -> Option<DateTime<Utc>> {
        match self {
            MainStatus::Running { started, .. } => Some(*started),
            MainStatus::BackingUp { started, .. } => *started,
            MainStatus::Stopped => None,
            MainStatus::Restarting => None,
            MainStatus::Stopping => None,
            MainStatus::Starting { .. } => None,
        }
    }
    pub fn backing_up(&self) -> Self {
        let (started, health) = match self {
            MainStatus::Starting { .. } => (Some(Utc::now()), Default::default()),
            MainStatus::Running { started, health } => (Some(started.clone()), health.clone()),
            MainStatus::Stopped | MainStatus::Stopping | MainStatus::Restarting => {
                (None, Default::default())
            }
            MainStatus::BackingUp { .. } => return self.clone(),
        };
        MainStatus::BackingUp { started, health }
    }
}
