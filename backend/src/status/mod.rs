use std::collections::BTreeMap;

use chrono::{DateTime, Utc};
use patch_db::{HasModel, Model};
use serde::{Deserialize, Serialize};

use self::health_check::HealthCheckId;
use crate::dependencies::DependencyErrors;
use crate::status::health_check::HealthCheckResult;

pub mod health_check;
#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct Status {
    pub configured: bool,
    #[model]
    pub main: MainStatus,
    #[model]
    pub dependency_errors: DependencyErrors,
}

#[derive(Debug, Clone, Deserialize, Serialize, HasModel)]
#[serde(tag = "status")]
#[serde(rename_all = "kebab-case")]
pub enum MainStatus {
    Stopped,
    Restarting,
    Stopping,
    Starting {
        restarting: bool,
    },
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
}
impl MainStatusModel {
    pub fn started(self) -> Model<Option<DateTime<Utc>>> {
        self.0.child("started")
    }
}
