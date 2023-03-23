use std::collections::BTreeMap;

use chrono::{DateTime, Utc};
use models::HealthCheckId;
use patch_db::HasModel;
use serde::{Deserialize, Serialize};

use crate::dependencies::DependencyErrors;
use crate::prelude::*;
#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct Status {
    pub configured: bool,
    pub main: MainStatus,
    pub dependency_errors: DependencyErrors,
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
        health: BTreeMap<HealthCheckId, String>,
    },
    BackingUp {
        was_running: bool,
    },
}
impl MainStatus {
    pub fn running(&self) -> bool {
        match self {
            MainStatus::Starting
            | MainStatus::Running { .. }
            | MainStatus::BackingUp { was_running: true } => true,
            MainStatus::Stopped
            | MainStatus::Stopping
            | MainStatus::Restarting
            | MainStatus::BackingUp { was_running: false } => false,
        }
    }
    pub fn stop(&mut self) {
        match self {
            MainStatus::Starting { .. } | MainStatus::Running { .. } => {
                *self = MainStatus::Stopping;
            }
            MainStatus::BackingUp { was_running } => {
                *was_running = false;
            }
            MainStatus::Stopped | MainStatus::Stopping | MainStatus::Restarting => (),
        }
    }

    pub fn backing_up(&self) -> Self {
        MainStatus::BackingUp {
            was_running: self.running(),
        }
    }
}
