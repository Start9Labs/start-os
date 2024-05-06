use std::collections::BTreeMap;

use chrono::{DateTime, Utc};
use imbl::OrdMap;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use self::health_check::HealthCheckId;
use crate::prelude::*;
use crate::status::health_check::HealthCheckResult;

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
pub enum MainStatus {
    Stopped,
    Restarting,
    #[serde(rename_all = "camelCase")]
    Stopping {
        timeout: crate::util::serde::Duration,
    },
    Starting,
    #[serde(rename_all = "camelCase")]
    Running {
        #[ts(type = "string")]
        started: DateTime<Utc>,
        #[ts(as = "BTreeMap<HealthCheckId, HealthCheckResult>")]
        health: OrdMap<HealthCheckId, HealthCheckResult>,
    },
    #[serde(rename_all = "camelCase")]
    BackingUp {
        #[ts(type = "string | null")]
        started: Option<DateTime<Utc>>,
        #[ts(as = "BTreeMap<HealthCheckId, HealthCheckResult>")]
        health: OrdMap<HealthCheckId, HealthCheckResult>,
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
            | MainStatus::Stopping { .. }
            | MainStatus::Restarting
            | MainStatus::BackingUp { started: None, .. } => false,
        }
    }
    // pub fn stop(&mut self) {
    //     match self {
    //         MainStatus::Starting { .. } | MainStatus::Running { .. } => {
    //             *self = MainStatus::Stopping;
    //         }
    //         MainStatus::BackingUp { started, .. } => {
    //             *started = None;
    //         }
    //         MainStatus::Stopped | MainStatus::Stopping | MainStatus::Restarting => (),
    //     }
    // }
    pub fn started(&self) -> Option<DateTime<Utc>> {
        match self {
            MainStatus::Running { started, .. } => Some(*started),
            MainStatus::BackingUp { started, .. } => *started,
            MainStatus::Stopped => None,
            MainStatus::Restarting => None,
            MainStatus::Stopping { .. } => None,
            MainStatus::Starting { .. } => None,
        }
    }
    pub fn backing_up(&self) -> Self {
        let (started, health) = match self {
            MainStatus::Starting { .. } => (Some(Utc::now()), Default::default()),
            MainStatus::Running { started, health } => (Some(started.clone()), health.clone()),
            MainStatus::Stopped | MainStatus::Stopping { .. } | MainStatus::Restarting => {
                (None, Default::default())
            }
            MainStatus::BackingUp { .. } => return self.clone(),
        };
        MainStatus::BackingUp { started, health }
    }

    pub fn health(&self) -> Option<&OrdMap<HealthCheckId, HealthCheckResult>> {
        match self {
            MainStatus::Running { health, .. } => Some(health),
            MainStatus::BackingUp { health, .. } => Some(health),
            MainStatus::Stopped | MainStatus::Stopping { .. } | MainStatus::Restarting => None,
            MainStatus::Starting { .. } => None,
        }
    }
}
