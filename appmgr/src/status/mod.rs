use std::collections::BTreeMap;

use chrono::{DateTime, Utc};
use patch_db::{DbHandle, HasModel};
use serde::{Deserialize, Serialize};
use tracing::instrument;

use self::health_check::HealthCheckId;
use crate::context::RpcContext;
use crate::dependencies::DependencyErrors;
use crate::notifications::NotificationLevel;
use crate::s9pk::manifest::Manifest;
use crate::status::health_check::HealthCheckResult;
use crate::Error;

pub mod health_check;
#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct Status {
    pub configured: bool,
    pub main: MainStatus,
    #[model]
    pub dependency_errors: DependencyErrors,
}

#[derive(Debug, Clone, Deserialize, Serialize, HasModel)]
#[serde(tag = "status")]
#[serde(rename_all = "kebab-case")]
pub enum MainStatus {
    Stopped,
    Stopping,
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
    #[instrument(skip(ctx, db, manifest))]
    pub async fn check<Db: DbHandle>(
        &mut self,
        ctx: &RpcContext,
        db: &mut Db,
        manifest: &Manifest,
    ) -> Result<(), Error> {
        match self {
            MainStatus::Running { started, health } => {
                *health = manifest
                    .health_checks
                    .check_all(
                        ctx,
                        *started,
                        &manifest.id,
                        &manifest.version,
                        &manifest.volumes,
                    )
                    .await?;
                let mut should_stop = false;
                for (check, res) in health {
                    match &res {
                        health_check::HealthCheckResult::Failure { error }
                            if manifest
                                .health_checks
                                .0
                                .get(check)
                                .map(|hc| hc.critical)
                                .unwrap_or_default() =>
                        {
                            ctx.notification_manager.notify(
                                db,
                                Some(manifest.id.clone()),
                                NotificationLevel::Error,
                                String::from("Critical Health Check Failed"),
                                format!("{} was shut down because a health check required for its operation failed\n{}", manifest.title, error),
                                (),
                                None,
                            )
                            .await?;
                            should_stop = true;
                        }
                        _ => (),
                    }
                }
                if should_stop {
                    *self = MainStatus::Stopping;
                }
            }
            _ => (),
        }
        Ok(())
    }
    pub fn running(&self) -> bool {
        match self {
            MainStatus::Running { .. }
            | MainStatus::BackingUp {
                started: Some(_), ..
            } => true,
            _ => false,
        }
    }
    pub fn stop(&mut self) {
        match self {
            MainStatus::Running { .. } => {
                *self = MainStatus::Stopping;
            }
            MainStatus::BackingUp { started, .. } => {
                *started = None;
            }
            _ => (),
        }
    }
}
