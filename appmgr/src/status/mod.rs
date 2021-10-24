use std::collections::BTreeMap;

use chrono::{DateTime, Utc};
use color_eyre::eyre::eyre;
use futures::{FutureExt, StreamExt};
use patch_db::{DbHandle, HasModel, Map};
use serde::{Deserialize, Serialize};
use tracing::instrument;

use self::health_check::HealthCheckId;
use crate::context::RpcContext;
use crate::dependencies::DependencyErrors;
use crate::manager::{Manager, Status as ManagerStatus};
use crate::notifications::NotificationLevel;
use crate::s9pk::manifest::Manifest;
use crate::status::health_check::HealthCheckResult;
use crate::Error;

pub mod health_check;

#[instrument(skip(ctx))]
pub async fn synchronize_all(ctx: &RpcContext) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    let pkg_ids = crate::db::DatabaseModel::new()
        .package_data()
        .keys(&mut db, false)
        .await?;
    for id in pkg_ids {
        if let Err(e) = async {
            let (mut status, manager) = if let Some(installed) = crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(&id)
                .and_then(|m| m.installed())
                .check(&mut db)
                .await?
            {
                (
                    installed.clone().status().get_mut(&mut db).await?,
                    ctx.managers
                        .get(&(
                            id.clone(),
                            installed
                                .manifest()
                                .version()
                                .get(&mut db, true)
                                .await?
                                .to_owned(),
                        ))
                        .await
                        .ok_or_else(|| Error::new(eyre!("No Manager"), crate::ErrorKind::Docker))?,
                )
            } else {
                return Ok::<_, Error>(());
            };

            let res = status.main.synchronize(&manager).await?;

            status.save(&mut db).await?;

            Ok(res)
        }
        .await
        {
            tracing::error!("Error syncronizing status of {}: {}", id, e);
            tracing::debug!("{:?}", e);
        }
    }
    Ok(())
}

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
    Restoring {
        running: bool,
    },
}
impl MainStatus {
    #[instrument(skip(manager))]
    pub async fn synchronize(&mut self, manager: &Manager) -> Result<(), Error> {
        match manager.status() {
            ManagerStatus::Stopped => match self {
                MainStatus::Stopped => (),
                MainStatus::Stopping => {
                    *self = MainStatus::Stopped;
                }
                MainStatus::Running { started, .. } => {
                    *started = Utc::now();
                    manager.start().await?;
                }
                MainStatus::BackingUp { .. } => (),
                MainStatus::Restoring { .. } => (),
            },
            ManagerStatus::Running => match self {
                MainStatus::Stopped | MainStatus::Stopping | MainStatus::Restoring { .. } => {
                    manager.stop().await?;
                }
                MainStatus::Running { .. } => (),
                MainStatus::BackingUp { .. } => {
                    manager.pause().await?;
                }
            },
            ManagerStatus::Paused => match self {
                MainStatus::Stopped | MainStatus::Stopping | MainStatus::Restoring { .. } => {
                    manager.stop().await?;
                }
                MainStatus::Running { .. } => {
                    manager.resume().await?;
                }
                MainStatus::BackingUp { .. } => (),
            },
        }
        Ok(())
    }
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
            }
            | MainStatus::Restoring { running: true } => true,
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
            MainStatus::Restoring { running } => {
                *running = false;
            }
            _ => (),
        }
    }
}
