use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use chrono::{DateTime, Utc};
use color_eyre::eyre::eyre;
use futures::{FutureExt, StreamExt};
use patch_db::{DbHandle, HasModel, LockType, Map, ModelData};
use serde::{Deserialize, Serialize};
use tracing::instrument;

use self::health_check::HealthCheckId;
use crate::context::RpcContext;
use crate::db::model::{CurrentDependencyInfo, InstalledPackageDataEntryModel};
use crate::dependencies::{break_transitive, DependencyError, DependencyErrors};
use crate::manager::{Manager, Status as ManagerStatus};
use crate::notifications::{NotificationLevel, NotificationSubtype};
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::status::health_check::HealthCheckResult;
use crate::Error;

pub mod health_check;

// Assume docker for now
#[instrument(skip(ctx))]
pub async fn synchronize_all(ctx: &RpcContext) -> Result<(), Error> {
    let pkg_ids = crate::db::DatabaseModel::new()
        .package_data()
        .keys(&mut ctx.db.handle(), false)
        .await?;
    futures::stream::iter(pkg_ids)
        .for_each_concurrent(None, |id| async move {
            #[instrument(skip(ctx))]
            async fn status(ctx: &RpcContext, id: PackageId) -> Result<(), Error> {
                let mut db = ctx.db.handle();
                // TODO: DRAGONS!!
                // this locks all of package data to solve a deadlock issue below. As of the writing of this comment, it
                // hangs during the `check` operation on /package-data/<id>. There is another daemon loop somewhere that
                // is likely iterating through packages in a different order.
                crate::db::DatabaseModel::new()
                    .package_data()
                    .lock(&mut db, LockType::Write)
                    .await;

                // Without the above lock, the below check operation will deadlock
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
                                id,
                                installed
                                    .manifest()
                                    .version()
                                    .get(&mut db, true)
                                    .await?
                                    .to_owned(),
                            ))
                            .await
                            .ok_or_else(|| {
                                Error::new(eyre!("No Manager"), crate::ErrorKind::Docker)
                            })?,
                    )
                } else {
                    return Ok(());
                };

                let res = status.main.synchronize(&manager).await?;

                status.save(&mut db).await?;

                Ok(res)
            }
            if let Err(e) = status(ctx, id.clone()).await {
                tracing::error!("Error syncronizing status of {}: {}", id, e);
                tracing::debug!("{:?}", e);
            }
        })
        .await;

    Ok(())
}

#[instrument(skip(ctx))]
pub async fn check_all(ctx: &RpcContext) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    // TODO: DRAGONS!!
    // this locks all of package data to solve a deadlock issue below. As of the writing of this comment, it
    // hangs during the `check` operation on /package-data/<id>. There is another daemon loop somewhere that
    // is likely iterating through packages in a different order.
    let pkg_ids = crate::db::DatabaseModel::new()
        .package_data()
        .keys(&mut db, true)
        .await?;
    let mut status_manifest = Vec::with_capacity(pkg_ids.len());
    let mut installed_deps = Vec::with_capacity(pkg_ids.len());
    for id in &pkg_ids {
        if let Some(installed) = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .and_then(|m| m.installed())
            .check(&mut db)
            .await?
        {
            let listed_deps = installed
                .clone()
                .manifest()
                .dependencies()
                .get(&mut db, false)
                .await?
                .to_owned()
                .0
                .into_iter()
                .map(|x| x.0)
                .collect::<BTreeSet<PackageId>>();
            status_manifest.push((
                installed.clone().status(),
                Arc::new(installed.clone().manifest().get(&mut db, true).await?),
            ));
            installed_deps.push((
                installed.clone(),
                Arc::new({
                    installed
                        .current_dependencies()
                        .get(&mut db, true)
                        .await?
                        .to_owned()
                        .into_iter()
                        .filter(|(id, _)| listed_deps.contains(id))
                        .collect::<BTreeMap<PackageId, CurrentDependencyInfo>>()
                }),
            ));
        }
    }
    drop(db);
    #[instrument(skip(ctx, db))]
    async fn main_status<Db: DbHandle>(
        ctx: RpcContext,
        status_model: StatusModel,
        manifest: Arc<ModelData<Manifest>>,
        mut db: Db,
    ) -> Result<MainStatus, Error> {
        let mut status = status_model.get_mut(&mut db).await?;

        status.main.check(&ctx, &mut db, &*manifest).await?;

        let res = status.main.clone();

        status.save(&mut db).await?;

        Ok(res)
    }
    let (status_sender, mut statuses_recv) = tokio::sync::mpsc::channel(status_manifest.len() + 1);
    let mut statuses = BTreeMap::new();
    futures::stream::iter(
        status_manifest
            .into_iter()
            .zip(pkg_ids.clone())
            .zip(std::iter::repeat(ctx)),
    )
    .for_each_concurrent(None, move |(((status, manifest), id), ctx)| {
        let status_sender = status_sender.clone();
        async move {
            match main_status(ctx.clone(), status, manifest, ctx.db.handle()).await {
                Err(e) => {
                    tracing::error!("Error running main health check for {}: {}", id, e);
                    tracing::debug!("{:?}", e);
                }
                Ok(status) => {
                    status_sender.send((id, status)).await.expect("unreachable");
                }
            }
        }
    })
    .await;
    while let Some((id, status)) = statuses_recv.recv().await {
        statuses.insert(id, status);
    }
    let statuses = Arc::new(statuses);
    #[instrument(skip(db))]
    async fn dependency_status<Db: DbHandle>(
        id: &PackageId,
        statuses: Arc<BTreeMap<PackageId, MainStatus>>,
        model: InstalledPackageDataEntryModel,
        current_deps: Arc<BTreeMap<PackageId, CurrentDependencyInfo>>,
        mut db: Db,
    ) -> Result<(), Error> {
        for (dep_id, dep_info) in current_deps.iter().filter(|(dep_id, _)| dep_id != &id) {
            if let Some(err) = match statuses.get(dep_id) {
                Some(MainStatus::Running { ref health, .. })
                | Some(MainStatus::BackingUp {
                    started: Some(_),
                    ref health,
                }) => {
                    let mut failures = BTreeMap::new();
                    for check in &dep_info.health_checks {
                        let res = health
                            .get(check)
                            .cloned()
                            .unwrap_or_else(|| HealthCheckResult::Disabled);
                        if !matches!(res, HealthCheckResult::Success) {
                            failures.insert(check.clone(), res);
                        }
                    }
                    if !failures.is_empty() {
                        Some(DependencyError::HealthChecksFailed { failures })
                    } else {
                        None
                    }
                }
                _ => Some(DependencyError::NotRunning),
            } {
                break_transitive(&mut db, id, dep_id, err, &mut BTreeMap::new()).await?;
            } else {
                let mut errs = model
                    .clone()
                    .status()
                    .dependency_errors()
                    .get_mut(&mut db)
                    .await?;
                if matches!(
                    errs.get(dep_id),
                    Some(DependencyError::HealthChecksFailed { .. })
                ) {
                    errs.0.remove(dep_id);
                    errs.save(&mut db).await?;
                }
            }
        }

        Ok(())
    }
    futures::stream::iter(installed_deps.into_iter().zip(pkg_ids.clone()))
        .for_each_concurrent(None, |((installed, deps), id)| {
            let statuses = statuses.clone();
            async move {
                if let Err(e) =
                    dependency_status(&id, statuses, installed, deps, ctx.db.handle()).await
                {
                    tracing::error!("Error running dependency health check for {}: {}", id, e);
                    tracing::debug!("{:?}", e);
                }
            }
        })
        .await;

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
    #[instrument(skip(ctx, db))]
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
                                NotificationSubtype::General
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
