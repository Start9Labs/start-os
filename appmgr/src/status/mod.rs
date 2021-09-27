use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::Arc;

use anyhow::anyhow;
use chrono::{DateTime, Utc};
use futures::future::BoxFuture;
use futures::{FutureExt, StreamExt};
use patch_db::{DbHandle, HasModel, Map, MapModel, ModelData};
use serde::{Deserialize, Serialize};

use self::health_check::{HealthCheckId, HealthCheckResult};
use crate::context::RpcContext;
use crate::db::model::{CurrentDependencyInfo, InstalledPackageDataEntryModel};
use crate::dependencies::{DependencyError, TaggedDependencyError};
use crate::manager::{Manager, Status as ManagerStatus};
use crate::notifications::{notify, NotificationLevel, NotificationSubtype};
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::status::health_check::HealthCheckResultVariant;
use crate::Error;

pub mod health_check;

// Assume docker for now
pub async fn synchronize_all(ctx: &RpcContext) -> Result<(), Error> {
    let pkg_ids = crate::db::DatabaseModel::new()
        .package_data()
        .keys(&mut ctx.db.handle(), false)
        .await?;
    futures::stream::iter(pkg_ids)
        .for_each_concurrent(None, |id| async move {
            async fn status(ctx: &RpcContext, id: PackageId) -> Result<(), Error> {
                let mut db = ctx.db.handle();
                // TODO: DRAGONS!!
                // this locks all of package data to solve a deadlock issue below. As of the writing of this comment, it
                // hangs during the `check` operation on /package-data/<id>. There is another daemon loop somewhere that
                // is likely iterating through packages in a different order.
                crate::db::DatabaseModel::new()
                    .package_data()
                    .lock(&mut db, true)
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
                                Error::new(anyhow!("No Manager"), crate::ErrorKind::Docker)
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
                log::error!("Error syncronizing status of {}: {}", id, e);
            }
        })
        .await;

    Ok(())
}

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
                .collect::<HashSet<PackageId>>();
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
    async fn main_status<Db: DbHandle>(
        ctx: RpcContext,
        status_model: StatusModel,
        manifest: Arc<ModelData<Manifest>>,
        mut db: Db,
    ) -> Result<MainStatus, Error> {
        let mut status = status_model.get_mut(&mut db).await?;

        status.main.check(&ctx, &*manifest).await?;

        let res = status.main.clone();

        status.save(&mut db).await?;

        Ok(res)
    }
    let (status_sender, mut statuses_recv) = tokio::sync::mpsc::channel(status_manifest.len() + 1);
    let mut statuses = HashMap::with_capacity(status_manifest.len());
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
                    log::error!("Error running main health check for {}: {}", id, e);
                    log::debug!("{:?}", e);
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
    async fn dependency_status<Db: DbHandle>(
        id: &PackageId,
        statuses: Arc<HashMap<PackageId, MainStatus>>,
        model: InstalledPackageDataEntryModel,
        current_deps: Arc<BTreeMap<PackageId, CurrentDependencyInfo>>,
        mut db: Db,
    ) -> Result<(), Error> {
        for (dep_id, dep_info) in &*current_deps {
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
                            .unwrap_or_else(|| HealthCheckResult {
                                result: HealthCheckResultVariant::Disabled,
                                time: Utc::now(),
                            });
                        if !matches!(res.result, HealthCheckResultVariant::Success) {
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
                handle_broken_dependents(
                    &mut db,
                    id,
                    dep_id,
                    model.clone(),
                    err,
                    &mut BTreeMap::new(),
                )
                .await?;
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
                    log::error!("Error running dependency health check for {}: {}", id, e);
                    log::debug!("{:?}", e);
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
    pub async fn check(&mut self, ctx: &RpcContext, manifest: &Manifest) -> Result<(), Error> {
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
                    match &res.result {
                        health_check::HealthCheckResultVariant::Failure { error }
                            if manifest
                                .health_checks
                                .0
                                .get(check)
                                .map(|hc| hc.critical)
                                .unwrap_or_default() =>
                        {
                            notify(
                                &ctx.secret_store,
                                &ctx.db,
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

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct DependencyErrors(pub BTreeMap<PackageId, DependencyError>);
impl Map for DependencyErrors {
    type Key = PackageId;
    type Value = DependencyError;
    fn get(&self, key: &Self::Key) -> Option<&Self::Value> {
        self.0.get(key)
    }
}
impl HasModel for DependencyErrors {
    type Model = MapModel<Self>;
}
impl DependencyErrors {
    pub async fn init<Db: DbHandle>(
        ctx: &RpcContext,
        db: &mut Db,
        manifest: &Manifest,
        current_dependencies: &BTreeMap<PackageId, CurrentDependencyInfo>,
    ) -> Result<DependencyErrors, Error> {
        let mut res = BTreeMap::new();
        for dep_id in current_dependencies.keys() {
            if let Err(e) = manifest
                .dependencies
                .0
                .get(dep_id)
                .ok_or_else(|| {
                    Error::new(
                        anyhow!("current dependency not in manifest"),
                        crate::ErrorKind::Dependency,
                    )
                })?
                .satisfied(
                    ctx,
                    db,
                    dep_id,
                    None,
                    &manifest.id,
                    &manifest.version,
                    &manifest.volumes,
                )
                .await?
            {
                res.insert(dep_id.clone(), e);
            }
        }
        Ok(DependencyErrors(res))
    }
}

pub fn handle_broken_dependents<'a, Db: DbHandle>(
    db: &'a mut Db,
    id: &'a PackageId,
    dependency: &'a PackageId,
    model: InstalledPackageDataEntryModel,
    error: DependencyError,
    breakages: &'a mut BTreeMap<PackageId, TaggedDependencyError>,
) -> BoxFuture<'a, Result<(), Error>> {
    async move {
        let mut status = model.clone().status().get_mut(db).await?;

        let old = status.dependency_errors.0.remove(id);
        let newly_broken = old.is_none();
        status.dependency_errors.0.insert(
            id.clone(),
            if let Some(old) = old {
                old.merge_with(error.clone())
            } else {
                error.clone()
            },
        );
        if newly_broken {
            breakages.insert(
                id.clone(),
                TaggedDependencyError {
                    dependency: dependency.clone(),
                    error: error.clone(),
                },
            );
            if status.main.running() {
                if model
                    .clone()
                    .manifest()
                    .dependencies()
                    .idx_model(dependency)
                    .get(db, true)
                    .await?
                    .into_owned()
                    .ok_or_else(|| {
                        Error::new(
                            anyhow!("{} not in listed dependencies", dependency),
                            crate::ErrorKind::Database,
                        )
                    })?
                    .critical
                {
                    status.main.stop();
                    let dependents = model.current_dependents().get(db, true).await?;
                    for (dependent, _) in &*dependents {
                        let dependent_model = crate::db::DatabaseModel::new()
                            .package_data()
                            .idx_model(dependent)
                            .and_then(|pkg| pkg.installed())
                            .check(db)
                            .await?
                            .ok_or_else(|| {
                                Error::new(
                                    anyhow!("{} is not installed", dependent),
                                    crate::ErrorKind::NotFound,
                                )
                            })?;
                        handle_broken_dependents(
                            db,
                            dependent,
                            id,
                            dependent_model,
                            DependencyError::NotRunning,
                            breakages,
                        )
                        .await?;
                    }
                }
            }
        }

        status.save(db).await?;

        Ok(())
    }
    .boxed()
}
