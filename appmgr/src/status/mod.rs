use std::collections::HashMap;
use std::sync::Arc;

use anyhow::anyhow;
use chrono::{DateTime, Utc};
use futures::StreamExt;
use indexmap::IndexMap;
use patch_db::{DbHandle, HasModel, Map, MapModel, ModelData};
use serde::{Deserialize, Serialize};

use self::health_check::{HealthCheckId, HealthCheckResult};
use crate::context::RpcContext;
use crate::db::model::CurrentDependencyInfo;
use crate::dependencies::DependencyError;
use crate::manager::{Manager, Status as ManagerStatus};
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::status::health_check::HealthCheckResultVariant;
use crate::Error;

pub mod health_check;

// Assume docker for now
pub async fn synchronize_all(ctx: &RpcContext) -> Result<(), Error> {
    let mut pkg_ids = crate::db::DatabaseModel::new()
        .package_data()
        .keys(&mut ctx.db.handle(), true)
        .await?;
    for id in pkg_ids {
        async fn status(ctx: &RpcContext, id: PackageId) -> Result<(), Error> {
            let mut db = ctx.db.handle();
            let pkg_data = crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(&id)
                .check(&mut db)
                .await?
                .ok_or_else(|| {
                    Error::new(
                        anyhow!("VersionedPackageData does not exist"),
                        crate::ErrorKind::Database,
                    )
                })?;
            let (mut status, manager) =
                if let Some(installed) = pkg_data.installed().check(&mut db).await? {
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
    }

    Ok(())
}

pub async fn check_all(ctx: &RpcContext) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    let pkg_ids = crate::db::DatabaseModel::new()
        .package_data()
        .keys(&mut db, true)
        .await?;
    let mut status_manifest = Vec::with_capacity(pkg_ids.len());
    let mut status_deps = Vec::with_capacity(pkg_ids.len());
    for id in &pkg_ids {
        let model = crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(id)
            .check(&mut db)
            .await?
            .ok_or_else(|| {
                Error::new(
                    anyhow!("PackageDataEntry does not exist"),
                    crate::ErrorKind::Database,
                )
            })?;
        if let Some(installed) = model.installed().check(&mut db).await? {
            status_manifest.push((
                installed.clone().status(),
                Arc::new(installed.clone().manifest().get(&mut db, true).await?),
            ));
            status_deps.push((
                installed.clone().status(),
                Arc::new(installed.current_dependencies().get(&mut db, true).await?),
            ));
        }
    }
    drop(db);
    async fn main_status<Db: DbHandle>(
        status_model: StatusModel,
        manifest: Arc<ModelData<Manifest>>,
        mut db: Db,
    ) -> Result<MainStatus, Error> {
        let mut status = status_model.get_mut(&mut db).await?;

        status.main.check(&*manifest).await?;

        let res = status.main.clone();

        status.save(&mut db).await?;

        Ok(res)
    }
    let (status_sender, mut statuses_recv) = tokio::sync::mpsc::channel(status_manifest.len() + 1);
    let mut statuses = HashMap::with_capacity(status_manifest.len());
    futures::stream::iter(status_manifest.into_iter().zip(pkg_ids.clone()))
        .for_each_concurrent(None, move |((status, manifest), id)| {
            let status_sender = status_sender.clone();
            async move {
                match tokio::spawn(main_status(status, manifest, ctx.db.handle()))
                    .await
                    .unwrap()
                {
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
        statuses: Arc<HashMap<PackageId, MainStatus>>,
        status_model: StatusModel,
        current_deps: Arc<ModelData<IndexMap<PackageId, CurrentDependencyInfo>>>,
        mut db: Db,
    ) -> Result<(), Error> {
        let mut status = status_model.get_mut(&mut db).await?;

        status
            .dependency_errors
            .update_health_based(&current_deps, &*statuses)
            .await?;

        status.save(&mut db).await?;

        Ok(())
    }
    futures::stream::iter(status_deps.into_iter().zip(pkg_ids.clone()))
        .for_each_concurrent(None, |((status, deps), id)| {
            let statuses = statuses.clone();
            async move {
                if let Err(e) =
                    tokio::spawn(dependency_status(statuses, status, deps, ctx.db.handle()))
                        .await
                        .unwrap()
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

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(tag = "status")]
#[serde(rename_all = "kebab-case")]
pub enum MainStatus {
    Stopped,
    Stopping,
    Running {
        started: DateTime<Utc>,
        health: IndexMap<HealthCheckId, HealthCheckResult>,
    },
    BackingUp {
        started: Option<DateTime<Utc>>,
        health: IndexMap<HealthCheckId, HealthCheckResult>,
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
    pub async fn check(&mut self, manifest: &Manifest) -> Result<(), Error> {
        match self {
            MainStatus::Running { started, health } => {
                *health = manifest
                    .health_checks
                    .check_all(started, &manifest.id, &manifest.version, &manifest.volumes)
                    .await?;
                for (check, res) in health {
                    if matches!(
                        res.result,
                        health_check::HealthCheckResultVariant::Failure { .. }
                    ) && manifest
                        .health_checks
                        .0
                        .get(check)
                        .map(|hc| hc.critical)
                        .unwrap_or_default()
                    {
                        todo!("emit notification");
                        *self = MainStatus::Stopping;
                    }
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
pub struct DependencyErrors(pub IndexMap<PackageId, DependencyError>);
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
        db: &mut Db,
        manifest: &Manifest,
        current_dependencies: &IndexMap<PackageId, CurrentDependencyInfo>,
    ) -> Result<DependencyErrors, Error> {
        let mut res = IndexMap::new();
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
    async fn update_health_based(
        &mut self,
        dependencies: &IndexMap<PackageId, CurrentDependencyInfo>,
        statuses: &HashMap<PackageId, MainStatus>,
    ) -> Result<(), Error> {
        for (dep_id, dep_info) in dependencies {
            if matches!(
                self.get(&dep_id),
                Some(&DependencyError::NotRunning)
                    | Some(&DependencyError::HealthChecksFailed { .. })
                    | None
            ) {
                match statuses.get(dep_id) {
                    Some(MainStatus::Running { ref health, .. })
                    | Some(MainStatus::BackingUp {
                        started: Some(_),
                        ref health,
                    }) => {
                        let mut failures = IndexMap::new();
                        for check in &dep_info.health_checks {
                            let res =
                                health
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
                        self.0.insert(
                            dep_id.clone(),
                            DependencyError::HealthChecksFailed { failures },
                        );
                    }
                    _ => {
                        self.0.insert(dep_id.clone(), DependencyError::NotRunning);
                    }
                }
            }
        }

        Ok(())
    }
}
