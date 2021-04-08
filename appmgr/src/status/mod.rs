use std::collections::HashMap;
use std::sync::Arc;

use anyhow::anyhow;
use bollard::container::{ListContainersOptions, StartContainerOptions, StopContainerOptions};
use bollard::models::{ContainerStateStatusEnum, ContainerSummaryInner};
use bollard::Docker;
use chrono::{DateTime, Utc};
use futures::{StreamExt, TryFutureExt};
use indexmap::IndexMap;
use patch_db::{DbHandle, HasModel, Map, MapModel, Model, ModelData, ModelDataMut};
use serde::{Deserialize, Serialize};

use self::health_check::{HealthCheckId, HealthCheckResult};
use crate::action::docker::DockerAction;
use crate::context::RpcContext;
use crate::db::model::{
    CurrentDependencyInfo, InstalledPackageDataEntryModel, PackageDataEntryModel,
};
use crate::dependencies::{Dependencies, DependencyError};
use crate::id::InterfaceId;
use crate::net::host::Hosts;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::status::health_check::HealthCheckResultVariant;
use crate::util::Invoke;
use crate::Error;

pub mod health_check;

// Assume docker for now
pub async fn synchronize_all(ctx: &RpcContext) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    let mut pkg_ids = crate::db::DatabaseModel::new()
        .package_data()
        .keys(&mut db)
        .await?;
    let mut container_names = Vec::with_capacity(pkg_ids.len());
    for id in pkg_ids.clone().into_iter() {
        if let Some(version) = &*crate::db::DatabaseModel::new()
            .package_data()
            .idx_model(&id)
            .expect(&mut db)
            .await?
            .installed()
            .map(|i| i.manifest().version())
            .get(&mut db)
            .await?
        {
            container_names.push(DockerAction::container_name(id.as_ref(), version));
        } else {
            pkg_ids.remove(&id);
        }
    }
    let mut filters = HashMap::new();
    filters.insert("name".to_owned(), container_names);
    let info = ctx
        .docker
        .list_containers(Some(ListContainersOptions {
            all: true,
            size: false,
            limit: None,
            filters,
        }))
        .await?;
    let mut fuckening = false;
    for summary in info {
        let id = if let Some(id) = summary.names.iter().flatten().find_map(|s| {
            DockerAction::uncontainer_name(s.as_str()).and_then(|id| pkg_ids.take(id))
        }) {
            id
        } else {
            continue;
        };
        async fn status<Db: DbHandle>(
            docker: &Docker,
            id: &PackageId,
            db: &mut Db,
            summary: &ContainerSummaryInner,
        ) -> Result<bool, Error> {
            let pkg_data = crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(id)
                .check(db)
                .await?
                .ok_or_else(|| {
                    Error::new(
                        anyhow!("VersionedPackageData does not exist"),
                        crate::ErrorKind::Database,
                    )
                })?;
            let (mut status, manifest) =
                if let Some(installed) = pkg_data.installed().check(db).await? {
                    (
                        installed.clone().status().get_mut(db).await?,
                        installed.manifest().get(db).await?,
                    )
                } else {
                    return Ok(false);
                };

            let res = status.main.synchronize(docker, &*manifest, summary).await?;

            status.save(db).await?;

            Ok(res)
        }
        match status(&ctx.docker, &id, &mut db, &summary).await {
            Ok(a) => fuckening |= a,
            Err(e) => log::error!("Error syncronizing status of {}: {}", id, e),
        }
    }

    if fuckening {
        tokio::process::Command::new("service")
            .arg("docker")
            .arg("restart")
            .invoke(crate::ErrorKind::Docker)
            .await?;
    }

    for id in pkg_ids {
        log::warn!("No container for {}", id);
    }

    Ok(())
}

pub async fn check_all(ctx: &RpcContext) -> Result<(), Error> {
    let mut db = ctx.db.handle();
    let hosts = Arc::new(
        crate::db::DatabaseModel::new()
            .network()
            .hosts()
            .get(&mut db)
            .await?
            .to_owned(),
    );
    let pkg_ids = crate::db::DatabaseModel::new()
        .package_data()
        .keys(&mut db)
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
                Arc::new(installed.clone().manifest().get(&mut db).await?),
            ));
            status_deps.push((
                installed.clone().status(),
                Arc::new(installed.current_dependencies().get(&mut db).await?),
            ));
        }
    }
    drop(db);
    async fn main_status<Db: DbHandle>(
        status_model: StatusModel,
        manifest: Arc<ModelData<Manifest>>,
        hosts: Arc<Hosts>,
        mut db: Db,
    ) -> Result<MainStatus, Error> {
        let mut status = status_model.get_mut(&mut db).await?;

        status.main.check(&*manifest, &*hosts).await?;

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
            .zip(std::iter::repeat(hosts)),
    )
    .for_each_concurrent(None, move |(((status, manifest), id), hosts)| {
        let status_sender = status_sender.clone();
        async move {
            match tokio::spawn(main_status(status, manifest, hosts, ctx.db.handle()))
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
    pub async fn synchronize(
        &mut self,
        docker: &Docker,
        manifest: &Manifest,
        summary: &ContainerSummaryInner,
    ) -> Result<bool, Error> {
        // true if Docker Fuckening
        async fn check_fuckening(docker: &Docker, manifest: &Manifest) -> Result<bool, Error> {
            Ok(docker
                .inspect_container(
                    &DockerAction::container_name(&manifest.id, &manifest.version),
                    None,
                )
                .await?
                .state
                .as_ref()
                .and_then(|s| s.status)
                == Some(ContainerStateStatusEnum::RUNNING))
        }
        let name = DockerAction::container_name(&manifest.id, &manifest.version);
        let state = summary.state.as_ref().map(|s| s.as_str());
        match state {
            Some("created") | Some("exited") => match self {
                MainStatus::Stopped => (),
                MainStatus::Stopping => {
                    *self = MainStatus::Stopped;
                }
                MainStatus::Running { started, .. } => {
                    *started = Utc::now();
                    docker
                        .start_container(&name, None::<StartContainerOptions<String>>)
                        .await?;
                }
                MainStatus::BackingUp { .. } => (),
                MainStatus::Restoring { .. } => (),
            },
            Some("running") | Some("restarting") => match self {
                MainStatus::Stopped | MainStatus::Stopping | MainStatus::Restoring { .. } => {
                    docker
                        .stop_container(&name, Some(StopContainerOptions { t: 30 }))
                        .await?;
                    return check_fuckening(docker, manifest).await;
                }
                MainStatus::Running { .. } => (),
                MainStatus::BackingUp { .. } => {
                    docker.pause_container(&name).await?;
                }
            },
            Some("paused") => match self {
                MainStatus::Stopped | MainStatus::Stopping | MainStatus::Restoring { .. } => {
                    docker.unpause_container(&name).await?;
                    docker
                        .stop_container(&name, Some(StopContainerOptions { t: 30 }))
                        .await?;
                    return check_fuckening(docker, manifest).await;
                }
                MainStatus::Running { .. } => {
                    docker.unpause_container(&name).await?;
                }
                MainStatus::BackingUp { .. } => (),
            },
            unknown => {
                return Err(Error::new(
                    anyhow!("Unexpected Docker Status: {:?}", unknown),
                    crate::ErrorKind::Docker,
                ));
            }
        }
        Ok(false)
    }
    pub async fn check(&mut self, manifest: &Manifest, hosts: &Hosts) -> Result<(), Error> {
        match self {
            MainStatus::Running { started, health } => {
                *health = manifest
                    .health_checks
                    .check_all(
                        started,
                        &manifest.id,
                        &manifest.version,
                        &manifest.volumes,
                        hosts,
                    )
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
