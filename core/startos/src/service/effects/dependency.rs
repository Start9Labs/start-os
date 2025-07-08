use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use std::str::FromStr;

use clap::builder::ValueParserFactory;
use exver::VersionRange;
use imbl::OrdMap;
use imbl_value::InternedString;
use models::{FromStrParser, HealthCheckId, PackageId, ReplayId, VersionString, VolumeId};

use crate::db::model::package::{
    CurrentDependencies, CurrentDependencyInfo, CurrentDependencyKind, ManifestPreference,
    TaskEntry,
};
use crate::disk::mount::filesystem::bind::{Bind, FileType};
use crate::disk::mount::filesystem::idmapped::IdMapped;
use crate::disk::mount::filesystem::{FileSystem, MountType};
use crate::disk::mount::util::{is_mountpoint, unmount};
use crate::service::effects::prelude::*;
use crate::status::health_check::NamedHealthCheckResult;
use crate::volume::data_dir;
use crate::DATA_DIR;

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct MountTarget {
    package_id: PackageId,
    volume_id: VolumeId,
    subpath: Option<PathBuf>,
    readonly: bool,
    filetype: FileType,
}
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct MountParams {
    location: PathBuf,
    target: MountTarget,
}
pub async fn mount(
    context: EffectContext,
    MountParams {
        location,
        target:
            MountTarget {
                package_id,
                volume_id,
                subpath,
                readonly,
                filetype,
            },
    }: MountParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let subpath = subpath.unwrap_or_default();
    let subpath = subpath.strip_prefix("/").unwrap_or(&subpath);
    let source = data_dir(DATA_DIR, &package_id, &volume_id).join(subpath);
    let location = location.strip_prefix("/").unwrap_or(&location);
    let mountpoint = context
        .seed
        .persistent_container
        .lxc_container
        .get()
        .or_not_found("lxc container")?
        .rootfs_dir()
        .join(location);

    if is_mountpoint(&mountpoint).await? {
        unmount(&mountpoint, true).await?;
    }
    IdMapped::new(Bind::new(source).with_type(filetype), 0, 100000, 65536)
        .mount(
            mountpoint,
            if readonly {
                MountType::ReadOnly
            } else {
                MountType::ReadWrite
            },
        )
        .await?;

    Ok(())
}

pub async fn get_installed_packages(context: EffectContext) -> Result<BTreeSet<PackageId>, Error> {
    context
        .deref()?
        .seed
        .ctx
        .db
        .peek()
        .await
        .into_public()
        .into_package_data()
        .keys()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum DependencyKind {
    Exists,
    Running,
}
#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase", tag = "kind")]
#[serde(rename_all_fields = "camelCase")]
#[ts(export)]
pub enum DependencyRequirement {
    Running {
        id: PackageId,
        health_checks: BTreeSet<HealthCheckId>,
        #[ts(type = "string")]
        version_range: VersionRange,
    },
    Exists {
        id: PackageId,
        #[ts(type = "string")]
        version_range: VersionRange,
    },
}
// filebrowser:exists,bitcoind:running:foo+bar+baz
impl FromStr for DependencyRequirement {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once(':') {
            Some((id, "e")) | Some((id, "exists")) => Ok(Self::Exists {
                id: id.parse()?,
                version_range: "*".parse()?, // TODO
            }),
            Some((id, rest)) => {
                let health_checks = match rest.split_once(':') {
                    Some(("r", rest)) | Some(("running", rest)) => rest
                        .split('+')
                        .map(|id| id.parse().map_err(Error::from))
                        .collect(),
                    Some((kind, _)) => Err(Error::new(
                        eyre!("unknown dependency kind {kind}"),
                        ErrorKind::InvalidRequest,
                    )),
                    None => match rest {
                        "r" | "running" => Ok(BTreeSet::new()),
                        kind => Err(Error::new(
                            eyre!("unknown dependency kind {kind}"),
                            ErrorKind::InvalidRequest,
                        )),
                    },
                }?;
                Ok(Self::Running {
                    id: id.parse()?,
                    health_checks,
                    version_range: "*".parse()?, // TODO
                })
            }
            None => Ok(Self::Running {
                id: s.parse()?,
                health_checks: BTreeSet::new(),
                version_range: "*".parse()?, // TODO
            }),
        }
    }
}
impl ValueParserFactory for DependencyRequirement {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        FromStrParser::new()
    }
}
#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "camelCase")]
#[ts(export)]
pub struct SetDependenciesParams {
    dependencies: Vec<DependencyRequirement>,
}
pub async fn set_dependencies(
    context: EffectContext,
    SetDependenciesParams { dependencies }: SetDependenciesParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let id = &context.seed.id;

    let mut deps = BTreeMap::new();
    for dependency in dependencies {
        let (dep_id, kind, version_range) = match dependency {
            DependencyRequirement::Exists { id, version_range } => {
                (id, CurrentDependencyKind::Exists, version_range)
            }
            DependencyRequirement::Running {
                id,
                health_checks,
                version_range,
            } => (
                id,
                CurrentDependencyKind::Running { health_checks },
                version_range,
            ),
        };
        let info = CurrentDependencyInfo {
            title: context
                .seed
                .persistent_container
                .s9pk
                .dependency_metadata(&dep_id)
                .await?
                .map(|m| m.title),
            icon: context
                .seed
                .persistent_container
                .s9pk
                .dependency_icon_data_url(&dep_id)
                .await?,
            kind,
            version_range,
        };
        deps.insert(dep_id, info);
    }
    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(id)
                .or_not_found(id)?
                .as_current_dependencies_mut()
                .ser(&CurrentDependencies(deps))
        })
        .await
        .result
}

pub async fn get_dependencies(context: EffectContext) -> Result<Vec<DependencyRequirement>, Error> {
    let context = context.deref()?;
    let id = &context.seed.id;
    let db = context.seed.ctx.db.peek().await;
    let data = db
        .as_public()
        .as_package_data()
        .as_idx(id)
        .or_not_found(id)?
        .as_current_dependencies()
        .de()?;

    Ok(data
        .0
        .into_iter()
        .map(|(id, current_dependency_info)| {
            let CurrentDependencyInfo {
                version_range,
                kind,
                ..
            } = current_dependency_info;
            match kind {
                CurrentDependencyKind::Exists => {
                    DependencyRequirement::Exists { id, version_range }
                }
                CurrentDependencyKind::Running { health_checks } => {
                    DependencyRequirement::Running {
                        id,
                        health_checks,
                        version_range,
                    }
                }
            }
        })
        .collect())
}

#[derive(Debug, Clone, Serialize, Deserialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CheckDependenciesParam {
    #[ts(optional)]
    package_ids: Option<Vec<PackageId>>,
}
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CheckDependenciesResult {
    package_id: PackageId,
    #[ts(type = "string | null")]
    title: Option<InternedString>,
    installed_version: Option<VersionString>,
    satisfies: BTreeSet<VersionString>,
    is_running: bool,
    tasks: BTreeMap<ReplayId, TaskEntry>,
    #[ts(as = "BTreeMap::<HealthCheckId, NamedHealthCheckResult>")]
    health_checks: OrdMap<HealthCheckId, NamedHealthCheckResult>,
}
pub async fn check_dependencies(
    context: EffectContext,
    CheckDependenciesParam { package_ids }: CheckDependenciesParam,
) -> Result<Vec<CheckDependenciesResult>, Error> {
    let context = context.deref()?;
    let db = context.seed.ctx.db.peek().await;
    let pde = db
        .as_public()
        .as_package_data()
        .as_idx(&context.seed.id)
        .or_not_found(&context.seed.id)?;
    let current_dependencies = pde.as_current_dependencies().de()?;
    let tasks = pde.as_tasks().de()?;
    let package_dependency_info: Vec<_> = package_ids
        .unwrap_or_else(|| current_dependencies.0.keys().cloned().collect())
        .into_iter()
        .filter_map(|x| {
            let info = current_dependencies.0.get(&x)?;
            Some((x, info))
        })
        .collect();
    let mut results = Vec::with_capacity(package_dependency_info.len());

    for (package_id, dependency_info) in package_dependency_info {
        let title = dependency_info.title.clone();
        let Some(package) = db.as_public().as_package_data().as_idx(&package_id) else {
            let tasks = tasks
                .iter()
                .filter(|(_, v)| v.task.package_id == package_id)
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            results.push(CheckDependenciesResult {
                package_id,
                title,
                installed_version: None,
                satisfies: BTreeSet::new(),
                is_running: false,
                tasks,
                health_checks: Default::default(),
            });
            continue;
        };
        let manifest = package.as_state_info().as_manifest(ManifestPreference::New);
        let installed_version = manifest.as_version().de()?.into_version();
        let satisfies = manifest.as_satisfies().de()?;
        let installed_version = Some(installed_version.clone().into());
        let is_installed = true;
        let status = package.as_status().de()?;
        let is_running = if is_installed {
            status.running()
        } else {
            false
        };
        let health_checks = status.health().cloned().unwrap_or_default();
        let tasks = tasks
            .iter()
            .filter(|(_, v)| v.task.package_id == package_id)
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        results.push(CheckDependenciesResult {
            package_id,
            title,
            installed_version,
            satisfies,
            is_running,
            tasks,
            health_checks,
        });
    }
    Ok(results)
}
