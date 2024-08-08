use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use exver::{Version, VersionRange};
use indexmap::IndexMap;
pub use models::PackageId;
use models::{ActionId, HealthCheckId, ImageId, VolumeId};
use serde::{Deserialize, Serialize};
use url::Url;

use crate::prelude::*;
use crate::s9pk::git_hash::GitHash;
use crate::s9pk::manifest::{Alerts, Description, HardwareRequirements};
use crate::util::serde::{Duration, IoFormat};

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Manifest {
    pub eos_version: Version,
    pub id: PackageId,
    #[serde(default)]
    pub git_hash: Option<GitHash>,
    pub title: String,
    pub version: exver::emver::Version,
    pub description: Description,
    #[serde(default)]
    pub assets: Assets,
    #[serde(default)]
    pub build: Option<Vec<String>>,
    pub release_notes: String,
    pub license: String, // type of license
    pub wrapper_repo: Url,
    pub upstream_repo: Url,
    pub support_site: Option<Url>,
    pub marketing_site: Option<Url>,
    pub donation_url: Option<Url>,
    #[serde(default)]
    pub alerts: Alerts,
    pub main: PackageProcedure,
    pub health_checks: HealthChecks,
    pub config: Option<ConfigActions>,
    pub properties: Option<PackageProcedure>,
    pub volumes: BTreeMap<VolumeId, Value>,
    // #[serde(default)]
    // pub interfaces: Interfaces,
    // #[serde(default)]
    pub backup: BackupActions,
    #[serde(default)]
    pub migrations: Migrations,
    #[serde(default)]
    pub actions: BTreeMap<ActionId, Action>,
    // #[serde(default)]
    // pub permissions: Permissions,
    #[serde(default)]
    pub dependencies: BTreeMap<PackageId, DepInfo>,

    #[serde(default)]
    pub replaces: Vec<String>,

    #[serde(default)]
    pub hardware_requirements: HardwareRequirements,
}

impl Manifest {
    pub fn package_procedures(&self) -> impl Iterator<Item = &PackageProcedure> {
        use std::iter::once;
        let main = once(&self.main);
        let cfg_get = self.config.as_ref().map(|a| &a.get).into_iter();
        let cfg_set = self.config.as_ref().map(|a| &a.set).into_iter();
        let props = self.properties.iter();
        let backups = vec![&self.backup.create, &self.backup.restore].into_iter();
        let migrations = self
            .migrations
            .to
            .values()
            .chain(self.migrations.from.values());
        let actions = self.actions.values().map(|a| &a.implementation);
        main.chain(cfg_get)
            .chain(cfg_set)
            .chain(props)
            .chain(backups)
            .chain(migrations)
            .chain(actions)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "type")]
#[model = "Model<Self>"]
pub enum PackageProcedure {
    Docker(DockerProcedure),
    Script(Value),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct DockerProcedure {
    pub image: ImageId,
    #[serde(default)]
    pub system: bool,
    pub entrypoint: String,
    #[serde(default)]
    pub args: Vec<String>,
    #[serde(default)]
    pub inject: bool,
    #[serde(default)]
    pub mounts: BTreeMap<VolumeId, PathBuf>,
    #[serde(default)]
    pub io_format: Option<IoFormat>,
    #[serde(default)]
    pub sigterm_timeout: Option<Duration>,
    #[serde(default)]
    pub shm_size_mb: Option<usize>, // TODO: use postfix sizing? like 1k vs 1m vs 1g
    #[serde(default)]
    pub gpu_acceleration: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct HealthChecks(pub BTreeMap<HealthCheckId, HealthCheck>);

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct HealthCheck {
    pub name: String,
    pub success_message: Option<String>,
    #[serde(flatten)]
    implementation: PackageProcedure,
    pub timeout: Option<Duration>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ConfigActions {
    pub get: PackageProcedure,
    pub set: PackageProcedure,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackupActions {
    pub create: PackageProcedure,
    pub restore: PackageProcedure,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Migrations {
    pub from: IndexMap<VersionRange, PackageProcedure>,
    pub to: IndexMap<VersionRange, PackageProcedure>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Action {
    pub name: String,
    pub description: String,
    #[serde(default)]
    pub warning: Option<String>,
    pub implementation: PackageProcedure,
    // pub allowed_statuses: Vec<DockerStatus>,
    // #[serde(default)]
    // pub input_spec: ConfigSpec,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct DepInfo {
    pub version: VersionRange,
    pub requirement: DependencyRequirement,
    pub description: Option<String>,
    #[serde(default)]
    pub config: Option<DependencyConfig>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct DependencyConfig {
    check: PackageProcedure,
    auto_configure: PackageProcedure,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "type")]
pub enum DependencyRequirement {
    OptIn { how: String },
    OptOut { how: String },
    Required,
}
impl DependencyRequirement {
    pub fn required(&self) -> bool {
        matches!(self, &DependencyRequirement::Required)
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Assets {
    #[serde(default)]
    pub license: Option<PathBuf>,
    #[serde(default)]
    pub instructions: Option<PathBuf>,
    #[serde(default)]
    pub icon: Option<PathBuf>,
    #[serde(default)]
    pub docker_images: Option<PathBuf>,
    #[serde(default)]
    pub assets: Option<PathBuf>,
    #[serde(default)]
    pub scripts: Option<PathBuf>,
}
impl Assets {
    pub fn license_path(&self) -> &Path {
        self.license
            .as_ref()
            .map(|a| a.as_path())
            .unwrap_or(Path::new("LICENSE.md"))
    }
    pub fn instructions_path(&self) -> &Path {
        self.instructions
            .as_ref()
            .map(|a| a.as_path())
            .unwrap_or(Path::new("INSTRUCTIONS.md"))
    }
    pub fn icon_path(&self) -> &Path {
        self.icon
            .as_ref()
            .map(|a| a.as_path())
            .unwrap_or(Path::new("icon.png"))
    }
    pub fn icon_type(&self) -> &str {
        self.icon
            .as_ref()
            .and_then(|icon| icon.extension())
            .and_then(|ext| ext.to_str())
            .unwrap_or("png")
    }
    pub fn docker_images_path(&self) -> &Path {
        self.docker_images
            .as_ref()
            .map(|a| a.as_path())
            .unwrap_or(Path::new("docker-images"))
    }
    pub fn assets_path(&self) -> &Path {
        self.assets
            .as_ref()
            .map(|a| a.as_path())
            .unwrap_or(Path::new("assets"))
    }
    pub fn scripts_path(&self) -> &Path {
        self.scripts
            .as_ref()
            .map(|a| a.as_path())
            .unwrap_or(Path::new("scripts"))
    }
}
