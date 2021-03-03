use std::path::PathBuf;

use linear_map::LinearMap;

use crate::actions::Action;
use crate::dependencies::Dependencies;
use crate::tor::HiddenServiceVersion;
use crate::tor::PortMapping;

pub type ManifestLatest = ManifestV0;

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Description {
    pub short: String,
    pub long: String,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum ImageConfig {
    Tar,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Asset {
    pub src: PathBuf,
    pub dst: PathBuf,
    pub overwrite: bool,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct ManifestV0 {
    pub id: String,
    pub version: emver::Version,
    pub title: String,
    pub description: Description,
    pub release_notes: String,
    #[serde(default)]
    pub install_alert: Option<String>,
    #[serde(default)]
    pub uninstall_alert: Option<String>,
    #[serde(default)]
    pub restore_alert: Option<String>,
    #[serde(default)]
    pub start_alert: Option<String>,
    #[serde(default)]
    pub has_instructions: bool,
    #[serde(default = "emver::VersionRange::any")]
    pub os_version_required: emver::VersionRange,
    #[serde(default = "emver::VersionRange::any")]
    pub os_version_recommended: emver::VersionRange,
    pub ports: Vec<PortMapping>,
    pub image: ImageConfig,
    #[serde(default)]
    pub shm_size_mb: Option<usize>,
    pub mount: PathBuf,
    #[serde(default)]
    pub public: Option<PathBuf>,
    #[serde(default)]
    pub shared: Option<PathBuf>,
    #[serde(default)]
    pub assets: Vec<Asset>,
    #[serde(default)]
    pub hidden_service_version: HiddenServiceVersion,
    #[serde(default)]
    pub dependencies: Dependencies,
    #[serde(default)]
    pub actions: Vec<Action>,
    #[serde(flatten)]
    pub extra: LinearMap<String, serde_yaml::Value>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
#[serde(tag = "compat")]
#[serde(rename_all = "lowercase")]
pub enum Manifest {
    V0(ManifestV0),
}
impl Manifest {
    pub fn into_latest(self) -> ManifestLatest {
        match self {
            Manifest::V0(m) => m,
        }
    }
}
