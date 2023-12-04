use std::path::{Path, PathBuf};

use imbl_value::InOMap;
pub use models::PackageId;
use serde::{Deserialize, Serialize};
use url::Url;

use super::git_hash::GitHash;
use crate::dependencies::Dependencies;
use crate::prelude::*;
use crate::s9pk::manifest::{Alerts, Description, HardwareRequirements};
use crate::util::Version;
use crate::version::{Current, VersionT};
use crate::volume::Volumes;

fn current_version() -> Version {
    Current::new().semver().into()
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct Manifest {
    #[serde(default = "current_version")]
    pub eos_version: Version,
    pub id: PackageId,
    #[serde(default)]
    pub git_hash: Option<GitHash>,
    #[serde(default)]
    pub assets: Assets,
    pub title: String,
    pub version: Version,
    pub description: Description,
    pub release_notes: String,
    pub license: String, // type of license
    pub wrapper_repo: Url,
    pub upstream_repo: Url,
    pub support_site: Option<Url>,
    pub marketing_site: Option<Url>,
    pub donation_url: Option<Url>,
    #[serde(default)]
    pub alerts: Alerts,
    pub volumes: Volumes,
    #[serde(default)]
    pub dependencies: Dependencies,
    pub config: Option<InOMap<String, Value>>,

    #[serde(default)]
    pub replaces: Vec<String>,

    #[serde(default)]
    pub hardware_requirements: HardwareRequirements,
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
