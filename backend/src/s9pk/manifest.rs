use std::path::{Path, PathBuf};

use color_eyre::eyre::eyre;
pub use models::{PackageId, SYSTEM_PACKAGE_ID};
use patch_db::HasModel;
use serde::{Deserialize, Serialize};
use url::Url;

use super::git_hash::GitHash;
use crate::container::DockerContainers;
use crate::dependencies::Dependencies;
use crate::net::interface::Interfaces;
use crate::prelude::*;
use crate::status::health_check::HealthChecks;
use crate::util::Version;
use crate::version::{Current, VersionT};
use crate::volume::Volumes;

fn current_version() -> Version {
    Current::new().semver().into()
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
// #[macro_debug]
pub struct Manifest {
    #[serde(default = "current_version")]
    pub os_version: Version,
    pub id: PackageId,
    #[serde(default)]
    pub git_hash: Option<GitHash>,
    pub title: String,
    pub version: Version,
    pub description: Description,
    #[serde(default)]
    pub assets: Assets,
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
    pub containers: DockerContainers,
    #[serde(default)]
    pub replaces: Vec<String>,
}

impl Manifest {
    pub fn with_git_hash(mut self, git_hash: GitHash) -> Self {
        self.git_hash = Some(git_hash);
        self
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, PartialEq, Eq)]
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

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct Description {
    pub short: String,
    pub long: String,
}
impl Description {
    pub fn validate(&self) -> Result<(), Error> {
        if self.short.chars().skip(160).next().is_some() {
            return Err(Error::new(
                eyre!("Short description must be 160 characters or less."),
                ErrorKind::ValidateS9pk,
            ));
        }
        if self.long.chars().skip(5000).next().is_some() {
            return Err(Error::new(
                eyre!("Long description must be 5000 characters or less."),
                ErrorKind::ValidateS9pk,
            ));
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub struct Alerts {
    pub install: Option<String>,
    pub uninstall: Option<String>,
    pub restore: Option<String>,
    pub start: Option<String>,
    pub stop: Option<String>,
}
