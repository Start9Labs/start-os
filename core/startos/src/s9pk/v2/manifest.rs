use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use color_eyre::eyre::eyre;
pub use models::PackageId;
use serde::{Deserialize, Serialize};
use url::Url;

use super::git_hash::GitHash;
use crate::action::Actions;
use crate::backup::BackupActions;
use crate::config::action::ConfigActions;
use crate::dependencies::Dependencies;
use crate::migration::Migrations;
use crate::net::interface::Interfaces;
use crate::prelude::*;
use crate::procedure::PackageProcedure;
use crate::status::health_check::HealthChecks;
use crate::util::serde::Regex;
use crate::util::Version;
use crate::version::{Current, VersionT};
use crate::volume::Volumes;

fn current_version() -> Version {
    Current::new().semver().into()
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct Manifest {
    pub id: PackageId,
    pub title: String,
    pub version: Version,
    pub release_notes: String,
    pub license: String, // type of license
    #[serde(default)]
    pub replaces: Vec<String>,
    pub wrapper_repo: Url,
    pub upstream_repo: Url,
    pub support_site: Url,
    pub marketing_site: Url,
    pub donation_url: Option<Url>,
    pub description: Description,
    pub images: Vec<ImageId>,
    pub assets: Vec<VolumeId>, // TODO: AssetsId
    pub volumes: Vec<VolumeId>,
    #[serde(default)]
    pub alerts: Alerts,
    #[serde(default)]
    pub dependencies: Dependencies,
    #[serde(default)]
    pub hardware_requirements: HardwareRequirements,
    #[serde(default)]
    pub git_hash: Option<GitHash>,
    #[serde(default = "current_version")]
    pub os_version: Version,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct HardwareRequirements {
    #[serde(default)]
    device: BTreeMap<String, Regex>,
    ram: Option<u64>,
    pub arch: Option<Vec<String>>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Description {
    pub short: String,
    pub long: String,
}
impl Description {
    pub fn validate(&self) -> Result<(), Error> {
        if self.short.chars().skip(160).next().is_some() {
            return Err(Error::new(
                eyre!("Short description must be 160 characters or less."),
                crate::ErrorKind::ValidateS9pk,
            ));
        }
        if self.long.chars().skip(5000).next().is_some() {
            return Err(Error::new(
                eyre!("Long description must be 5000 characters or less."),
                crate::ErrorKind::ValidateS9pk,
            ));
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Alerts {
    pub install: Option<String>,
    pub uninstall: Option<String>,
    pub restore: Option<String>,
    pub start: Option<String>,
    pub stop: Option<String>,
}
