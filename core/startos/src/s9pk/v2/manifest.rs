use std::collections::BTreeMap;

use color_eyre::eyre::eyre;
use helpers::const_true;
pub use models::PackageId;
use models::{ImageId, VolumeId};
use serde::{Deserialize, Serialize};
use url::Url;

use crate::dependencies::Dependencies;
use crate::prelude::*;
use crate::s9pk::v1::git_hash::GitHash;
use crate::util::serde::Regex;
use crate::util::Version;
use crate::version::{Current, VersionT};

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
    #[serde(default = "const_true")]
    pub has_config: bool,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
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
#[serde(rename_all = "camelCase")]
pub struct Alerts {
    pub install: Option<String>,
    pub uninstall: Option<String>,
    pub restore: Option<String>,
    pub start: Option<String>,
    pub stop: Option<String>,
}
