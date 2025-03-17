use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use color_eyre::eyre::eyre;
use exver::{Version, VersionRange};
use imbl_value::InternedString;
pub use models::PackageId;
use models::{mime, ImageId, VolumeId};
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::dependencies::Dependencies;
use crate::prelude::*;
use crate::s9pk::git_hash::GitHash;
use crate::s9pk::merkle_archive::directory_contents::DirectoryContents;
use crate::s9pk::merkle_archive::expected::{Expected, Filter};
use crate::s9pk::v2::pack::ImageConfig;
use crate::util::serde::Regex;
use crate::util::VersionString;
use crate::version::{Current, VersionT};

fn current_version() -> Version {
    Current::default().semver()
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Manifest {
    pub id: PackageId,
    #[ts(type = "string")]
    pub title: InternedString,
    pub version: VersionString,
    pub satisfies: BTreeSet<VersionString>,
    pub release_notes: String,
    #[ts(type = "string")]
    pub can_migrate_to: VersionRange,
    #[ts(type = "string")]
    pub can_migrate_from: VersionRange,
    #[ts(type = "string")]
    pub license: InternedString, // type of license
    #[ts(type = "string")]
    pub wrapper_repo: Url,
    #[ts(type = "string")]
    pub upstream_repo: Url,
    #[ts(type = "string")]
    pub support_site: Url,
    #[ts(type = "string")]
    pub marketing_site: Url,
    #[ts(type = "string | null")]
    pub donation_url: Option<Url>,
    pub description: Description,
    pub images: BTreeMap<ImageId, ImageConfig>,
    pub volumes: BTreeSet<VolumeId>,
    #[serde(default)]
    pub alerts: Alerts,
    #[serde(default)]
    pub dependencies: Dependencies,
    #[serde(default)]
    pub hardware_requirements: HardwareRequirements,
    #[ts(optional)]
    #[serde(default = "GitHash::load_sync")]
    pub git_hash: Option<GitHash>,
    #[serde(default = "current_version")]
    #[ts(type = "string")]
    pub os_version: Version,
}
impl Manifest {
    pub fn validate_for<'a, T: Clone>(
        &self,
        arch: Option<&str>,
        archive: &'a DirectoryContents<T>,
    ) -> Result<Filter, Error> {
        let mut expected = Expected::new(archive);
        expected.check_file("manifest.json")?;
        expected.check_stem("icon", |ext| {
            ext.and_then(|e| e.to_str())
                .and_then(mime)
                .map_or(false, |mime| mime.starts_with("image/"))
        })?;
        expected.check_file("LICENSE.md")?;
        expected.check_file("instructions.md")?;
        expected.check_file("javascript.squashfs")?;
        for (dependency, _) in &self.dependencies.0 {
            let dep_path = Path::new("dependencies").join(dependency);
            let _ = expected.check_file(dep_path.join("metadata.json"));
            let _ = expected.check_stem(dep_path.join("icon"), |ext| {
                ext.and_then(|e| e.to_str())
                    .and_then(mime)
                    .map_or(false, |mime| mime.starts_with("image/"))
            });
        }
        if let Err(e) = expected.check_file(Path::new("assets.squashfs")) {
            // backwards compatibility for alpha s9pks - remove eventually
            if expected.check_dir("assets").is_err() {
                return Err(e);
            }
        }
        for (image_id, config) in &self.images {
            let mut check_arch = |arch: &str| {
                let mut arch = arch;
                if let Err(e) = expected.check_file(
                    Path::new("images")
                        .join(arch)
                        .join(image_id)
                        .with_extension("squashfs"),
                ) {
                    if let Some(emulate_as) = &config.emulate_missing_as {
                        expected.check_file(
                            Path::new("images")
                                .join(arch)
                                .join(image_id)
                                .with_extension("squashfs"),
                        )?;
                        arch = &**emulate_as;
                    } else {
                        return Err(e);
                    }
                }
                expected.check_file(
                    Path::new("images")
                        .join(arch)
                        .join(image_id)
                        .with_extension("json"),
                )?;
                expected.check_file(
                    Path::new("images")
                        .join(arch)
                        .join(image_id)
                        .with_extension("env"),
                )?;
                Ok(())
            };
            if let Some(arch) = arch {
                check_arch(arch)?;
            } else if let Some(arches) = &self.hardware_requirements.arch {
                for arch in arches {
                    check_arch(arch)?;
                }
            } else if let Some(arch) = config.emulate_missing_as.as_deref() {
                if !config.arch.contains(arch) {
                    return Err(Error::new(
                        eyre!("`emulateMissingAs` must match an included `arch`"),
                        ErrorKind::ParseS9pk,
                    ));
                }
                for arch in &config.arch {
                    check_arch(&arch)?;
                }
            } else {
                return Err(Error::new(eyre!("`emulateMissingAs` required for all images if no `arch` specified in `hardwareRequirements`"), ErrorKind::ParseS9pk));
            }
        }
        Ok(expected.into_filter())
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HardwareRequirements {
    #[serde(default)]
    pub device: Vec<DeviceFilter>,
    #[ts(type = "number | null")]
    pub ram: Option<u64>,
    #[ts(type = "string[] | null")]
    pub arch: Option<BTreeSet<InternedString>>,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DeviceFilter {
    #[ts(type = "\"processor\" | \"display\"")]
    pub class: InternedString,
    #[ts(type = "string")]
    pub pattern: Regex,
    pub pattern_description: String,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[ts(export)]
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

#[derive(Clone, Debug, Default, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct Alerts {
    pub install: Option<String>,
    pub uninstall: Option<String>,
    pub restore: Option<String>,
    pub start: Option<String>,
    pub stop: Option<String>,
}
