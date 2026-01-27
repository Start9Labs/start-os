use std::collections::{BTreeMap, BTreeSet};
use std::u32;

use chrono::Utc;
use exver::{Version, VersionRange};
use imbl_value::InternedString;
use patch_db::ModelExt;
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::PackageId;
use crate::prelude::*;
use crate::registry::asset::RegistryAsset;
use crate::registry::context::RegistryContext;
use crate::registry::device_info::DeviceInfo;
use crate::rpc_continuations::Guid;
use crate::s9pk::S9pk;
use crate::s9pk::git_hash::GitHash;
use crate::s9pk::manifest::{Alerts, Description, HardwareRequirements, LocaleString};
use crate::s9pk::merkle_archive::source::FileSource;
use crate::sign::commitment::merkle_archive::MerkleArchiveCommitment;
use crate::sign::{AnySignature, AnyVerifyingKey};
use crate::util::{DataUrl, VersionString};

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct PackageIndex {
    pub categories: BTreeMap<InternedString, Category>,
    pub packages: BTreeMap<PackageId, PackageInfo>,
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct PackageInfo {
    #[ts(as = "BTreeMap::<Guid, String>")]
    pub authorized: BTreeMap<Guid, VersionRange>,
    pub versions: BTreeMap<VersionString, PackageVersionInfo>,
    #[ts(type = "string[]")]
    pub categories: BTreeSet<InternedString>,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Category {
    pub name: LocaleString,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS, PartialEq)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct DependencyMetadata {
    pub title: Option<LocaleString>,
    pub icon: Option<DataUrl<'static>>,
    pub description: Option<LocaleString>,
    pub optional: bool,
}
impl DependencyMetadata {
    pub fn localize_for(&mut self, locale: &str) {
        self.title.as_mut().map(|t| t.localize_for(locale));
        self.description.as_mut().map(|d| d.localize_for(locale));
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS, PartialEq)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct PackageMetadata {
    #[ts(type = "string")]
    pub title: InternedString,
    pub icon: DataUrl<'static>,
    pub description: Description,
    pub release_notes: LocaleString,
    pub git_hash: Option<GitHash>,
    #[ts(type = "string")]
    pub license: InternedString,
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
    #[ts(type = "string | null")]
    pub docs_url: Option<Url>,
    pub alerts: Alerts,
    pub dependency_metadata: BTreeMap<PackageId, DependencyMetadata>,
    #[ts(type = "string")]
    pub os_version: Version,
    #[ts(type = "string | null")]
    pub sdk_version: Option<Version>,
    #[serde(default)]
    pub hardware_acceleration: bool,
}
impl PackageMetadata {
    pub async fn load<S: FileSource + Clone>(s9pk: &S9pk<S>) -> Result<Self, Error> {
        let manifest = s9pk.as_manifest();
        let mut dependency_metadata = BTreeMap::new();
        for (id, info) in &manifest.dependencies.0 {
            let metadata = s9pk.dependency_metadata(id).await?;
            dependency_metadata.insert(
                id.clone(),
                DependencyMetadata {
                    title: metadata.map(|m| m.title),
                    icon: s9pk.dependency_icon_data_url(id).await?,
                    description: info.description.clone(),
                    optional: info.optional,
                },
            );
        }
        Ok(Self {
            title: manifest.title.clone(),
            icon: s9pk.icon_data_url().await?,
            description: manifest.description.clone(),
            release_notes: manifest.release_notes.clone(),
            git_hash: manifest.git_hash.clone(),
            license: manifest.license.clone(),
            wrapper_repo: manifest.wrapper_repo.clone(),
            upstream_repo: manifest.upstream_repo.clone(),
            support_site: manifest.support_site.clone(),
            marketing_site: manifest.marketing_site.clone(),
            donation_url: manifest.donation_url.clone(),
            docs_url: manifest.docs_url.clone(),
            alerts: manifest.alerts.clone(),
            dependency_metadata,
            os_version: manifest.os_version.clone(),
            sdk_version: manifest.sdk_version.clone(),
            hardware_acceleration: manifest.hardware_acceleration.clone(),
        })
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct PackageVersionInfo {
    #[serde(flatten)]
    pub metadata: PackageMetadata,
    #[ts(type = "string | null")]
    pub source_version: Option<VersionRange>,
    pub s9pks: Vec<(HardwareRequirements, RegistryAsset<MerkleArchiveCommitment>)>,
}
impl PackageVersionInfo {
    pub async fn from_s9pk<S: FileSource + Clone>(
        s9pk: &S9pk<S>,
        urls: Vec<Url>,
    ) -> Result<Self, Error> {
        Ok(Self {
            metadata: PackageMetadata::load(s9pk).await?,
            source_version: None, // TODO
            s9pks: vec![(
                s9pk.as_manifest().hardware_requirements.clone(),
                RegistryAsset {
                    published_at: Utc::now(),
                    urls,
                    commitment: s9pk.as_archive().commitment().await?,
                    signatures: [(
                        AnyVerifyingKey::Ed25519(s9pk.as_archive().signer()),
                        AnySignature::Ed25519(s9pk.as_archive().signature().await?),
                    )]
                    .into_iter()
                    .collect(),
                },
            )],
        })
    }
    pub fn merge_with(&mut self, other: Self, replace_urls: bool) -> Result<(), Error> {
        for (hw_req, asset) in other.s9pks {
            if let Some((_, matching)) = self
                .s9pks
                .iter_mut()
                .find(|(h, s)| s.commitment == asset.commitment && *h == hw_req)
            {
                if replace_urls {
                    matching.urls = asset.urls;
                } else {
                    for url in asset.urls {
                        if matching.urls.contains(&url) {
                            continue;
                        }
                        matching.urls.push(url);
                    }
                }
            } else {
                if let Some((h, matching)) = self.s9pks.iter_mut().find(|(h, _)| *h == hw_req) {
                    *matching = asset;
                    *h = hw_req;
                } else {
                    self.s9pks.push((hw_req, asset));
                }
            }
        }
        self.s9pks.sort_by_key(|(h, _)| h.specificity_desc());
        Ok(())
    }
    pub fn table(self, version: &VersionString) -> prettytable::Table {
        use prettytable::*;

        let mut table = Table::new();

        table.add_row(row![bc => &self.metadata.title]);
        table.add_row(row![br -> "VERSION", AsRef::<str>::as_ref(version)]);
        table.add_row(row![br -> "RELEASE NOTES", &self.metadata.release_notes.localized()]);
        table.add_row(
            row![br -> "ABOUT", &textwrap::wrap(&self.metadata.description.short.localized(), 80).join("\n")],
        );
        table.add_row(row![
            br -> "DESCRIPTION",
            &textwrap::wrap(&self.metadata.description.long.localized(), 80).join("\n")
        ]);
        table.add_row(row![br -> "GIT HASH", self.metadata.git_hash.as_deref().unwrap_or("N/A")]);
        table.add_row(row![br -> "LICENSE", &self.metadata.license]);
        table.add_row(row![br -> "PACKAGE REPO", &self.metadata.wrapper_repo.to_string()]);
        table.add_row(row![br -> "SERVICE REPO", &self.metadata.upstream_repo.to_string()]);
        table.add_row(row![br -> "WEBSITE", &self.metadata.marketing_site.to_string()]);
        table.add_row(row![br -> "SUPPORT", &self.metadata.support_site.to_string()]);

        table
    }
}
impl Model<PackageVersionInfo> {
    /// Filters this package version for compatibility with the given device.
    /// Returns false if the package is incompatible (should be removed).
    /// Modifies s9pks in place to only include compatible variants.
    pub fn for_device(&mut self, device_info: &DeviceInfo) -> Result<bool, Error> {
        if !self
            .as_metadata()
            .as_os_version()
            .de()?
            .satisfies(&device_info.os.compat)
        {
            return Ok(false);
        }
        if let Some(hw) = &device_info.hardware {
            self.as_s9pks_mut().mutate(|s9pks| {
                s9pks.retain(|(hw_req, _)| {
                    if let Some(arch) = &hw_req.arch {
                        if !arch.contains(&hw.arch) {
                            return false;
                        }
                    }
                    if let Some(ram) = hw_req.ram {
                        if hw.ram < ram {
                            return false;
                        }
                    }
                    if let Some(dev) = &hw.devices {
                        for device_filter in &hw_req.device {
                            if !dev
                                .iter()
                                .filter(|d| d.class() == &*device_filter.class)
                                .any(|d| device_filter.matches(d))
                            {
                                return false;
                            }
                        }
                    }
                    true
                });
                if hw.devices.is_some() {
                    s9pks.sort_by_key(|(req, _)| req.specificity_desc());
                } else {
                    s9pks.sort_by_key(|(req, _)| {
                        let (dev, arch, ram) = req.specificity_desc();
                        (u32::MAX - dev, arch, ram)
                    });
                }
                Ok(())
            })?;

            if ModelExt::as_value(self.as_s9pks())
                .as_array()
                .map_or(true, |s| s.is_empty())
            {
                return Ok(false);
            }

            if let Some(locale) = device_info.os.language.as_deref() {
                let metadata = self.as_metadata_mut();
                metadata
                    .as_alerts_mut()
                    .mutate(|a| Ok(a.localize_for(locale)))?;
                metadata
                    .as_dependency_metadata_mut()
                    .as_entries_mut()?
                    .into_iter()
                    .try_for_each(|(_, d)| d.mutate(|d| Ok(d.localize_for(locale))))?;
                metadata
                    .as_description_mut()
                    .mutate(|d| Ok(d.localize_for(locale)))?;
                metadata
                    .as_release_notes_mut()
                    .mutate(|r| Ok(r.localize_for(locale)))?;
            }
        }

        Ok(true)
    }
}

pub async fn get_package_index(ctx: RegistryContext) -> Result<PackageIndex, Error> {
    ctx.db.peek().await.into_index().into_package().de()
}
