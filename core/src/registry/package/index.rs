use std::collections::{BTreeMap, BTreeSet};

use chrono::Utc;
use exver::{Version, VersionRange};
use imbl_value::InternedString;
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
use crate::s9pk::manifest::{Alerts, Description, HardwareRequirements};
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
    pub name: String,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct DependencyMetadata {
    #[ts(type = "string | null")]
    pub title: Option<InternedString>,
    pub icon: Option<DataUrl<'static>>,
    pub description: Option<String>,
    pub optional: bool,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct PackageMetadata {
    #[ts(type = "string")]
    pub title: InternedString,
    pub icon: DataUrl<'static>,
    pub description: Description,
    pub release_notes: String,
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
    pub async fn from_s9pk<S: FileSource + Clone>(s9pk: &S9pk<S>, url: Url) -> Result<Self, Error> {
        Ok(Self {
            metadata: PackageMetadata::load(s9pk).await?,
            source_version: None, // TODO
            s9pks: vec![(
                s9pk.as_manifest().hardware_requirements.clone(),
                RegistryAsset {
                    published_at: Utc::now(),
                    urls: vec![url],
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
    pub fn merge_with(&mut self, other: Self) -> Result<(), Error> {
        for (hw_req, asset) in other.s9pks {
            if let Some((_, matching)) = self
                .s9pks
                .iter_mut()
                .find(|(h, s)| s.commitment == asset.commitment && *h == hw_req)
            {
                for url in asset.urls {
                    if matching.urls.contains(&url) {
                        continue;
                    }
                    matching.urls.push(url);
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
    pub fn table(&self, version: &VersionString) -> prettytable::Table {
        use prettytable::*;

        let mut table = Table::new();

        table.add_row(row![bc => &self.metadata.title]);
        table.add_row(row![br -> "VERSION", AsRef::<str>::as_ref(version)]);
        table.add_row(row![br -> "RELEASE NOTES", &self.metadata.release_notes]);
        table.add_row(
            row![br -> "ABOUT", &textwrap::wrap(&self.metadata.description.short, 80).join("\n")],
        );
        table.add_row(row![
            br -> "DESCRIPTION",
            &textwrap::wrap(&self.metadata.description.long, 80).join("\n")
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
    pub fn for_device(
        &self,
        device_info: &DeviceInfo,
    ) -> Result<Option<Vec<(HardwareRequirements, RegistryAsset<MerkleArchiveCommitment>)>>, Error>
    {
        if !self
            .as_metadata()
            .as_os_version()
            .de()?
            .satisfies(&device_info.os.compat)
        {
            return Ok(None);
        }
        let mut s9pk = self.as_s9pks().de()?;
        s9pk.retain(|(hw, _)| {
            if let Some(arch) = &hw.arch {
                if !arch.contains(&device_info.hardware.arch) {
                    return false;
                }
            }
            if let Some(ram) = hw.ram {
                if device_info.hardware.ram < ram {
                    return false;
                }
            }
            for device_filter in &hw.device {
                if !device_info
                    .hardware
                    .devices
                    .iter()
                    .filter(|d| d.class() == &*device_filter.class)
                    .any(|d| device_filter.pattern.as_ref().is_match(d.product()))
                {
                    return false;
                }
            }
            true
        });

        if s9pk.is_empty() {
            Ok(None)
        } else {
            Ok(Some(s9pk))
        }
    }
}

pub async fn get_package_index(ctx: RegistryContext) -> Result<PackageIndex, Error> {
    ctx.db.peek().await.into_index().into_package().de()
}
