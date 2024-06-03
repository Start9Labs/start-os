use std::collections::{BTreeMap, BTreeSet};

use emver::{Version, VersionRange};
use imbl_value::InternedString;
use models::{DataUrl, PackageId, VersionString};
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::prelude::*;
use crate::registry::asset::RegistryAsset;
use crate::registry::context::RegistryContext;
use crate::registry::device_info::DeviceInfo;
use crate::registry::signer::commitment::merkle_archive::MerkleArchiveCommitment;
use crate::registry::signer::sign::{AnySignature, AnyVerifyingKey};
use crate::rpc_continuations::Guid;
use crate::s9pk::git_hash::GitHash;
use crate::s9pk::manifest::{Description, HardwareRequirements};
use crate::s9pk::merkle_archive::source::FileSource;
use crate::s9pk::S9pk;

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct PackageIndex {
    #[ts(as = "BTreeMap::<String, Category>")]
    pub categories: BTreeMap<InternedString, Category>,
    pub packages: BTreeMap<PackageId, PackageInfo>,
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct PackageInfo {
    pub authorized: BTreeSet<Guid>,
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
    pub description: Description,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct PackageVersionInfo {
    pub title: String,
    pub icon: DataUrl<'static>,
    pub description: Description,
    pub release_notes: String,
    #[ts(type = "string")]
    pub git_hash: GitHash,
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
    pub os_version: VersionString,
    pub hardware_requirements: HardwareRequirements,
    #[ts(type = "string | null")]
    pub source_version: Option<VersionRange>,
    pub s9pk: RegistryAsset<MerkleArchiveCommitment>,
}
impl PackageVersionInfo {
    pub async fn from_s9pk<S: FileSource + Clone>(s9pk: &S9pk<S>, url: Url) -> Result<Self, Error> {
        let manifest = s9pk.as_manifest();
        Ok(Self {
            title: manifest.title.clone(),
            icon: s9pk.icon_data_url().await?,
            description: manifest.description.clone(),
            release_notes: manifest.release_notes.clone(),
            git_hash: manifest.git_hash.clone().or_not_found("git hash")?,
            license: manifest.license.clone(),
            wrapper_repo: manifest.wrapper_repo.clone(),
            upstream_repo: manifest.upstream_repo.clone(),
            support_site: manifest.support_site.clone(),
            marketing_site: manifest.marketing_site.clone(),
            os_version: manifest.os_version.clone(),
            hardware_requirements: manifest.hardware_requirements.clone(),
            source_version: None, // TODO
            s9pk: RegistryAsset {
                url,
                commitment: s9pk.as_archive().commitment().await?,
                signatures: [(
                    AnyVerifyingKey::Ed25519(s9pk.as_archive().signer()),
                    AnySignature::Ed25519(s9pk.as_archive().signature().await?),
                )]
                .into_iter()
                .collect(),
            },
        })
    }
    pub fn table(&self, version: &VersionString) -> prettytable::Table {
        use prettytable::*;

        let mut table = Table::new();

        table.add_row(row![bc => &self.title]);
        table.add_row(row![br -> "VERSION", AsRef::<str>::as_ref(version)]);
        table.add_row(row![br -> "RELEASE NOTES", &self.release_notes]);
        table.add_row(row![br -> "ABOUT", &self.description.short]);
        table.add_row(row![br -> "DESCRIPTION", &self.description.long]);
        table.add_row(row![br -> "GIT HASH", AsRef::<str>::as_ref(&self.git_hash)]);
        table.add_row(row![br -> "LICENSE", &self.license]);
        table.add_row(row![br -> "PACKAGE REPO", &self.wrapper_repo.to_string()]);
        table.add_row(row![br -> "SERVICE REPO", &self.upstream_repo.to_string()]);
        table.add_row(row![br -> "WEBSITE", &self.marketing_site.to_string()]);
        table.add_row(row![br -> "SUPPORT", &self.support_site.to_string()]);

        table
    }
}
impl Model<PackageVersionInfo> {
    pub fn works_for_device(&self, device_info: &DeviceInfo) -> Result<bool, Error> {
        if !self.as_os_version().de()?.satisfies(&device_info.os.compat) {
            return Ok(false);
        }
        let hw = self.as_hardware_requirements().de()?;
        if let Some(arch) = hw.arch {
            if !arch.contains(&device_info.hardware.arch) {
                return Ok(false);
            }
        }
        if let Some(ram) = hw.ram {
            if device_info.hardware.ram < ram {
                return Ok(false);
            }
        }
        for (class, regex) in hw.device {
            if !device_info
                .hardware
                .devices
                .get(&*class)
                .unwrap_or(&Vec::new())
                .iter()
                .any(|product| regex.as_ref().is_match(product))
            {
                return Ok(false);
            }
        }

        Ok(true)
    }
}

pub async fn get_package_index(ctx: RegistryContext) -> Result<PackageIndex, Error> {
    ctx.db.peek().await.into_index().into_package().de()
}
