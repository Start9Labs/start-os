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
use crate::registry::signer::commitment::merkle_archive::MerkleArchiveCommitment;
use crate::rpc_continuations::Guid;
use crate::s9pk::git_hash::GitHash;
use crate::s9pk::manifest::{Description, HardwareRequirements};

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
    pub signers: BTreeSet<Guid>,
    pub versions: BTreeMap<VersionString, PackageVersionInfo>,
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
    #[ts(type = "string[]")]
    pub categories: BTreeSet<InternedString>,
    pub os_version: VersionString,
    pub hardware_requirements: HardwareRequirements,
    #[ts(type = "string | null")]
    pub source_version: Option<VersionRange>,
    pub s9pk: RegistryAsset<MerkleArchiveCommitment>,
}

pub async fn get_package_index(ctx: RegistryContext) -> Result<PackageIndex, Error> {
    ctx.db.peek().await.into_index().into_package().de()
}
