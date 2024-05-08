use std::collections::BTreeMap;

use emver::Version;
use imbl_value::InternedString;
use models::{DataUrl, PackageId};
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::prelude::*;
use crate::s9pk::git_hash::GitHash;
use crate::s9pk::manifest::Description;

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct PackageIndex {
    #[ts(as = "BTreeMap::<String, Category>")]
    pub categories: BTreeMap<InternedString, Category>,
    #[ts(as = "BTreeMap::<PackageId, BTreeMap::<String, PackageInfo>>")]
    pub packages: BTreeMap<PackageId, BTreeMap<Version, PackageInfo>>,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Category {
    pub name: String,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct PackageInfo {
    pub title: String,
    pub icon: DataUrl<'static>,
    pub description: Description,
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
}
