use std::collections::{BTreeMap, BTreeSet};

use emver::VersionRange;
use imbl_value::InternedString;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::prelude::*;
use crate::registry::asset::RegistryAsset;
use crate::registry::context::RegistryContext;
use crate::rpc_continuations::RequestGuid;
use crate::util::Version;

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct OsIndex {
    #[ts(as = "BTreeMap::<String, OsVersionInfo>")]
    pub versions: BTreeMap<Version, OsVersionInfo>,
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct OsVersionInfo {
    pub headline: String,
    pub release_notes: String,
    #[ts(type = "string")]
    pub source_version: VersionRange,
    #[ts(type = "string[]")]
    pub signers: BTreeSet<RequestGuid>,
    #[ts(as = "BTreeMap::<String, RegistryAsset>")]
    pub iso: BTreeMap<InternedString, RegistryAsset>, // platform (i.e. x86_64-nonfree) -> asset
    #[ts(as = "BTreeMap::<String, RegistryAsset>")]
    pub squashfs: BTreeMap<InternedString, RegistryAsset>, // platform (i.e. x86_64-nonfree) -> asset
    #[ts(as = "BTreeMap::<String, RegistryAsset>")]
    pub img: BTreeMap<InternedString, RegistryAsset>, // platform (i.e. raspberrypi) -> asset
}

pub async fn get_os_index(ctx: RegistryContext) -> Result<OsIndex, Error> {
    ctx.db.peek().await.into_index().into_os().de()
}
