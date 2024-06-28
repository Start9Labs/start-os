use std::collections::{BTreeMap, BTreeSet};

use exver::VersionRange;
use imbl_value::InternedString;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::prelude::*;
use crate::registry::asset::RegistryAsset;
use crate::registry::context::RegistryContext;
use crate::registry::signer::commitment::blake3::Blake3Commitment;
use crate::rpc_continuations::Guid;
use crate::util::VersionString;

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct OsIndex {
    pub versions: BTreeMap<VersionString, OsVersionInfo>,
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
    pub authorized: BTreeSet<Guid>,
    #[ts(as = "BTreeMap::<String, RegistryAsset::<Blake3Commitment>>")]
    pub iso: BTreeMap<InternedString, RegistryAsset<Blake3Commitment>>, // platform (i.e. x86_64-nonfree) -> asset
    #[ts(as = "BTreeMap::<String, RegistryAsset::<Blake3Commitment>>")]
    pub squashfs: BTreeMap<InternedString, RegistryAsset<Blake3Commitment>>, // platform (i.e. x86_64-nonfree) -> asset
    #[ts(as = "BTreeMap::<String, RegistryAsset::<Blake3Commitment>>")]
    pub img: BTreeMap<InternedString, RegistryAsset<Blake3Commitment>>, // platform (i.e. raspberrypi) -> asset
}

pub async fn get_os_index(ctx: RegistryContext) -> Result<OsIndex, Error> {
    ctx.db.peek().await.into_index().into_os().de()
}
