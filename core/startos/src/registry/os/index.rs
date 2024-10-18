use std::collections::{BTreeMap, BTreeSet};

use exver::{Version, VersionRange};
use imbl_value::InternedString;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::prelude::*;
use crate::registry::asset::RegistryAsset;
use crate::registry::context::RegistryContext;
use crate::registry::signer::commitment::blake3::Blake3Commitment;
use crate::rpc_continuations::Guid;

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct OsIndex {
    pub versions: OsVersionInfoMap,
}

#[derive(Debug, Default, Deserialize, Serialize, TS)]
pub struct OsVersionInfoMap(
    #[ts(as = "BTreeMap::<String, OsVersionInfo>")] pub BTreeMap<Version, OsVersionInfo>,
);
impl Map for OsVersionInfoMap {
    type Key = Version;
    type Value = OsVersionInfo;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(InternedString::from_display(key))
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(InternedString::from_display(key))
    }
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
