use std::collections::BTreeMap;

use axum::routing::get;
use axum::{Json, Router};
use emver::{Version, VersionRange};
use imbl_value::InternedString;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::RegistryContext;
use crate::prelude::*;
use crate::registry::asset::RegistryAsset;
use crate::registry::signer::SignerInfo;

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct OsIndex {
    pub signers: Vec<SignerInfo>,
    #[ts(as = "BTreeMap::<String, OsVersionInfo>")]
    pub versions: BTreeMap<Version, OsVersionInfo>,
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct OsVersionInfo {
    pub headline: String,
    pub release_notes: String,
    #[ts(type = "string")]
    pub source_version: VersionRange,
    #[ts(as = "BTreeMap::<String, RegistryAsset>")]
    pub iso: BTreeMap<InternedString, RegistryAsset>,
    #[ts(as = "BTreeMap::<String, RegistryAsset>")]
    pub squashfs: BTreeMap<InternedString, RegistryAsset>,
    #[ts(as = "BTreeMap::<String, RegistryAsset>")]
    pub img: BTreeMap<InternedString, RegistryAsset>,
}

pub async fn get_os_index(ctx: RegistryContext) -> Result<OsIndex, Error> {
    ctx.db.peek().await.into_index().into_os().de()
}

pub fn router(ctx: &RegistryContext) -> Router {
    Router::new().route("/index", {
        let ctx = ctx.clone();
        get(|| async { Ok::<_, Error>(Json(get_os_index(ctx).await?)) })
    })
}
