use std::collections::BTreeMap;

use imbl_value::InternedString;
use models::PackageId;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::prelude::*;
use crate::util::PathOrUrl;
use crate::Error;

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Dependencies(pub BTreeMap<PackageId, DepInfo>);
impl Map for Dependencies {
    type Key = PackageId;
    type Value = DepInfo;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key)
    }
    fn key_string(key: &Self::Key) -> Result<imbl_value::InternedString, Error> {
        Ok(key.clone().into())
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct DepInfo {
    pub description: Option<String>,
    pub optional: bool,
    pub s9pk: Option<PathOrUrl>,
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct DependencyMetadata {
    #[ts(type = "string")]
    pub title: InternedString,
}
