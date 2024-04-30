use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use crate::prelude::*;
use crate::registry::signer::SignatureInfo;

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct RegistryAsset {
    #[ts(type = "string")]
    pub url: Url,
    pub signature_info: SignatureInfo,
}
impl AsRef<RegistryAsset> for RegistryAsset {
    fn as_ref(&self) -> &RegistryAsset {
        self
    }
}
