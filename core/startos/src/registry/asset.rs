use reqwest::Client;
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWrite;
use ts_rs::TS;
use url::Url;

use crate::prelude::*;
use crate::registry::signer::{AcceptSigners, FileValidator, SignatureInfo};

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
impl RegistryAsset {
    pub fn validate(&self, accept: AcceptSigners) -> Result<FileValidator, Error> {
        self.signature_info.validate(accept)
    }
    pub async fn download(
        &self,
        client: Client,
        dst: &mut (impl AsyncWrite + Unpin + Send + ?Sized),
        validator: &FileValidator,
    ) -> Result<(), Error> {
        validator.download(self.url.clone(), client, dst).await
    }
}
