use serde::{Deserialize, Serialize};

use crate::account::AccountInfo;
use crate::net::acme::AcmeCertStore;
use crate::net::ssl::CertStore;
use crate::prelude::*;

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[model = "Model<Self>"]
#[serde(rename_all = "camelCase")]
pub struct KeyStore {
    pub local_certs: CertStore,
    #[serde(default)]
    pub acme: AcmeCertStore,
}
impl KeyStore {
    pub fn new(account: &AccountInfo) -> Result<Self, Error> {
        Ok(Self {
            local_certs: CertStore::new(account)?,
            acme: AcmeCertStore::new(),
        })
    }
}
