use serde::{Deserialize, Serialize};

use crate::account::AccountInfo;
use crate::net::acme::AcmeCertStore;
use crate::net::ssl::CertStore;
use crate::net::tor::OnionStore;
use crate::prelude::*;

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[model = "Model<Self>"]
pub struct KeyStore {
    pub onion: OnionStore,
    pub local_certs: CertStore,
    #[serde(default)]
    pub acme: AcmeCertStore,
}
impl KeyStore {
    pub fn new(account: &AccountInfo) -> Result<Self, Error> {
        let mut res = Self {
            onion: OnionStore::new(),
            local_certs: CertStore::new(account)?,
            acme: AcmeCertStore::new(),
        };
        res.onion.insert(account.tor_key.clone());
        Ok(res)
    }
}
