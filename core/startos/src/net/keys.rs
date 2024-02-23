use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
use torut::onion::{OnionAddressV3, TorSecretKeyV3};

use crate::account::AccountInfo;
use crate::net::ssl::CertStore;
use crate::prelude::*;

#[derive(Deserialize, Serialize, HasModel)]
#[model = "Model<Self>"]
pub struct KeyStore {
    pub onion: BTreeMap<OnionAddressV3, TorSecretKeyV3>,
    pub local_certs: CertStore,
    // pub letsencrypt_certs: BTreeMap<BTreeSet<InternedString>, CertData>
}
impl KeyStore {
    pub fn new(account: &AccountInfo) -> Result<Self, Error> {
        let mut res = Self {
            onion: BTreeMap::new(),
            local_certs: CertStore::new(account)?,
        };
        res.onion.insert(
            account.tor_key.public().get_onion_address(),
            account.tor_key.clone(),
        );
        Ok(res)
    }
}
