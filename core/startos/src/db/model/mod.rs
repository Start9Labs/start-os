use std::collections::BTreeMap;

use patch_db::HasModel;
use serde::{Deserialize, Serialize};

use crate::account::AccountInfo;
use crate::auth::Sessions;
use crate::backup::target::cifs::CifsTargets;
use crate::db::model::private::Private;
use crate::db::model::public::Public;
use crate::net::forward::AvailablePorts;
use crate::net::keys::KeyStore;
use crate::notifications::Notifications;
use crate::prelude::*;
use crate::ssh::SshKeys;
use crate::util::serde::Pem;

pub mod package;
pub mod private;
pub mod public;

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct Database {
    pub public: Public,
    pub private: Private,
}
impl Database {
    pub fn init(account: &AccountInfo, kiosk: Option<bool>) -> Result<Self, Error> {
        Ok(Self {
            public: Public::init(account, kiosk)?,
            private: Private {
                key_store: KeyStore::new(account)?,
                password: account.password.clone(),
                ssh_privkey: Pem(account.ssh_key.clone()),
                ssh_pubkeys: SshKeys::new(),
                available_ports: AvailablePorts::new(),
                sessions: Sessions::new(),
                notifications: Notifications::new(),
                cifs: CifsTargets::new(),
                package_stores: BTreeMap::new(),
                compat_s9pk_key: Pem(account.compat_s9pk_key.clone()),
            }, // TODO
        })
    }
}

pub type DatabaseModel = Model<Database>;
