use std::collections::BTreeMap;

use models::PackageId;
use patch_db::{HasModel, Value};
use serde::{Deserialize, Serialize};

use crate::auth::Sessions;
use crate::backup::target::cifs::CifsTargets;
use crate::net::forward::AvailablePorts;
use crate::net::keys::KeyStore;
use crate::notifications::Notifications;
use crate::prelude::*;
use crate::ssh::SshKeys;
use crate::util::serde::Pem;

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct Private {
    pub key_store: KeyStore,
    pub password: String, // argon2 hash
    pub ssh_privkey: Pem<ssh_key::PrivateKey>,
    pub ssh_pubkeys: SshKeys,
    pub available_ports: AvailablePorts,
    pub sessions: Sessions,
    pub notifications: Notifications,
    pub cifs: CifsTargets,
    #[serde(default)]
    pub package_stores: BTreeMap<PackageId, Value>,
}
