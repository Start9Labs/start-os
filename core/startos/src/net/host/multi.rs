use std::collections::BTreeMap;

use imbl_value::InternedString;
use models::HostId;
use serde::{Deserialize, Serialize};
use torut::onion::OnionAddressV3;

use crate::net::host::BindOptions;
use crate::net::keys::KeyStore;

pub struct MultiHost {
    pub id: HostId,
    pub onions: Vec<OnionAddressV3>,
    // TODO: domains
    // binds: BTreeMap<u16, BindOptions>,
}
