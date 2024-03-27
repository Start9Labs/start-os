use serde::{Deserialize, Serialize};
use torut::onion::OnionAddressV3;

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "kind")]
pub enum HostAddress {
    Onion { address: OnionAddressV3 },
}
