use std::fmt;
use std::str::FromStr;

use imbl_value::InternedString;
use serde::{Deserialize, Serialize};
use torut::onion::OnionAddressV3;
use ts_rs::TS;

use crate::prelude::*;

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, TS)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "kind")]
#[ts(export)]
pub enum HostAddress {
    Onion {
        #[ts(type = "string")]
        address: OnionAddressV3,
    },
    Domain {
        #[ts(type = "string")]
        address: InternedString,
    },
}

impl FromStr for HostAddress {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(addr) = s.strip_suffix(".onion") {
            Ok(HostAddress::Onion {
                address: addr
                    .parse::<OnionAddressV3>()
                    .with_kind(ErrorKind::ParseUrl)?,
            })
        } else {
            Ok(HostAddress::Domain { address: s.into() })
        }
    }
}

impl fmt::Display for HostAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Onion { address } => write!(f, "{address}.onion"),
            Self::Domain { address } => write!(f, "{address}"),
        }
    }
}
