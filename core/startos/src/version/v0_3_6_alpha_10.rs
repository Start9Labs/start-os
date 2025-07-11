use std::collections::{BTreeMap, BTreeSet};

use exver::{PreReleaseSegment, VersionRange};
use imbl_value::InternedString;
use serde::{Deserialize, Serialize};
use torut::onion::OnionAddressV3;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_6_alpha_9, VersionT};
use crate::net::host::address::DomainConfig;
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_3_6_alpha_10: exver::Version = exver::Version::new(
        [0, 3, 6],
        [PreReleaseSegment::String("alpha".into()), 10.into()]
    );
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "kind")]
enum HostAddress {
    Onion { address: OnionAddressV3 },
    Domain { address: InternedString },
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_6_alpha_9::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_3_6_alpha_10.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<Value, Error> {
        for (_, package) in db["public"]["packageData"]
            .as_object_mut()
            .ok_or_else(|| {
                Error::new(
                    eyre!("expected public.packageData to be an object"),
                    ErrorKind::Database,
                )
            })?
            .iter_mut()
        {
            for (_, host) in package["hosts"]
                .as_object_mut()
                .ok_or_else(|| {
                    Error::new(
                        eyre!("expected public.packageData[id].hosts to be an object"),
                        ErrorKind::Database,
                    )
                })?
                .iter_mut()
            {
                let mut onions = BTreeSet::new();
                let mut domains = BTreeMap::new();
                let addresses = from_value::<BTreeSet<HostAddress>>(host["addresses"].clone())?;
                for address in addresses {
                    match address {
                        HostAddress::Onion { address } => {
                            onions.insert(address);
                        }
                        HostAddress::Domain { address } => {
                            domains.insert(
                                address,
                                DomainConfig {
                                    public: true,
                                    acme: None,
                                },
                            );
                        }
                    }
                }
                host["onions"] = to_value(&onions)?;
                host["domains"] = to_value(&domains)?;
            }
        }

        Ok(Value::Null)
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
