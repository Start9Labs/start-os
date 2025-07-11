use std::collections::BTreeMap;

use exver::{PreReleaseSegment, VersionRange};
use imbl_value::json;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_6_alpha_11, VersionT};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_3_6_alpha_12: exver::Version = exver::Version::new(
        [0, 3, 6],
        [PreReleaseSegment::String("alpha".into()), 12.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_6_alpha_11::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_3_6_alpha_12.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<Value, Error> {
        let bindings: BTreeMap<u16, Value> = [(
            80,
            json!({
                "enabled": false,
                "options": {
                    "preferredExternalPort": 80,
                    "addSsl": {
                        "preferredExternalPort": 443,
                        "alpn": { "specified": [ "http/1.1", "h2" ] },
                    },
                    "secure": null,
                },
                "net": {
                    "assignedPort": null,
                    "assignedSslPort": 443,
                    "public": false,
                }
            }),
        )]
        .into_iter()
        .collect();
        let onion = db["public"]["serverInfo"]["onionAddress"].clone();
        db["public"]["serverInfo"]["host"] = json!({
            "bindings": bindings,
            "onions": [onion],
            "domains": {},
            "hostnameInfo": {},
        });

        Ok(Value::Null)
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
