use exver::{PreReleaseSegment, VersionRange};
use imbl_value::json;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_6_alpha_16, VersionT};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_4_0_alpha_0: exver::Version = exver::Version::new(
        [0, 4, 0],
        [PreReleaseSegment::String("alpha".into()), 0.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_6_alpha_16::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_4_0_alpha_0.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<(), Error> {
        let host = db["public"]["serverInfo"]["host"].clone();
        let mut wifi = db["public"]["serverInfo"]["wifi"].clone();
        wifi["enabled"] = Value::Bool(true);
        let mut network_interfaces = db["public"]["serverInfo"]["networkInterfaces"].clone();
        for (k, v) in network_interfaces
            .as_object_mut()
            .into_iter()
            .flat_map(|m| m.iter_mut())
        {
            v["inbound"] = v["public"].clone();
            if v["ipInfo"].is_object() {
                v["ipInfo"]["name"] = Value::String((&**k).to_owned().into());
            }
        }
        let acme = db["public"]["serverInfo"]["acme"].clone();
        db["public"]["serverInfo"]["network"] = json!({
            "host": host,
            "wifi": wifi,
            "networkInterfaces": network_interfaces,
            "acme": acme,
        });
        Ok(())
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
