use exver::{PreReleaseSegment, VersionRange};
use imbl_value::json;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_6_alpha_10, VersionT};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_3_6_alpha_11: exver::Version = exver::Version::new(
        [0, 3, 6],
        [PreReleaseSegment::String("alpha".into()), 11.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_6_alpha_10::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_3_6_alpha_11.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<(), Error> {
        let acme = std::mem::replace(
            &mut db["public"]["serverInfo"]["acme"],
            Value::Object(Default::default()),
        );
        if !acme.is_null() {
            db["public"]["serverInfo"]["acme"]
                [&acme["provider"].as_str().or_not_found("provider")?] =
                json!({ "contact": &acme["contact"] });
        }

        Ok(())
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
