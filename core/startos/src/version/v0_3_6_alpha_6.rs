use exver::{PreReleaseSegment, VersionRange};

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_6_alpha_5, VersionT};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_3_6_alpha_6: exver::Version = exver::Version::new(
        [0, 3, 6],
        [PreReleaseSegment::String("alpha".into()), 6.into()]
    );
}

#[derive(Default, Clone, Copy, Debug)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_6_alpha_5::Version;
    type PreUpRes = ();
    fn semver(self) -> exver::Version {
        V0_3_6_alpha_6.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn up(self, _db: &mut Value, _: Self::PreUpRes) -> Result<(), Error> {
        Ok(())
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
