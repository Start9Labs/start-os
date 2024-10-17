use exver::{PreReleaseSegment, VersionRange};

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_6_alpha_2, VersionT};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_3_6_alpha_3: exver::Version = exver::Version::new(
        [0, 3, 6],
        [PreReleaseSegment::String("alpha".into()), 3.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_6_alpha_2::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_3_6_alpha_3.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    fn up(self, _db: &mut Value, _: Self::PreUpRes) -> Result<(), Error> {
        Ok(())
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
