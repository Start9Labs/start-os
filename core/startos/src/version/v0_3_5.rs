use exver::{ExtendedVersion, VersionRange};

use super::VersionT;
use crate::prelude::*;
use crate::version::Current;

lazy_static::lazy_static! {
    pub static ref V0_3_0_COMPAT: VersionRange = VersionRange::and(
        VersionRange::anchor(
            exver::GTE,
            ExtendedVersion::new(
                exver::Version::new([0, 3, 0], []),
                exver::Version::default(),
            ),
        ),
        VersionRange::anchor(
            exver::LTE,
            ExtendedVersion::new(
                Current::default().semver().without_prerelease(),
                exver::Version::default(),
            )
        ),
    );
    static ref V0_3_5: exver::Version = exver::Version::new([0, 3, 5], []);
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = Self;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_3_5.clone()
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
