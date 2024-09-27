use exver::{PreReleaseSegment, VersionRange};

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_6_alpha_6, VersionT};
use crate::{prelude::*, util::ApplyRef};

lazy_static::lazy_static! {
    static ref V0_3_6_alpha_7: exver::Version = exver::Version::new(
        [0, 3, 6],
        [PreReleaseSegment::String("alpha".into()), 7.into()]
    );
}

#[derive(Clone, Debug)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_6_alpha_6::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> exver::Version {
        V0_3_6_alpha_7.clone()
    }
    fn compat(&self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    fn up(&self, db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
    fn down(&self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
