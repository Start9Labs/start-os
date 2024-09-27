use exver::{PreReleaseSegment, VersionRange};

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_6_alpha_3, VersionT};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_3_6_alpha_4: exver::Version = exver::Version::new(
        [0, 3, 6],
        [PreReleaseSegment::String("alpha".into()), 4.into()]
    );
}

#[derive(Clone, Debug)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_6_alpha_3::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> exver::Version {
        V0_3_6_alpha_4.clone()
    }
    fn compat(&self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
     fn up(&self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
     fn down(&self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
