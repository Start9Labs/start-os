use emver::VersionRange;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_5_1, VersionT};
use crate::prelude::*;

const V0_3_5_2: emver::Version = emver::Version::new(0, 3, 5, 2);

#[derive(Clone, Debug)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_5_1::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_5_2
    }
    fn compat(&self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    async fn up(&self, _db: &PatchDb) -> Result<(), Error> {
        Ok(())
    }
    async fn down(&self, _db: &PatchDb) -> Result<(), Error> {
        Ok(())
    }
}
