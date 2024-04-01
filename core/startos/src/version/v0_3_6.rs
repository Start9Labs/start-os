use emver::VersionRange;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_5_1, VersionT};
use crate::prelude::*;

const V0_3_6: emver::Version = emver::Version::new(0, 3, 6, 0);

#[derive(Clone, Debug)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_5_1::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_6
    }
    fn compat(&self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    async fn up(&self, _db: &PatchDb) -> Result<(), Error> {
        Err(Error::new(eyre!("unimplemented"), ErrorKind::Unknown))
    }
    async fn down(&self, _db: &PatchDb) -> Result<(), Error> {
        Ok(())
    }
}
