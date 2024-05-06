use emver::VersionRange;

use super::VersionT;
use crate::db::model::Database;
use crate::prelude::*;
use crate::version::Current;

lazy_static::lazy_static! {
    pub static ref V0_3_0_COMPAT: VersionRange = VersionRange::Conj(
        Box::new(VersionRange::Anchor(
            emver::GTE,
            emver::Version::new(0, 3, 0, 0),
        )),
        Box::new(VersionRange::Anchor(emver::LTE, Current::new().semver())),
    );
}

const V0_3_5: emver::Version = emver::Version::new(0, 3, 5, 0);

#[derive(Clone, Debug)]
pub struct Version;

impl VersionT for Version {
    type Previous = Self;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_5
    }
    fn compat(&self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    async fn up(&self, _db: &TypedPatchDb<Database>) -> Result<(), Error> {
        Ok(())
    }
    async fn down(&self, _db: &TypedPatchDb<Database>) -> Result<(), Error> {
        Ok(())
    }
}
