use async_trait::async_trait;
use emver::VersionRange;
use lazy_static::lazy_static;

use super::*;
use crate::prelude::*;

const V0_3_5: emver::Version = emver::Version::new(0, 3, 5, 0);
lazy_static! {
    static ref V0_3_0_COMPAT: VersionRange = VersionRange::Conj(
        Box::new(VersionRange::Anchor(
            emver::GTE,
            emver::Version::new(0, 3, 0, 0)
        )),
        Box::new(VersionRange::Anchor(emver::LTE, Current::new().semver())),
    );
}

#[derive(Clone, Debug)]
pub struct Version;

#[async_trait]
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
    async fn up(&self, _db: &PatchDb, _secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
    async fn down(&self, _db: &PatchDb, _secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
}
