use async_trait::async_trait;
use emver::VersionRange;
use lazy_static::lazy_static;

use super::*;

const V0_4_0: emver::Version = emver::Version::new(0, 4, 0, 0);
lazy_static! {
    pub static ref V0_4_0_COMPAT: VersionRange = VersionRange::Conj(
        Box::new(VersionRange::Anchor(emver::GTE, V0_4_0)),
        Box::new(VersionRange::Anchor(emver::LTE, Current::new().semver())),
    );
}

#[derive(Clone, Debug)]
pub struct Version;

#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_4_4::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_4_0
    }
    fn compat(&self) -> &'static VersionRange {
        &V0_4_0_COMPAT
    }
    async fn up<Db: DbHandle>(&self, _db: &mut Db, _secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, _db: &mut Db, _secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
}
