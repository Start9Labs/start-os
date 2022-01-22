use emver::VersionRange;
use lazy_static::lazy_static;

use super::*;

const V0_3_0: emver::Version = emver::Version::new(0, 3, 0, 0);
lazy_static! {
    static ref V0_3_0_COMPAT: VersionRange = VersionRange::Conj(
        Box::new(VersionRange::Anchor(
            emver::GTE,
            emver::Version::new(0, 3, 0, 0),
        )),
        Box::new(VersionRange::Anchor(emver::LTE, Current::new().semver())),
    );
}

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_0::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_0
    }
    fn compat(&self) -> &'static VersionRange {
        &*V0_3_0_COMPAT
    }
    async fn up<Db: DbHandle>(&self, _db: &mut Db) -> Result<(), Error> {
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, _db: &mut Db) -> Result<(), Error> {
        Ok(())
    }
}
