use async_trait::async_trait;
use emver::VersionRange;

use super::*;

const V0_3_4_2: emver::Version = emver::Version::new(0, 3, 4, 2);
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
        V0_3_4_2
    }
    fn compat(&self) -> &'static VersionRange {
        &*V0_3_0_COMPAT
    }
    async fn up(&self, db: &PatchDb, secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
    async fn down(&self, db: &PatchDb, secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
}
