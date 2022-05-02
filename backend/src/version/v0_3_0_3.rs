use emver::VersionRange;

use super::*;

const V0_3_0_3: emver::Version = emver::Version::new(0, 3, 0, 3);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_0_2::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_0_3
    }
    fn compat(&self) -> &'static VersionRange {
        &*v0_3_0::V0_3_0_COMPAT
    }
    async fn up<Db: DbHandle>(&self, _db: &mut Db) -> Result<(), Error> {
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, _db: &mut Db) -> Result<(), Error> {
        Ok(())
    }
}
