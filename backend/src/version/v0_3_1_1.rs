use emver::VersionRange;

use super::{v0_3_0::V0_3_0_COMPAT, *};

const V0_3_1_1: emver::Version = emver::Version::new(0, 3, 1, 1);

#[derive(Clone, Debug)]
pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_1::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_1_1
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
