use emver::VersionRange;

use super::*;
use crate::disk::quirks::{fetch_quirks, save_quirks, update_quirks};

const V0_3_0_1: emver::Version = emver::Version::new(0, 3, 0, 1);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_0::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_0_1
    }
    fn compat(&self) -> &'static VersionRange {
        &*v0_3_0::V0_3_0_COMPAT
    }
    async fn up<Db: DbHandle>(&self, _db: &mut Db) -> Result<(), Error> {
        let mut q = fetch_quirks().await?;
        update_quirks(&mut q).await?;
        save_quirks(&q).await?;
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, _db: &mut Db) -> Result<(), Error> {
        Ok(())
    }
}
