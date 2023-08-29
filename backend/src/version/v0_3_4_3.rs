use async_trait::async_trait;
use emver::VersionRange;

use super::v0_3_4::V0_3_0_COMPAT;
use super::*;

use crate::prelude::*;

const V0_3_4_3: emver::Version = emver::Version::new(0, 3, 4, 3);

#[derive(Clone, Debug)]
pub struct Version;

#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_4_2::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_4_3
    }
    fn compat(&self) -> &'static VersionRange {
        &*V0_3_0_COMPAT
    }
    async fn up(&self, db: PatchDb, _secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
    async fn down(&self, _db: PatchDb, _secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
}
