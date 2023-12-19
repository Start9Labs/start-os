use async_trait::async_trait;
use emver::VersionRange;
use sqlx::PgPool;

use super::VersionT;
use super::{v0_3_4::V0_3_0_COMPAT, v0_3_5_1};
use crate::prelude::*;

const V0_3_5_2: emver::Version = emver::Version::new(0, 3, 5, 2);

#[derive(Clone, Debug)]
pub struct Version;

#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_5_1::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_5_2
    }
    fn compat(&self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    async fn up(&self, _db: PatchDb, _secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
    async fn down(&self, _db: PatchDb, _secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
}
