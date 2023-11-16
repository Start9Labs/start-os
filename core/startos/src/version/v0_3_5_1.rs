use async_trait::async_trait;
use emver::VersionRange;
use sqlx::PgPool;

use super::v0_3_4::V0_3_0_COMPAT;
use super::{v0_3_5, VersionT};
use crate::prelude::*;

const V0_3_5_1: emver::Version = emver::Version::new(0, 3, 5, 1);

#[derive(Clone, Debug)]
pub struct Version;

#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_5::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_5_1
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
