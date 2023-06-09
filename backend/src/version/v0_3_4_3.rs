use async_trait::async_trait;
use emver::VersionRange;

use super::v0_3_0::V0_3_0_COMPAT;
use super::*;

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
    async fn up<Db: DbHandle>(&self, db: &mut Db, _secrets: &PgPool) -> Result<(), Error> {
        crate::db::DatabaseModel::new()
            .server_info()
            .get_mut(db)
            .await?
            .save(db)
            .await?;
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, _db: &mut Db, _secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
}
