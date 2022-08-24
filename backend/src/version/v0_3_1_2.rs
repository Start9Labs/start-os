use emver::VersionRange;

use super::v0_3_0::V0_3_0_COMPAT;
use super::*;

const V0_3_1_2: emver::Version = emver::Version::new(0, 3, 1, 2);

#[derive(Clone, Debug)]
pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_1::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_1_2
    }
    fn compat(&self) -> &'static VersionRange {
        &*V0_3_0_COMPAT
    }
    async fn up<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error> {
        let id = crate::db::DatabaseModel::new()
            .server_info()
            .hostname()
            .get(db, false)
            .await?;
        if crate::db::DatabaseModel::new()
            .server_info()
            .hostname()
            .get(db, false)
            .await
            .is_err()
        {
            crate::db::DatabaseModel::new()
                .server_info()
                .hostname()
                .put(db, &format!("embassy-{}", &*id))
                .await?;
        }

        // let ui = crate::db::DatabaseModel::new()
        // .ui()
        // .get(db, &false)
        // .await
        // .is_err()

        Ok(())
    }
    async fn down<Db: DbHandle>(&self, _db: &mut Db) -> Result<(), Error> {
        Ok(())
    }
}
