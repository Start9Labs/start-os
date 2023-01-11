use async_trait::async_trait;
use emver::VersionRange;
use serde_json::json;

use super::v0_3_0::V0_3_0_COMPAT;
use super::*;

const V0_3_3_1: emver::Version = emver::Version::new(0, 3, 3, 1);

#[derive(Clone, Debug)]
pub struct Version;

#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_3::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_3_1
    }
    fn compat(&self) -> &'static VersionRange {
        &*V0_3_0_COMPAT
    }
    async fn up<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error> {
        let mut ui = crate::db::DatabaseModel::new().ui().get_mut(db).await?;
        ui["marketplace"]["known-hosts"]["https://community-registry.start9.com/"] = json!({});
        ui.save(db).await?;

        Ok(())
    }
    async fn down<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error> {
        let mut ui = crate::db::DatabaseModel::new().ui().get_mut(db).await?;

        ui["marketplace"]["known-hosts"]["https://community-registry.start9.com/"].take();
        ui.save(db).await?;
        Ok(())
    }
}
