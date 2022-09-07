use emver::VersionRange;

use crate::hostname::{generate_id, get_hostname, sync_hostname};

use super::v0_3_0::V0_3_0_COMPAT;
use super::*;

const V0_3_2: emver::Version = emver::Version::new(0, 3, 2, 0);

#[derive(Clone, Debug)]
pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_1_2::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_2
    }
    fn compat(&self) -> &'static VersionRange {
        &*V0_3_0_COMPAT
    }
    async fn up<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error> {
        let hostname = get_hostname(db).await?;
        crate::db::DatabaseModel::new()
            .server_info()
            .hostname()
            .put(db, &Some(hostname.0))
            .await?;
        crate::db::DatabaseModel::new()
            .server_info()
            .id()
            .put(db, &generate_id())
            .await?;

        sync_hostname(db).await?;
        let mut ui = crate::db::DatabaseModel::new()
            .ui()
            .get(db, false)
            .await?
            .clone();
        if let serde_json::Value::Object(ref mut ui) = ui {
            ui.insert("ack-instructions".to_string(), serde_json::json!({}));
        }
        crate::db::DatabaseModel::new().ui().put(db, &ui).await?;
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error> {
        let mut ui = crate::db::DatabaseModel::new()
            .ui()
            .get(db, false)
            .await?
            .clone();
        if let serde_json::Value::Object(ref mut ui) = ui {
            ui.remove("ack-instructions");
        }
        crate::db::DatabaseModel::new().ui().put(db, &ui).await?;
        Ok(())
    }
}
