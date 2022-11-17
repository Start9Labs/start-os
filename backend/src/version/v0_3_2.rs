use emver::VersionRange;

use super::v0_3_0::V0_3_0_COMPAT;
use super::*;
use crate::config::util::MergeWith;
use crate::hostname::{generate_id, sync_hostname};

const V0_3_2: emver::Version = emver::Version::new(0, 3, 2, 0);

lazy_static::lazy_static! {
    static ref DEFAULT_UI: serde_json::Value =serde_json::json!({
        "name": null,
        "auto-check-updates": true,
        "pkg-order": [],
        "ack-welcome": "0.3.2",
        "marketplace": {
          "selected-id": null,
          "known-hosts": {}
        },
        "dev": {},
        "gaming": {
          "snake": {
            "high-score": 0
          }
        },
        "ack-instructions": {}
      });

}

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
        let receipts = crate::hostname::HostNameReceipt::new(db).await?;
        crate::hostname::ensure_hostname_is_set(db, &receipts).await?;
        receipts.id.set(db, generate_id()).await?;

        let mut ui = crate::db::DatabaseModel::new()
            .ui()
            .get(db, false)
            .await?
            .clone();
        ui.merge_with(&DEFAULT_UI);
        crate::db::DatabaseModel::new().ui().put(db, &ui).await?;

        sync_hostname(db, &receipts).await?;
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, _db: &mut Db) -> Result<(), Error> {
        Ok(())
    }
}
