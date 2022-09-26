use emver::VersionRange;

use crate::config::util::MergeWith;
use crate::hostname::{generate_id, get_hostname, sync_hostname};
use crate::ErrorKind;

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
        let defaults: serde_json::Value = serde_json::from_str(include_str!(
            "../../../frontend/patchdb-ui-seed.json"
        ))
        .map_err(|x| {
            Error::new(
                eyre!("Serialization error {:?}", x),
                ErrorKind::Serialization,
            )
        })?;
        let mut ui = crate::db::DatabaseModel::new()
            .ui()
            .get(db, false)
            .await?
            .clone();
        ui.merge_with(&defaults);
        crate::db::DatabaseModel::new().ui().put(db, &ui).await?;

        Ok(())
    }
    async fn down<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error> {
        Ok(())
    }
}
