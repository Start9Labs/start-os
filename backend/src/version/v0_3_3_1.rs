use async_trait::async_trait;
use emver::VersionRange;
use serde_json::{json, Value};

use super::v0_3_3::V0_3_0_COMPAT;
use super::*;

const V0_3_3_1: emver::Version = emver::Version::new(0, 3, 3, 1);

const COMMUNITY_URL: &str = "https://community-registry.start9.com/";
const MAIN_REGISTRY: &str = "https://registry.start9.com/";
const COMMUNITY_SERVICES: &[&str] = &[
    "ipfs",
    "agora",
    "lightning-jet",
    "balanceofsatoshis",
    "mastodon",
    "lndg",
    "robosats",
    "thunderhub",
    "syncthing",
    "sphinx-relay",
];

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
    async fn up<Db: DbHandle>(&self, db: &mut Db, secrets: &PgPool) -> Result<(), Error> {
        let parsed_url = Some(COMMUNITY_URL.parse().unwrap());
        let mut ui = crate::db::DatabaseModel::new().ui().get_mut(db).await?;
        ui["marketplace"]["known-hosts"][COMMUNITY_URL] = json!({});
        ui.save(db).await?;
        for package_id in crate::db::DatabaseModel::new()
            .package_data()
            .keys(db)
            .await?
        {
            if !COMMUNITY_SERVICES.contains(&&*package_id.to_string()) {
                continue;
            }
            crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(&package_id)
                .expect(db)
                .await?
                .installed()
                .expect(db)
                .await?
                .marketplace_url()
                .put(db, &parsed_url)
                .await?;
        }
        ui["theme"] = json!("Dark".to_string());
        ui["widgets"] = json!([]);
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, db: &mut Db, secrets: &PgPool) -> Result<(), Error> {
        let mut ui = crate::db::DatabaseModel::new().ui().get_mut(db).await?;
        let parsed_url = Some(MAIN_REGISTRY.parse().unwrap());
        for package_id in crate::db::DatabaseModel::new()
            .package_data()
            .keys(db)
            .await?
        {
            if !COMMUNITY_SERVICES.contains(&&*package_id.to_string()) {
                continue;
            }
            crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(&package_id)
                .expect(db)
                .await?
                .installed()
                .expect(db)
                .await?
                .marketplace_url()
                .put(db, &parsed_url)
                .await?;
        }

        if let Value::Object(ref mut obj) = *ui {
            obj.remove("theme");
            obj.remove("widgets");
        }

        ui["marketplace"]["known-hosts"][COMMUNITY_URL].take();
        ui.save(db).await?;
        Ok(())
    }
}
