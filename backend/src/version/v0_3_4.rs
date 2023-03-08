use async_trait::async_trait;
use emver::VersionRange;
use itertools::Itertools;
use openssl::hash::MessageDigest;
use serde_json::{json, Value};
use ssh_key::public::Ed25519PublicKey;

use crate::account::AccountInfo;
use crate::hostname::{generate_hostname, sync_hostname, Hostname};

use super::v0_3_0::V0_3_0_COMPAT;
use super::*;

const V0_3_4: emver::Version = emver::Version::new(0, 3, 4, 0);

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
        V0_3_4
    }
    fn compat(&self) -> &'static VersionRange {
        &*V0_3_0_COMPAT
    }
    async fn up<Db: DbHandle>(&self, db: &mut Db, secrets: &PgPool) -> Result<(), Error> {
        let mut account = AccountInfo::load(secrets).await?;
        crate::db::DatabaseModel::new()
            .server_info()
            .pubkey()
            .put(
                db,
                &ssh_key::PublicKey::from(Ed25519PublicKey::from(&account.key.ssh_key()))
                    .to_openssh()?,
            )
            .await?;
        crate::db::DatabaseModel::new()
            .server_info()
            .ca_fingerprint()
            .put(
                db,
                &account
                    .root_ca_cert
                    .digest(MessageDigest::sha256())
                    .unwrap()
                    .iter()
                    .map(|x| format!("{x:X}"))
                    .join(":"),
            )
            .await?;
        let server_info = crate::db::DatabaseModel::new()
            .server_info()
            .get(db)
            .await?
            .into_owned();
        account.hostname = server_info
            .hostname
            .map(Hostname)
            .unwrap_or_else(generate_hostname);
        account.server_id = server_info.id;
        account.save(secrets).await?;
        sync_hostname(&account).await?;

        let parsed_url = Some(COMMUNITY_URL.parse().unwrap());
        let mut ui = crate::db::DatabaseModel::new().ui().get_mut(db).await?;
        ui["marketplace"]["known-hosts"][COMMUNITY_URL] = json!({});
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
        ui.save(db).await?;
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, db: &mut Db, _secrets: &PgPool) -> Result<(), Error> {
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
