use async_trait::async_trait;
use emver::VersionRange;
use itertools::Itertools;
use lazy_static::lazy_static;
use serde_json::{json, Value};

use super::*;

const V0_3_4: emver::Version = emver::Version::new(0, 3, 4, 0);
lazy_static! {
    pub static ref V0_3_0_COMPAT: VersionRange = VersionRange::Conj(
        Box::new(VersionRange::Anchor(
            emver::GTE,
            emver::Version::new(0, 3, 0, 0)
        )),
        Box::new(VersionRange::Anchor(emver::LTE, Current::new().semver())),
    );
}

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
    type Previous = Self;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_4
    }
    fn compat(&self) -> &'static VersionRange {
        &*V0_3_0_COMPAT
    }
    // added pubkey, ca_fingerprint
    async fn up(&self, secrets: &PgPool, db: &PatchDb) -> Result<(), Error> {
        // let mut account = AccountInfo::load(secrets).await?;
        // crate::db::DatabaseModel::new()
        //     .server_info()
        //     .pubkey()
        //     .put(
        //         db,
        //         &ssh_key::PublicKey::from(Ed25519PublicKey::from(&account.key.ssh_key()))
        //             .to_openssh()?,
        //     )
        //     .await?;
        // crate::db::DatabaseModel::new()
        //     .server_info()
        //     .ca_fingerprint()
        //     .put(
        //         db,
        //         &account
        //             .root_ca_cert
        //             .digest(MessageDigest::sha256())
        //             .unwrap()
        //             .iter()
        //             .map(|x| format!("{x:X}"))
        //             .join(":"),
        //     )
        //     .await?;
        // let server_info = crate::db::DatabaseModel::new()
        //     .server_info()
        //     .get(db)
        //     .await?
        //     .into_owned();
        // account.hostname = Hostname(server_info.hostname);
        // account.server_id = server_info.id;
        // account.save(secrets).await?;

        // sync_hostname(&account).await?;

        Ok(())
    }
    async fn down(&self, secrets: &PgPool, db: &PatchDb) -> Result<(), Error> {
        Ok(())
    }
}
