use async_trait::async_trait;
use emver::VersionRange;
use itertools::Itertools;
use openssl::hash::MessageDigest;
use serde_json::{json, Value};
use ssh_key::public::Ed25519PublicKey;

use super::*;
use crate::account::AccountInfo;
use crate::hostname::{sync_hostname, Hostname};
use crate::prelude::*;

const V0_3_4: emver::Version = emver::Version::new(0, 3, 4, 0);

lazy_static::lazy_static! {
    pub static ref V0_3_0_COMPAT: VersionRange = VersionRange::Conj(
        Box::new(VersionRange::Anchor(
            emver::GTE,
            emver::Version::new(0, 3, 0, 0),
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
    async fn up(&self, db: PatchDb, secrets: &PgPool) -> Result<(), Error> {
        let mut account = AccountInfo::load(secrets).await?;
        let account = db
            .mutate(|d| {
                d.as_server_info_mut().as_pub_key_mut().ser(
                    &ssh_key::PublicKey::from(Ed25519PublicKey::from(&account.key.ssh_key()))
                        .to_openssh()?,
                );
                d.as_server_info_mut().as_ca_fingerprint_mut().ser(
                    &account
                        .root_ca_cert
                        .digest(MessageDigest::sha256())
                        .unwrap()
                        .iter()
                        .map(|x| format!("{x:X}"))
                        .join(":"),
                );
                let server_info = d.as_server_info();
                account.hostname = server_info.as_hostname().de().map(Hostname)?;
                account.server_id = server_info.as_id().de()?;

                Ok(account)
            })
            .await?;
        account.save(secrets).await?;
        let peek = db.peek().await?;
        sync_hostname(&account.hostname).await?;

        let parsed_url = Some(COMMUNITY_URL.parse().unwrap());
        db.mutate(|d| {
            let mut ui = d.as_ui().de()?;
            ui["marketplace"]["known-hosts"][COMMUNITY_URL] = json!({});
            ui["marketplace"]["known-hosts"][MAIN_REGISTRY] = json!({});
            for package_id in d.as_package_data().keys()? {
                if !COMMUNITY_SERVICES.contains(&&*package_id.to_string()) {
                    continue;
                }
                d.as_package_data_mut()
                    .as_idx_model_mut(&package_id)
                    .or_not_found(&package_id)?
                    .as_installed_mut()
                    .or_not_found(&package_id)?
                    .as_marketplace_url_mut()
                    .ser(&parsed_url)?;
            }
            ui["theme"] = json!("Dark".to_string());
            ui["widgets"] = json!([]);

            d.as_ui_mut().ser(&ui)
        })
        .await
    }
    async fn down(&self, db: PatchDb, _secrets: &PgPool) -> Result<(), Error> {
        db.mutate(|d| {
            let mut ui = d.as_ui().de()?;
            let parsed_url = Some(MAIN_REGISTRY.parse().unwrap());
            for package_id in db.as_package_data().keys()? {
                if !COMMUNITY_SERVICES.contains(&&*package_id.to_string()) {
                    continue;
                }
                d.as_package_data_mut()
                    .as_idx_model_mut(&package_id)
                    .or_not_found(&package_id)?
                    .as_installed_mut()
                    .or_not_found(&package_id)?
                    .as_marketplace_url_mut()
                    .ser(&parsed_url)?;
            }

            if let Value::Object(ref mut obj) = *ui {
                obj.remove("theme");
                obj.remove("widgets");
            }

            ui["marketplace"]["known-hosts"][COMMUNITY_URL].take();
            ui["marketplace"]["known-hosts"][MAIN_REGISTRY].take();
            d.as_ui_mut().ser(&ui)
        })
        .await
    }
}
