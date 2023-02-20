use async_trait::async_trait;
use emver::VersionRange;
use itertools::Itertools;
use lazy_static::lazy_static;
use openssl::hash::MessageDigest;
use ssh_key::public::Ed25519PublicKey;

use crate::account::AccountInfo;
use crate::hostname::{sync_hostname, Hostname};

use super::*;

const V0_3_4: emver::Version = emver::Version::new(0, 3, 4, 0);
lazy_static! {
    pub static ref V0_3_4_COMPAT: VersionRange = VersionRange::Conj(
        Box::new(VersionRange::Anchor(
            emver::GTE,
            emver::Version::new(0, 3, 4, 0),
        )),
        Box::new(VersionRange::Anchor(emver::LTE, Current::new().semver())),
    );
}

#[derive(Clone, Debug)]
pub struct Version;

#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_3_1::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_4
    }
    fn compat(&self) -> &'static VersionRange {
        &*V0_3_4_COMPAT
    }
    // added pubkey, ca_fingerprint
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
        account.hostname = Hostname(server_info.hostname);
        account.server_id = server_info.id;
        account.save(secrets).await?;

        sync_hostname(&account).await?;

        Ok(())
    }
    async fn down<Db: DbHandle>(&self, db: &mut Db, secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
}
