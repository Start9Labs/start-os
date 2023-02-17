use color_eyre::eyre::eyre;
use ed25519_dalek::{ExpandedSecretKey, SecretKey};
use models::{Id, InterfaceId, PackageId};
use openssl::pkey::{PKey, Private};
use openssl::sha::Sha256;
use openssl::x509::X509;
use p256::elliptic_curve::pkcs8::EncodePrivateKey;
use sqlx::PgExecutor;
use ssh_key::private::Ed25519PrivateKey;
use torut::onion::{OnionAddressV3, TorSecretKeyV3};
use tracing::instrument;
use zeroize::Zeroize;

use crate::net::ssl::CertPair;
use crate::prelude::*;

// TODO: delete once we may change tor addresses
#[instrument(skip_all)]
async fn compat(
    secrets: impl PgExecutor<'_>,
    interface: &Option<(PackageId, InterfaceId)>,
) -> Result<Option<ExpandedSecretKey>, Error> {
    if let Some((package, interface)) = interface {
        if let Some(r) = sqlx::query!(
            "SELECT key FROM tor WHERE package = $1 AND interface = $2",
            **package,
            **interface
        )
        .fetch_optional(secrets)
        .await?
        {
            Ok(Some(ExpandedSecretKey::from_bytes(&r.key)?))
        } else {
            Ok(None)
        }
    } else {
        if let Some(key) = sqlx::query!("SELECT tor_key FROM account WHERE id = 0")
            .fetch_one(secrets)
            .await?
            .tor_key
        {
            Ok(Some(ExpandedSecretKey::from_bytes(&key)?))
        } else {
            Ok(None)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Key {
    interface: Option<(PackageId, InterfaceId)>,
    base: [u8; 32],
    tor_key: [u8; 64], // Does NOT necessarily match base
}
impl Key {
    pub fn interface(&self) -> Option<(PackageId, InterfaceId)> {
        self.interface.clone()
    }
    pub fn as_bytes(&self) -> [u8; 32] {
        self.base
    }
    pub fn internal_address(&self) -> String {
        self.interface
            .as_ref()
            .map(|(pkg_id, _)| format!("{}.embassy", pkg_id))
            .unwrap_or_else(|| "embassy".to_owned())
    }
    pub fn tor_key(&self) -> TorSecretKeyV3 {
        ed25519_dalek::ExpandedSecretKey::from_bytes(&self.tor_key)
            .unwrap()
            .to_bytes()
            .into()
    }
    pub fn tor_address(&self) -> OnionAddressV3 {
        self.tor_key().public().get_onion_address()
    }
    pub fn base_address(&self) -> String {
        self.tor_key()
            .public()
            .get_onion_address()
            .get_address_without_dot_onion()
    }
    pub fn local_address(&self) -> String {
        self.base_address() + ".local"
    }
    pub fn openssl_key_ed25519(&self) -> PKey<Private> {
        PKey::private_key_from_raw_bytes(&self.base, openssl::pkey::Id::ED25519).unwrap()
    }
    pub fn openssl_key_nistp256(&self) -> PKey<Private> {
        let mut buf = self.base;
        loop {
            if let Ok(k) = p256::SecretKey::from_be_bytes(&buf) {
                return PKey::private_key_from_pkcs8(&*k.to_pkcs8_der().unwrap().as_bytes())
                    .unwrap();
            }
            let mut sha = Sha256::new();
            sha.update(&buf);
            buf = sha.finish();
        }
    }
    pub fn ssh_key(&self) -> Ed25519PrivateKey {
        Ed25519PrivateKey::from_bytes(&self.base)
    }
    pub(crate) fn from_pair(
        interface: Option<(PackageId, InterfaceId)>,
        bytes: [u8; 32],
        tor_key: [u8; 64],
    ) -> Self {
        Self {
            interface,
            tor_key,
            base: bytes,
        }
    }
    pub fn from_bytes(interface: Option<(PackageId, InterfaceId)>, bytes: [u8; 32]) -> Self {
        Self::from_pair(
            interface,
            bytes,
            ExpandedSecretKey::from(&SecretKey::from_bytes(&bytes).unwrap()).to_bytes(),
        )
    }
    pub fn new(interface: Option<(PackageId, InterfaceId)>) -> Self {
        Self::from_bytes(interface, rand::random())
    }
    pub(super) fn with_certs(self, certs: CertPair, int: X509, root: X509) -> KeyInfo {
        KeyInfo {
            key: self,
            certs,
            int,
            root,
        }
    }
    pub async fn for_package(
        secrets: impl PgExecutor<'_>,
        package: &PackageId,
    ) -> Result<Vec<Self>, Error> {
        sqlx::query!(
            r#"
                SELECT
                    network_keys.package,
                    network_keys.interface,
                    network_keys.key,
                    tor.key AS "tor_key?"
                FROM
                    network_keys
                LEFT JOIN
                    tor
                ON
                    network_keys.package = tor.package
                AND
                    network_keys.interface = tor.interface
                WHERE
                    network_keys.package = $1
            "#,
            **package
        )
        .fetch_all(secrets)
        .await?
        .into_iter()
        .map(|row| {
            let interface = Some((
                package.clone(),
                InterfaceId::from(Id::try_from(row.interface)?),
            ));
            let bytes = row.key.try_into().map_err(|e: Vec<u8>| {
                Error::new(
                    eyre!("Invalid length for network key {} expected 32", e.len()),
                    ErrorKind::Database,
                )
            })?;
            Ok(match row.tor_key {
                Some(tor_key) => Key::from_pair(
                    interface,
                    bytes,
                    tor_key.try_into().map_err(|e: Vec<u8>| {
                        Error::new(
                            eyre!("Invalid length for tor key {} expected 64", e.len()),
                            ErrorKind::Database,
                        )
                    })?,
                ),
                None => Key::from_bytes(interface, bytes),
            })
        })
        .collect()
    }
    #[instrument(skip_all)]
    pub async fn for_interface<Ex>(
        secrets: &mut Ex,
        interface: Option<(PackageId, InterfaceId)>,
    ) -> Result<Self, Error>
    where
        for<'a> &'a mut Ex: PgExecutor<'a>,
    {
        let tentative = rand::random::<[u8; 32]>();
        let actual = if let Some((pkg, iface)) = &interface {
            let k = tentative.as_slice();
            let actual = sqlx::query!(
                "INSERT INTO network_keys (package, interface, key) VALUES ($1, $2, $3) ON CONFLICT (package, interface) DO UPDATE SET package = EXCLUDED.package RETURNING key",
                **pkg,
                **iface,
                k,
            )
            .fetch_one(&mut *secrets)
            .await?.key;
            let mut bytes = tentative;
            bytes.clone_from_slice(actual.get(0..32).ok_or_else(|| {
                Error::new(
                    eyre!("Invalid key size returned from DB"),
                    ErrorKind::Database,
                )
            })?);
            bytes
        } else {
            let actual = sqlx::query!("SELECT network_key FROM account WHERE id = 0")
                .fetch_one(&mut *secrets)
                .await?
                .network_key;
            let mut bytes = tentative;
            bytes.clone_from_slice(actual.get(0..32).ok_or_else(|| {
                Error::new(
                    eyre!("Invalid key size returned from DB"),
                    ErrorKind::Database,
                )
            })?);
            bytes
        };
        let mut res = Self::from_bytes(interface, actual);
        if let Some(tor_key) = compat(secrets, &res.interface).await? {
            res.tor_key = tor_key.to_bytes();
        }
        Ok(res)
    }
}
impl Drop for Key {
    fn drop(&mut self) {
        self.base.zeroize();
        self.tor_key.zeroize();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct KeyInfo {
    key: Key,
    certs: CertPair,
    int: X509,
    root: X509,
}
impl KeyInfo {
    pub fn key(&self) -> &Key {
        &self.key
    }
    pub fn certs(&self) -> &CertPair {
        &self.certs
    }
    pub fn int_ca(&self) -> &X509 {
        &self.int
    }
    pub fn root_ca(&self) -> &X509 {
        &self.root
    }
    pub fn fullchain_ed25519(&self) -> Vec<&X509> {
        vec![&self.certs.ed25519, &self.int, &self.root]
    }
    pub fn fullchain_nistp256(&self) -> Vec<&X509> {
        vec![&self.certs.nistp256, &self.int, &self.root]
    }
}

#[test]
pub fn test_keygen() {
    let key = Key::new(None);
    key.tor_key();
    key.openssl_key_nistp256();
}
