use models::{InterfaceId, PackageId};
use openssl::asn1::Asn1Time;
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use sqlx::PgExecutor;
use torut::onion::TorSecretKeyV3;

use crate::net::ssl::make_leaf_cert;
use crate::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Key {
    bytes: [u8; 32],
}
impl Key {
    pub fn tor_key(&self) -> TorSecretKeyV3 {
        ed25519_dalek::ExpandedSecretKey::from(
            &ed25519_dalek::SecretKey::from_bytes(&self.bytes).unwrap(),
        )
        .to_bytes()
        .into()
    }
    pub fn openssl_key(&self) -> PKey<Private> {
        PKey::private_key_from_raw_bytes(&self.bytes, openssl::pkey::Id::ED25519).unwrap()
    }
    pub fn new() -> Self {
        Self {
            bytes: rand::random(),
        }
    }
    pub fn from_bytes(bytes: [u8; 32]) -> Self {
        Self { bytes }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct KeyInfo {
    interface: Option<(PackageId, InterfaceId)>,
    key: Key,
    cert: X509,
    int: X509,
    root: X509,
}
impl KeyInfo {
    pub fn interface(&self) -> Option<(&PackageId, &InterfaceId)> {
        self.interface.as_ref().map(|(a, b)| (a, b))
    }
    pub fn key(&self) -> &Key {
        &self.key
    }
    pub fn cert(&self) -> &X509 {
        &self.cert
    }
    pub fn fullchain(&self) -> Vec<&X509> {
        vec![&self.cert, &self.int, &self.root]
    }
    pub async fn load(
        secrets: impl PgExecutor<'_>,
        interface: Option<(PackageId, InterfaceId)>,
    ) -> Result<Option<Self>, Error> {
        if let Some((key, cert, int, root)) = {
            let package = interface.as_ref().map(|a| &**a.0).unwrap_or_default();
            let interface = interface.as_ref().map(|a| &**a.1).unwrap_or_default();
            sqlx::query!(
                "SELECT network_keys.key, network_keys.cert_pem, account.root_ca_cert_pem, account.int_ca_cert_pem FROM network_keys CROSS JOIN account WHERE package = $1 AND interface = $2",
                package,
                interface,
            )
            .fetch_optional(secrets)
            .await?
            .map(|r| (r.key, r.cert_pem, r.int_ca_cert_pem, r.root_ca_cert_pem))
        } {
            let mut bytes = [0u8; 32];
            bytes.clone_from_slice(&key);
            let cert = X509::from_pem(&cert.into_bytes())?;
            let int = X509::from_pem(&int.into_bytes())?;
            let root = X509::from_pem(&root.into_bytes())?;
            Ok(Some(KeyInfo {
                interface,
                key: Key::from_bytes(bytes),
                cert,
                int,
                root,
            }))
        } else {
            Ok(None)
        }
    }
    pub async fn save(&self, secrets: impl PgExecutor<'_>) -> Result<(), Error> {
        let package = self.interface.as_ref().map(|a| &**a.0).unwrap_or_default();
        let interface = self.interface.as_ref().map(|a| &**a.1).unwrap_or_default();
        let key = &self.key.bytes;
        let cert_pem = String::from_utf8(self.cert.to_pem()?)?;
        sqlx::query!(
            "INSERT INTO network_keys (package, interface, key, cert_pem) VALUES ($1, $2, $3, $4) ON CONFLICT (package, interface) DO UPDATE SET key = EXCLUDED.key, cert_pem = EXCLUDED.cert_pem",
            package,
            interface,
            key,
            cert_pem,
        )
        .execute(secrets)
        .await?;
        Ok(())
    }
    pub async fn create(
        secrets: impl PgExecutor<'_>,
        interface: Option<(PackageId, InterfaceId)>,
        key: Key,
    ) -> Result<Self, Error> {
        let (root, int, int_key) = {
            let row = sqlx::query!("SELECT root_ca_cert_pem, int_ca_cert_pem, int_ca_key_pem FROM account WHERE id = 0").fetch_one(secrets).await?;
            (
                X509::from_pem(&row.root_ca_cert_pem.into_bytes())?,
                X509::from_pem(&row.int_ca_cert_pem.into_bytes())?,
                PKey::private_key_from_pem(&row.int_ca_key_pem.into_bytes())?,
            )
        };
        let cert = make_leaf_cert(
            (&int_key, &int),
            (
                &key.openssl_key(),
                &key.tor_key()
                    .public()
                    .get_onion_address()
                    .get_address_without_dot_onion(),
                interface.as_ref().map(|i| &i.0),
            ),
        )?;
        Ok(Self {
            interface,
            key,
            cert,
            int,
            root,
        })
    }
    pub async fn load_or_create<Ex>(
        secrets: &mut Ex,
        interface: Option<(PackageId, InterfaceId)>,
    ) -> Result<Self, Error>
    where
        for<'a> &'a mut Ex: PgExecutor<'a>,
    {
        if let Some(k) = Self::load(&mut *secrets, interface.clone()).await? {
            if k.cert.not_after().compare(&*Asn1Time::days_from_now(90)?)?
                == std::cmp::Ordering::Greater
            {
                Ok(k)
            } else {
                let k = Self::create(&mut *secrets, interface, k.key).await?;
                k.save(&mut *secrets).await?;
                Ok(k)
            }
        } else {
            let k = Self::create(&mut *secrets, interface, Key::new()).await?;
            k.save(&mut *secrets).await?;
            Ok(k)
        }
    }
}

#[test]
pub fn test_keygen() {
    let key = Key::new();
    key.tor_key();
    key.openssl_key();
}
