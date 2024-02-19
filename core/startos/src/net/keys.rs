use clap::Parser;
use color_eyre::eyre::eyre;
use models::{HostId, Id, PackageId};
use openssl::pkey::{PKey, Private};
use openssl::sha::Sha256;
use openssl::x509::X509;
use p256::elliptic_curve::pkcs8::EncodePrivateKey;
use serde::{Deserialize, Serialize};
use sqlx::{Acquire, PgExecutor};
use ssh_key::private::Ed25519PrivateKey;
use torut::onion::{OnionAddressV3, TorSecretKeyV3};
use zeroize::Zeroize;

use crate::config::ConfigureContext;
use crate::context::RpcContext;
use crate::control::{restart, ControlParams};
use crate::disk::fsck::RequiresReboot;
use crate::net::ssl::CertPair;
use crate::prelude::*;
use crate::util::crypto::ed25519_expand_key;

// TODO: delete once we may change tor addresses
async fn compat(
    secrets: impl PgExecutor<'_>,
    host: &Option<(PackageId, HostId)>,
) -> Result<Option<[u8; 64]>, Error> {
    if let Some((package, host)) = host {
        if let Some(r) = sqlx::query!(
            "SELECT key FROM tor WHERE package = $1 AND interface = $2",
            package,
            host
        )
        .fetch_optional(secrets)
        .await?
        {
            Ok(Some(<[u8; 64]>::try_from(r.key).map_err(|e| {
                Error::new(
                    eyre!("expected vec of len 64, got len {}", e.len()),
                    ErrorKind::ParseDbField,
                )
            })?))
        } else {
            Ok(None)
        }
    } else if let Some(key) = sqlx::query!("SELECT tor_key FROM account WHERE id = 0")
        .fetch_one(secrets)
        .await?
        .tor_key
    {
        Ok(Some(<[u8; 64]>::try_from(key).map_err(|e| {
            Error::new(
                eyre!("expected vec of len 64, got len {}", e.len()),
                ErrorKind::ParseDbField,
            )
        })?))
    } else {
        Ok(None)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Key {
    host: Option<(PackageId, HostId)>,
    base: [u8; 32],
    tor_key: [u8; 64], // Does NOT necessarily match base
}
impl Key {
    pub fn host(&self) -> Option<(PackageId, HostId)> {
        self.host.clone()
    }
    pub fn as_bytes(&self) -> [u8; 32] {
        self.base
    }
    pub fn internal_address(&self) -> String {
        self.host
            .as_ref()
            .map(|(pkg_id, _)| format!("{}.embassy", pkg_id))
            .unwrap_or_else(|| "embassy".to_owned())
    }
    pub fn tor_key(&self) -> TorSecretKeyV3 {
        self.tor_key.into()
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
            if let Ok(k) = p256::SecretKey::from_slice(&buf) {
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
        host: Option<(PackageId, HostId)>,
        bytes: [u8; 32],
        tor_key: [u8; 64],
    ) -> Self {
        Self {
            host,
            tor_key,
            base: bytes,
        }
    }
    pub fn from_bytes(host: Option<(PackageId, HostId)>, bytes: [u8; 32]) -> Self {
        Self::from_pair(host, bytes, ed25519_expand_key(&bytes))
    }
    pub fn new(host: Option<(PackageId, HostId)>) -> Self {
        Self::from_bytes(host, rand::random())
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
            package
        )
        .fetch_all(secrets)
        .await?
        .into_iter()
        .map(|row| {
            let host = Some((package.clone(), HostId::from(Id::try_from(row.interface)?)));
            let bytes = row.key.try_into().map_err(|e: Vec<u8>| {
                Error::new(
                    eyre!("Invalid length for network key {} expected 32", e.len()),
                    crate::ErrorKind::Database,
                )
            })?;
            Ok(match row.tor_key {
                Some(tor_key) => Key::from_pair(
                    host,
                    bytes,
                    tor_key.try_into().map_err(|e: Vec<u8>| {
                        Error::new(
                            eyre!("Invalid length for tor key {} expected 64", e.len()),
                            crate::ErrorKind::Database,
                        )
                    })?,
                ),
                None => Key::from_bytes(host, bytes),
            })
        })
        .collect()
    }
    pub async fn for_host<Ex>(
        secrets: &mut Ex,
        host: Option<(PackageId, HostId)>,
    ) -> Result<Self, Error>
    where
        for<'a> &'a mut Ex: PgExecutor<'a>,
    {
        let tentative = rand::random::<[u8; 32]>();
        let actual = if let Some((pkg, iface)) = &host {
            let k = tentative.as_slice();
            let actual = sqlx::query!(
                "INSERT INTO network_keys (package, interface, key) VALUES ($1, $2, $3) ON CONFLICT (package, interface) DO UPDATE SET package = EXCLUDED.package RETURNING key",
                pkg,
                iface,
                k,
            )
            .fetch_one(&mut *secrets)
            .await?.key;
            let mut bytes = tentative;
            bytes.clone_from_slice(actual.get(0..32).ok_or_else(|| {
                Error::new(
                    eyre!("Invalid key size returned from DB"),
                    crate::ErrorKind::Database,
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
                    crate::ErrorKind::Database,
                )
            })?);
            bytes
        };
        let mut res = Self::from_bytes(host, actual);
        if let Some(tor_key) = compat(secrets, &res.host).await? {
            res.tor_key = tor_key;
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

pub fn display_requires_reboot(_: RotateKeysParams, args: RequiresReboot) {
    if args.0 {
        println!("Server must be restarted for changes to take effect");
    }
}
#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct RotateKeysParams {
    package: Option<PackageId>,
    host: Option<HostId>,
}

// #[command(display(display_requires_reboot))]
pub async fn rotate_key(
    ctx: RpcContext,
    RotateKeysParams { package, host }: RotateKeysParams,
) -> Result<RequiresReboot, Error> {
    let mut pgcon = ctx.secret_store.acquire().await?;
    let mut tx = pgcon.begin().await?;
    if let Some(package) = package {
        let Some(host) = host else {
            return Err(Error::new(
                eyre!("Must specify host"),
                ErrorKind::InvalidRequest,
            ));
        };
        sqlx::query!(
            "DELETE FROM tor WHERE package = $1 AND interface = $2",
            &package,
            &host,
        )
        .execute(&mut *tx)
        .await?;
        sqlx::query!(
            "DELETE FROM network_keys WHERE package = $1 AND interface = $2",
            &package,
            &host,
        )
        .execute(&mut *tx)
        .await?;
        let new_key = Key::for_host(&mut *tx, Some((package.clone(), host.clone()))).await?;
        let needs_config = ctx
            .db
            .mutate(|v| {
                let installed = v
                    .as_public_mut()
                    .as_package_data_mut()
                    .as_idx_mut(&package)
                    .or_not_found(&package)?
                    .as_installed_mut()
                    .or_not_found("installed")?;
                let addrs = installed
                    .as_interface_addresses_mut()
                    .as_idx_mut(&host)
                    .or_not_found(&host)?;
                if let Some(lan) = addrs.as_lan_address_mut().transpose_mut() {
                    lan.ser(&new_key.local_address())?;
                }
                if let Some(lan) = addrs.as_tor_address_mut().transpose_mut() {
                    lan.ser(&new_key.tor_address().to_string())?;
                }

                // TODO
                // if installed
                //     .as_manifest()
                //     .as_config()
                //     .transpose_ref()
                //     .is_some()
                // {
                //     installed
                //         .as_status_mut()
                //         .as_configured_mut()
                //         .replace(&false)
                // } else {
                //     Ok(false)
                // }
                Ok(false)
            })
            .await?;
        tx.commit().await?;
        if needs_config {
            ctx.services
                .get(&package)
                .await
                .as_ref()
                .ok_or_else(|| {
                    Error::new(
                        eyre!("There is no manager running for {package}"),
                        ErrorKind::Unknown,
                    )
                })?
                .configure(ConfigureContext::default())
                .await?;
        } else {
            restart(ctx, ControlParams { id: package }).await?;
        }
        Ok(RequiresReboot(false))
    } else {
        sqlx::query!("UPDATE account SET tor_key = NULL, network_key = gen_random_bytes(32)")
            .execute(&mut *tx)
            .await?;
        let new_key = Key::for_host(&mut *tx, None).await?;
        let url = format!("https://{}", new_key.tor_address()).parse()?;
        ctx.db
            .mutate(|v| {
                v.as_public_mut()
                    .as_server_info_mut()
                    .as_tor_address_mut()
                    .ser(&url)
            })
            .await?;
        tx.commit().await?;
        Ok(RequiresReboot(true))
    }
}
