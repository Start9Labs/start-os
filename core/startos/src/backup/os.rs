use imbl_value::InternedString;
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use patch_db::Value;
use serde::{Deserialize, Serialize};
use ssh_key::private::Ed25519Keypair;
use torut::onion::TorSecretKeyV3;

use crate::account::AccountInfo;
use crate::hostname::{generate_hostname, generate_id, Hostname};
use crate::prelude::*;
use crate::util::crypto::ed25519_expand_key;
use crate::util::serde::{Base32, Base64, Pem};

pub struct OsBackup {
    pub account: AccountInfo,
    pub ui: Value,
}
impl<'de> Deserialize<'de> for OsBackup {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let tagged = OsBackupSerDe::deserialize(deserializer)?;
        Ok(match tagged.version {
            0 => patch_db::value::from_value::<OsBackupV0>(tagged.rest)
                .map_err(serde::de::Error::custom)?
                .project()
                .map_err(serde::de::Error::custom)?,
            1 => patch_db::value::from_value::<OsBackupV1>(tagged.rest)
                .map_err(serde::de::Error::custom)?
                .project(),
            2 => patch_db::value::from_value::<OsBackupV2>(tagged.rest)
                .map_err(serde::de::Error::custom)?
                .project(),
            v => {
                return Err(serde::de::Error::custom(&format!(
                    "Unknown backup version {v}"
                )))
            }
        })
    }
}
impl Serialize for OsBackup {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        OsBackupSerDe {
            version: 2,
            rest: patch_db::value::to_value(&OsBackupV2::unproject(self))
                .map_err(serde::ser::Error::custom)?,
        }
        .serialize(serializer)
    }
}

#[derive(Deserialize, Serialize)]
struct OsBackupSerDe {
    #[serde(default)]
    version: usize,
    #[serde(flatten)]
    rest: Value,
}

/// V0
#[derive(Deserialize)]
#[serde(rename = "kebab-case")]
struct OsBackupV0 {
    tor_key: Base32<[u8; 64]>, // Base32 Encoded Ed25519 Expanded Secret Key
    root_ca_key: Pem<PKey<Private>>, // PEM Encoded OpenSSL Key
    root_ca_cert: Pem<X509>,   // PEM Encoded OpenSSL X509 Certificate
    ui: Value,                 // JSON Value
}
impl OsBackupV0 {
    fn project(self) -> Result<OsBackup, Error> {
        Ok(OsBackup {
            account: AccountInfo {
                server_id: generate_id(),
                hostname: generate_hostname(),
                password: Default::default(),
                root_ca_key: self.root_ca_key.0,
                root_ca_cert: self.root_ca_cert.0,
                ssh_key: ssh_key::PrivateKey::random(
                    &mut rand::thread_rng(),
                    ssh_key::Algorithm::Ed25519,
                )?,
                tor_keys: vec![TorSecretKeyV3::from(self.tor_key.0)],
                compat_s9pk_key: ed25519_dalek::SigningKey::generate(&mut rand::thread_rng()),
            },
            ui: self.ui,
        })
    }
}

/// V1
#[derive(Deserialize, Serialize)]
#[serde(rename = "kebab-case")]
struct OsBackupV1 {
    server_id: String,               // uuidv4
    hostname: InternedString,        // embassy-<adjective>-<noun>
    net_key: Base64<[u8; 32]>,       // Ed25519 Secret Key
    root_ca_key: Pem<PKey<Private>>, // PEM Encoded OpenSSL Key
    root_ca_cert: Pem<X509>,         // PEM Encoded OpenSSL X509 Certificate
    ui: Value,                       // JSON Value
}
impl OsBackupV1 {
    fn project(self) -> OsBackup {
        OsBackup {
            account: AccountInfo {
                server_id: self.server_id,
                hostname: Hostname(self.hostname),
                password: Default::default(),
                root_ca_key: self.root_ca_key.0,
                root_ca_cert: self.root_ca_cert.0,
                ssh_key: ssh_key::PrivateKey::from(Ed25519Keypair::from_seed(&self.net_key.0)),
                tor_keys: vec![TorSecretKeyV3::from(ed25519_expand_key(&self.net_key.0))],
                compat_s9pk_key: ed25519_dalek::SigningKey::from_bytes(&self.net_key),
            },
            ui: self.ui,
        }
    }
}

/// V2
#[derive(Deserialize, Serialize)]
#[serde(rename = "kebab-case")]

struct OsBackupV2 {
    server_id: String,                               // uuidv4
    hostname: InternedString,                        // <adjective>-<noun>
    root_ca_key: Pem<PKey<Private>>,                 // PEM Encoded OpenSSL Key
    root_ca_cert: Pem<X509>,                         // PEM Encoded OpenSSL X509 Certificate
    ssh_key: Pem<ssh_key::PrivateKey>,               // PEM Encoded OpenSSH Key
    tor_keys: Vec<TorSecretKeyV3>,                   // Base64 Encoded Ed25519 Expanded Secret Key
    compat_s9pk_key: Pem<ed25519_dalek::SigningKey>, // PEM Encoded ED25519 Key
    ui: Value,                                       // JSON Value
}
impl OsBackupV2 {
    fn project(self) -> OsBackup {
        OsBackup {
            account: AccountInfo {
                server_id: self.server_id,
                hostname: Hostname(self.hostname),
                password: Default::default(),
                root_ca_key: self.root_ca_key.0,
                root_ca_cert: self.root_ca_cert.0,
                ssh_key: self.ssh_key.0,
                tor_keys: self.tor_keys,
                compat_s9pk_key: self.compat_s9pk_key.0,
            },
            ui: self.ui,
        }
    }
    fn unproject(backup: &OsBackup) -> Self {
        Self {
            server_id: backup.account.server_id.clone(),
            hostname: backup.account.hostname.0.clone(),
            root_ca_key: Pem(backup.account.root_ca_key.clone()),
            root_ca_cert: Pem(backup.account.root_ca_cert.clone()),
            ssh_key: Pem(backup.account.ssh_key.clone()),
            tor_keys: backup.account.tor_keys.clone(),
            compat_s9pk_key: Pem(backup.account.compat_s9pk_key.clone()),
            ui: backup.ui.clone(),
        }
    }
}
