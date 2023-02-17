use openssl::pkey::PKey;
use openssl::x509::X509;
use patch_db::Value;
use serde::{Deserialize, Serialize};

use crate::account::AccountInfo;
use crate::hostname::{generate_hostname, generate_id, Hostname};
use crate::net::keys::Key;
use crate::prelude::*;
use crate::util::serde::Base64;

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
        match tagged.version {
            0 => patch_db::value::from_value::<OsBackupV0>(tagged.rest)
                .map_err(serde::de::Error::custom)?
                .project()
                .map_err(serde::de::Error::custom),
            1 => patch_db::value::from_value::<OsBackupV1>(tagged.rest)
                .map_err(serde::de::Error::custom)?
                .project()
                .map_err(serde::de::Error::custom),
            v => Err(serde::de::Error::custom(&format!(
                "Unknown backup version {v}"
            ))),
        }
    }
}
impl Serialize for OsBackup {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        OsBackupSerDe {
            version: 1,
            rest: patch_db::value::to_value(
                &OsBackupV1::unproject(self).map_err(serde::ser::Error::custom)?,
            )
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
    // tor_key: Base32<[u8; 64]>,
    root_ca_key: String,  // PEM Encoded OpenSSL Key
    root_ca_cert: String, // PEM Encoded OpenSSL X509 Certificate
    ui: Value,            // JSON Value
}
impl OsBackupV0 {
    fn project(self) -> Result<OsBackup, Error> {
        Ok(OsBackup {
            account: AccountInfo {
                server_id: generate_id(),
                hostname: generate_hostname(),
                password: Default::default(),
                key: Key::new(None),
                root_ca_key: PKey::private_key_from_pem(self.root_ca_key.as_bytes())?,
                root_ca_cert: X509::from_pem(self.root_ca_cert.as_bytes())?,
            },
            ui: self.ui,
        })
    }
}

/// V1
#[derive(Deserialize, Serialize)]
#[serde(rename = "kebab-case")]
struct OsBackupV1 {
    server_id: String,         // uuidv4
    hostname: String,          // embassy-<adjective>-<noun>
    net_key: Base64<[u8; 32]>, // Ed25519 Secret Key
    root_ca_key: String,       // PEM Encoded OpenSSL Key
    root_ca_cert: String,      // PEM Encoded OpenSSL X509 Certificate
    ui: Value,                 // JSON Value
                               // TODO add more
}
impl OsBackupV1 {
    fn project(self) -> Result<OsBackup, Error> {
        Ok(OsBackup {
            account: AccountInfo {
                server_id: self.server_id,
                hostname: Hostname(self.hostname),
                password: Default::default(),
                key: Key::from_bytes(None, self.net_key.0),
                root_ca_key: PKey::private_key_from_pem(self.root_ca_key.as_bytes())?,
                root_ca_cert: X509::from_pem(self.root_ca_cert.as_bytes())?,
            },
            ui: self.ui,
        })
    }
    fn unproject(backup: &OsBackup) -> Result<Self, Error> {
        Ok(Self {
            server_id: backup.account.server_id.clone(),
            hostname: backup.account.hostname.0.clone(),
            net_key: Base64(backup.account.key.as_bytes()),
            root_ca_key: String::from_utf8(backup.account.root_ca_key.private_key_to_pem_pkcs8()?)?,
            root_ca_cert: String::from_utf8(backup.account.root_ca_cert.to_pem()?)?,
            ui: backup.ui.clone(),
        })
    }
}
