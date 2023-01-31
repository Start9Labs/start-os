use ed25519_dalek::{ExpandedSecretKey, SecretKey};
use models::ResultExt;
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use sqlx::PgExecutor;

use crate::hostname::{generate_hostname, generate_id, Hostname};
use crate::net::keys::Key;
use crate::net::ssl::{generate_key, make_root_cert};
use crate::Error;

fn hash_password(password: &str) -> Result<String, Error> {
    argon2::hash_encoded(
        password.as_bytes(),
        &rand::random::<[u8; 16]>()[..],
        &argon2::Config::default(),
    )
    .with_kind(crate::ErrorKind::PasswordHashGeneration)
}

#[derive(Debug, Clone)]
pub struct AccountInfo {
    pub server_id: String,
    pub hostname: Hostname,
    pub password: String,
    pub key: Key,
    pub root_ca_key: PKey<Private>,
    pub root_ca_cert: X509,
}
impl AccountInfo {
    pub fn new(password: &str) -> Result<Self, Error> {
        let server_id = generate_id();
        let hostname = generate_hostname();
        let root_ca_key = generate_key()?;
        let root_ca_cert = make_root_cert(&root_ca_key, &hostname)?;
        Ok(Self {
            server_id,
            hostname,
            password: hash_password(password)?,
            key: Key::new(None),
            root_ca_key,
            root_ca_cert,
        })
    }

    pub async fn load(secrets: impl PgExecutor<'_>) -> Result<Self, Error> {
        let r = sqlx::query!("SELECT * FROM account WHERE id = 0")
            .fetch_one(secrets)
            .await?;

        let server_id = r.server_id.unwrap_or_else(generate_id);
        let hostname = r.hostname.map(Hostname).unwrap_or_else(generate_hostname);
        let password = r.password;
        let network_key = SecretKey::from_bytes(&r.network_key)?;
        let tor_key = if let Some(k) = &r.tor_key {
            ExpandedSecretKey::from_bytes(k)?
        } else {
            ExpandedSecretKey::from(&network_key)
        };
        let key = Key::from_pair(None, network_key.to_bytes(), tor_key.to_bytes());
        let root_ca_key = PKey::private_key_from_pem(r.root_ca_key_pem.as_bytes())?;
        let root_ca_cert = X509::from_pem(r.root_ca_cert_pem.as_bytes())?;

        Ok(Self {
            server_id,
            hostname,
            password,
            key,
            root_ca_key,
            root_ca_cert,
        })
    }

    pub async fn save(&self, secrets: impl PgExecutor<'_>) -> Result<(), Error> {
        let server_id = self.server_id.as_str();
        let hostname = self.hostname.0.as_str();
        let password = self.password.as_str();
        let network_key = self.key.as_bytes();
        let network_key = network_key.as_slice();
        let root_ca_key = String::from_utf8(self.root_ca_key.private_key_to_pem_pkcs8()?)?;
        let root_ca_cert = String::from_utf8(self.root_ca_cert.to_pem()?)?;

        sqlx::query!(
            r#"
            INSERT INTO account (
                id,
                server_id,
                hostname,
                password,
                network_key,
                root_ca_key_pem,
                root_ca_cert_pem
            ) VALUES (
                0, $1, $2, $3, $4, $5, $6
            ) ON CONFLICT (id) DO UPDATE SET
                server_id = EXCLUDED.server_id,
                hostname = EXCLUDED.hostname,
                password = EXCLUDED.password,
                network_key = EXCLUDED.network_key,
                root_ca_key_pem = EXCLUDED.root_ca_key_pem,
                root_ca_cert_pem = EXCLUDED.root_ca_cert_pem
            "#,
            server_id,
            hostname,
            password,
            network_key,
            root_ca_key,
            root_ca_cert,
        )
        .execute(secrets)
        .await?;

        Ok(())
    }

    pub fn set_password(&mut self, password: &str) -> Result<(), Error> {
        self.password = hash_password(password)?;
        Ok(())
    }
}
