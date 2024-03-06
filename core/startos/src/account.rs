use std::time::SystemTime;

use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use torut::onion::{OnionAddressV3, TorSecretKeyV3};

use crate::db::model::DatabaseModel;
use crate::hostname::{generate_hostname, generate_id, Hostname};
use crate::net::ssl::{generate_key, make_root_cert};
use crate::prelude::*;
use crate::util::serde::Pem;

fn hash_password(password: &str) -> Result<String, Error> {
    argon2::hash_encoded(
        password.as_bytes(),
        &rand::random::<[u8; 16]>()[..],
        &argon2::Config::rfc9106_low_mem(),
    )
    .with_kind(crate::ErrorKind::PasswordHashGeneration)
}

#[derive(Debug, Clone)]
pub struct AccountInfo {
    pub server_id: String,
    pub hostname: Hostname,
    pub password: String,
    pub tor_key: TorSecretKeyV3,
    pub root_ca_key: PKey<Private>,
    pub root_ca_cert: X509,
}
impl AccountInfo {
    pub fn new(password: &str, start_time: SystemTime) -> Result<Self, Error> {
        let server_id = generate_id();
        let hostname = generate_hostname();
        let tor_key = TorSecretKeyV3::generate();
        let root_ca_key = generate_key()?;
        let root_ca_cert = make_root_cert(&root_ca_key, &hostname, start_time)?;
        Ok(Self {
            server_id,
            hostname,
            password: hash_password(password)?,
            tor_key,
            root_ca_key,
            root_ca_cert,
        })
    }

    pub fn load(db: &DatabaseModel) -> Result<Self, Error> {
        let server_id = db.as_public().as_server_info().as_id().de()?;
        let hostname = Hostname(db.as_public().as_server_info().as_hostname().de()?);
        let password = db.as_private().as_password().de()?;
        let tor_key = db.as_private().as_tor_key().de()?;
        let cert_store = db.as_private().as_key_store().as_local_certs();
        let root_ca_key = cert_store.as_root_key().de()?.0;
        let root_ca_cert = cert_store.as_root_cert().de()?.0;

        Ok(Self {
            server_id,
            hostname,
            password,
            tor_key,
            root_ca_key,
            root_ca_cert,
        })
    }

    pub fn save(&self, db: &mut DatabaseModel) -> Result<(), Error> {
        db.as_public_mut()
            .as_server_info_mut()
            .as_id_mut()
            .ser(&self.server_id)?;
        db.as_public_mut()
            .as_server_info_mut()
            .as_hostname_mut()
            .ser(&self.hostname.0)?;
        db.as_private_mut().as_password_mut().ser(&self.password)?;
        db.as_private_mut().as_tor_key_mut().ser(&self.tor_key)?;
        let cert_store = db.as_private_mut().as_key_store_mut().as_local_certs_mut();
        cert_store
            .as_root_key_mut()
            .ser(Pem::new_ref(&self.root_ca_key))?;
        cert_store
            .as_root_cert_mut()
            .ser(Pem::new_ref(&self.root_ca_cert))?;
        Ok(())
    }

    pub fn set_password(&mut self, password: &str) -> Result<(), Error> {
        self.password = hash_password(password)?;
        Ok(())
    }
}
