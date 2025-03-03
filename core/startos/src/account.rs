use std::time::SystemTime;

use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use torut::onion::TorSecretKeyV3;

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
    pub tor_keys: Vec<TorSecretKeyV3>,
    pub root_ca_key: PKey<Private>,
    pub root_ca_cert: X509,
    pub ssh_key: ssh_key::PrivateKey,
    pub compat_s9pk_key: ed25519_dalek::SigningKey,
}
impl AccountInfo {
    pub fn new(password: &str, start_time: SystemTime) -> Result<Self, Error> {
        let server_id = generate_id();
        let hostname = generate_hostname();
        let tor_key = vec![TorSecretKeyV3::generate()];
        let root_ca_key = generate_key()?;
        let root_ca_cert = make_root_cert(&root_ca_key, &hostname, start_time)?;
        let ssh_key = ssh_key::PrivateKey::from(ssh_key::private::Ed25519Keypair::random(
            &mut ssh_key::rand_core::OsRng::default(),
        ));
        let compat_s9pk_key =
            ed25519_dalek::SigningKey::generate(&mut ssh_key::rand_core::OsRng::default());
        Ok(Self {
            server_id,
            hostname,
            password: hash_password(password)?,
            tor_keys: tor_key,
            root_ca_key,
            root_ca_cert,
            ssh_key,
            compat_s9pk_key,
        })
    }

    pub fn load(db: &DatabaseModel) -> Result<Self, Error> {
        let server_id = db.as_public().as_server_info().as_id().de()?;
        let hostname = Hostname(db.as_public().as_server_info().as_hostname().de()?);
        let password = db.as_private().as_password().de()?;
        let key_store = db.as_private().as_key_store();
        let tor_addrs = db.as_public().as_server_info().as_host().as_onions().de()?;
        let tor_keys = tor_addrs
            .into_iter()
            .map(|tor_addr| key_store.as_onion().get_key(&tor_addr))
            .collect::<Result<_, _>>()?;
        let cert_store = key_store.as_local_certs();
        let root_ca_key = cert_store.as_root_key().de()?.0;
        let root_ca_cert = cert_store.as_root_cert().de()?.0;
        let ssh_key = db.as_private().as_ssh_privkey().de()?.0;
        let compat_s9pk_key = db.as_private().as_compat_s9pk_key().de()?.0;

        Ok(Self {
            server_id,
            hostname,
            password,
            tor_keys,
            root_ca_key,
            root_ca_cert,
            ssh_key,
            compat_s9pk_key,
        })
    }

    pub fn save(&self, db: &mut DatabaseModel) -> Result<(), Error> {
        let server_info = db.as_public_mut().as_server_info_mut();
        server_info.as_id_mut().ser(&self.server_id)?;
        server_info.as_hostname_mut().ser(&self.hostname.0)?;
        server_info
            .as_pubkey_mut()
            .ser(&self.ssh_key.public_key().to_openssh()?)?;
        server_info.as_host_mut().as_onions_mut().ser(
            &self
                .tor_keys
                .iter()
                .map(|tor_key| tor_key.public().get_onion_address())
                .collect(),
        )?;
        db.as_private_mut().as_password_mut().ser(&self.password)?;
        db.as_private_mut()
            .as_ssh_privkey_mut()
            .ser(Pem::new_ref(&self.ssh_key))?;
        db.as_private_mut()
            .as_compat_s9pk_key_mut()
            .ser(Pem::new_ref(&self.compat_s9pk_key))?;
        let key_store = db.as_private_mut().as_key_store_mut();
        for tor_key in &self.tor_keys {
            key_store.as_onion_mut().insert_key(tor_key)?;
        }
        let cert_store = key_store.as_local_certs_mut();
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
