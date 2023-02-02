use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::path::Path;

use futures::FutureExt;
use openssl::asn1::{Asn1Integer, Asn1Time};
use openssl::bn::{BigNum, MsbOption};
use openssl::ec::{EcGroup, EcKey};
use openssl::hash::MessageDigest;
use openssl::nid::Nid;
use openssl::pkey::{PKey, Private};
use openssl::x509::{X509Builder, X509Extension, X509NameBuilder, X509};
use openssl::*;
use tokio::sync::{Mutex, RwLock};
use tracing::instrument;

use crate::account::AccountInfo;
use crate::hostname::Hostname;
use crate::net::keys::{Key, KeyInfo};
use crate::s9pk::manifest::PackageId;
use crate::{Error, ErrorKind, ResultExt};

static CERTIFICATE_VERSION: i32 = 2; // X509 version 3 is actually encoded as '2' in the cert because fuck you.
pub const ROOT_CA_STATIC_PATH: &str = "/var/lib/embassy/ssl/root-ca.crt";

#[derive(Debug)]
pub struct SslManager {
    root_cert: X509,
    int_key: PKey<Private>,
    int_cert: X509,
    cert_cache: RwLock<BTreeMap<Key, X509>>,
}
impl SslManager {
    pub fn new(account: &AccountInfo) -> Result<Self, Error> {
        let int_key = generate_key()?;
        let int_cert = make_int_cert((&account.root_ca_key, &account.root_ca_cert), &int_key)?;
        Ok(Self {
            root_cert: account.root_ca_cert.clone(),
            int_key,
            int_cert,
            cert_cache: RwLock::new(BTreeMap::new()),
        })
    }
    pub async fn with_cert(&self, key: Key) -> Result<KeyInfo, Error> {
        if let Some(cert) = self.cert_cache.read().await.get(&key) {
            if cert
                .not_after()
                .compare(Asn1Time::days_from_now(30)?.as_ref())?
                == Ordering::Greater
            {
                return Ok(key.with_cert(
                    cert.clone(),
                    self.int_cert.clone(),
                    self.root_cert.clone(),
                ));
            }
        }
        let cert = make_leaf_cert(
            (&self.int_key, &self.int_cert),
            (
                &key.openssl_key(),
                &key.tor_key()
                    .public()
                    .get_onion_address()
                    .get_address_without_dot_onion(),
                key.interface().map(|i| i.0).as_ref(),
            ),
        )?;
        self.cert_cache
            .write()
            .await
            .insert(key.clone(), cert.clone());

        Ok(key.with_cert(cert, self.int_cert.clone(), self.root_cert.clone()))
    }
}

const EC_CURVE_NAME: nid::Nid = nid::Nid::X9_62_PRIME256V1;
lazy_static::lazy_static! {
    static ref EC_GROUP: EcGroup = EcGroup::from_curve_name(EC_CURVE_NAME).unwrap();
    static ref SSL_MUTEX: Mutex<()> = Mutex::new(()); // TODO: make thread safe
}

pub async fn export_key(key: &PKey<Private>, target: &Path) -> Result<(), Error> {
    tokio::fs::write(target, key.private_key_to_pem_pkcs8()?)
        .map(|res| res.with_ctx(|_| (ErrorKind::Filesystem, target.display().to_string())))
        .await?;
    Ok(())
}
pub async fn export_cert(chain: &[&X509], target: &Path) -> Result<(), Error> {
    tokio::fs::write(
        target,
        chain
            .into_iter()
            .flat_map(|c| c.to_pem().unwrap())
            .collect::<Vec<u8>>(),
    )
    .await?;
    Ok(())
}

#[instrument]
fn rand_serial() -> Result<Asn1Integer, Error> {
    let mut bn = BigNum::new()?;
    bn.rand(64, MsbOption::MAYBE_ZERO, false)?;
    let asn1 = Asn1Integer::from_bn(&bn)?;
    Ok(asn1)
}
#[instrument]
pub fn generate_key() -> Result<PKey<Private>, Error> {
    let new_key = EcKey::generate(EC_GROUP.as_ref())?;
    let key = PKey::from_ec_key(new_key)?;
    Ok(key)
}

#[instrument]
pub fn make_root_cert(root_key: &PKey<Private>, hostname: &Hostname) -> Result<X509, Error> {
    let mut builder = X509Builder::new()?;
    builder.set_version(CERTIFICATE_VERSION)?;

    let embargo = Asn1Time::days_from_now(0)?;
    builder.set_not_before(&embargo)?;

    let expiration = Asn1Time::days_from_now(3650)?;
    builder.set_not_after(&expiration)?;

    builder.set_serial_number(&*rand_serial()?)?;

    let mut subject_name_builder = X509NameBuilder::new()?;
    subject_name_builder.append_entry_by_text("CN", &format!("{} Local Root CA", &*hostname.0))?;
    subject_name_builder.append_entry_by_text("O", "Start9")?;
    subject_name_builder.append_entry_by_text("OU", "Embassy")?;
    let subject_name = subject_name_builder.build();
    builder.set_subject_name(&subject_name)?;

    builder.set_issuer_name(&subject_name)?;

    builder.set_pubkey(&root_key)?;

    // Extensions
    let cfg = conf::Conf::new(conf::ConfMethod::default())?;
    let ctx = builder.x509v3_context(None, Some(&cfg));
    // subjectKeyIdentifier = hash
    let subject_key_identifier =
        X509Extension::new_nid(Some(&cfg), Some(&ctx), Nid::SUBJECT_KEY_IDENTIFIER, "hash")?;
    // basicConstraints = critical, CA:true, pathlen:0
    let basic_constraints = X509Extension::new_nid(
        Some(&cfg),
        Some(&ctx),
        Nid::BASIC_CONSTRAINTS,
        "critical,CA:true",
    )?;
    // keyUsage = critical, digitalSignature, cRLSign, keyCertSign
    let key_usage = X509Extension::new_nid(
        Some(&cfg),
        Some(&ctx),
        Nid::KEY_USAGE,
        "critical,digitalSignature,cRLSign,keyCertSign",
    )?;
    builder.append_extension(subject_key_identifier)?;
    builder.append_extension(basic_constraints)?;
    builder.append_extension(key_usage)?;
    builder.sign(&root_key, MessageDigest::sha256())?;
    let cert = builder.build();
    Ok(cert)
}
#[instrument]
pub fn make_int_cert(
    signer: (&PKey<Private>, &X509),
    applicant: &PKey<Private>,
) -> Result<X509, Error> {
    let mut builder = X509Builder::new()?;
    builder.set_version(CERTIFICATE_VERSION)?;

    let embargo = Asn1Time::days_from_now(0)?;
    builder.set_not_before(&embargo)?;

    let expiration = Asn1Time::days_from_now(3650)?;
    builder.set_not_after(&expiration)?;

    builder.set_serial_number(&*rand_serial()?)?;

    let mut subject_name_builder = X509NameBuilder::new()?;
    subject_name_builder.append_entry_by_text("CN", "Embassy Local Intermediate CA")?;
    subject_name_builder.append_entry_by_text("O", "Start9")?;
    subject_name_builder.append_entry_by_text("OU", "Embassy")?;
    let subject_name = subject_name_builder.build();
    builder.set_subject_name(&subject_name)?;

    builder.set_issuer_name(signer.1.subject_name())?;

    builder.set_pubkey(&applicant)?;

    let cfg = conf::Conf::new(conf::ConfMethod::default())?;
    let ctx = builder.x509v3_context(Some(&signer.1), Some(&cfg));
    // subjectKeyIdentifier = hash
    let subject_key_identifier =
        X509Extension::new_nid(Some(&cfg), Some(&ctx), Nid::SUBJECT_KEY_IDENTIFIER, "hash")?;
    // authorityKeyIdentifier = keyid:always,issuer
    let authority_key_identifier = X509Extension::new_nid(
        Some(&cfg),
        Some(&ctx),
        Nid::AUTHORITY_KEY_IDENTIFIER,
        "keyid:always,issuer",
    )?;
    // basicConstraints = critical, CA:true, pathlen:0
    let basic_constraints = X509Extension::new_nid(
        Some(&cfg),
        Some(&ctx),
        Nid::BASIC_CONSTRAINTS,
        "critical,CA:true,pathlen:0",
    )?;
    // keyUsage = critical, digitalSignature, cRLSign, keyCertSign
    let key_usage = X509Extension::new_nid(
        Some(&cfg),
        Some(&ctx),
        Nid::KEY_USAGE,
        "critical,digitalSignature,cRLSign,keyCertSign",
    )?;
    builder.append_extension(subject_key_identifier)?;
    builder.append_extension(authority_key_identifier)?;
    builder.append_extension(basic_constraints)?;
    builder.append_extension(key_usage)?;
    builder.sign(&signer.0, MessageDigest::sha256())?;
    let cert = builder.build();
    Ok(cert)
}

#[instrument]
pub fn make_leaf_cert(
    signer: (&PKey<Private>, &X509),
    applicant: (&PKey<Private>, &str, Option<&PackageId>),
) -> Result<X509, Error> {
    let mut builder = X509Builder::new()?;
    builder.set_version(CERTIFICATE_VERSION)?;

    let embargo = Asn1Time::days_from_now(0)?;
    builder.set_not_before(&embargo)?;

    // Google Apple and Mozilla reject certificate horizons longer than 397 days
    // https://techbeacon.com/security/google-apple-mozilla-enforce-1-year-max-security-certifications
    let expiration = Asn1Time::days_from_now(397)?;
    builder.set_not_after(&expiration)?;

    builder.set_serial_number(&*rand_serial()?)?;

    let mut subject_name_builder = X509NameBuilder::new()?;
    subject_name_builder.append_entry_by_text("CN", &format!("{}.local", &applicant.1))?;
    subject_name_builder.append_entry_by_text("O", "Start9")?;
    subject_name_builder.append_entry_by_text("OU", "Embassy")?;
    let subject_name = subject_name_builder.build();
    builder.set_subject_name(&subject_name)?;

    builder.set_issuer_name(signer.1.subject_name())?;

    builder.set_pubkey(&applicant.0)?;

    // Extensions
    let cfg = conf::Conf::new(conf::ConfMethod::default())?;
    let ctx = builder.x509v3_context(Some(&signer.1), Some(&cfg));
    // subjectKeyIdentifier = hash
    let subject_key_identifier =
        X509Extension::new_nid(Some(&cfg), Some(&ctx), Nid::SUBJECT_KEY_IDENTIFIER, "hash")?;
    // authorityKeyIdentifier = keyid:always,issuer
    let authority_key_identifier = X509Extension::new_nid(
        Some(&cfg),
        Some(&ctx),
        Nid::AUTHORITY_KEY_IDENTIFIER,
        "keyid,issuer:always",
    )?;
    let basic_constraints =
        X509Extension::new_nid(Some(&cfg), Some(&ctx), Nid::BASIC_CONSTRAINTS, "CA:FALSE")?;
    let key_usage = X509Extension::new_nid(
        Some(&cfg),
        Some(&ctx),
        Nid::KEY_USAGE,
        "critical,digitalSignature,keyEncipherment",
    )?;

    let applicant_dot_embassy = applicant
        .2
        .map(|id| format!("{id}.embassy"))
        .unwrap_or_else(|| "embassy".to_owned());
    let subject_alt_name = X509Extension::new_nid(
        Some(&cfg),
        Some(&ctx),
        Nid::SUBJECT_ALT_NAME,
        &format!(
            "DNS:{applicant_pubkey}.local,DNS:*.{applicant_pubkey}.local,DNS:{applicant_pubkey}.onion,DNS:*.{applicant_pubkey}.onion,DNS:{applicant_dot_embassy},DNS:*.{applicant_dot_embassy}",
            applicant_pubkey = &applicant.1,
        ),
    )?;
    builder.append_extension(subject_key_identifier)?;
    builder.append_extension(authority_key_identifier)?;
    builder.append_extension(subject_alt_name)?;
    builder.append_extension(basic_constraints)?;
    builder.append_extension(key_usage)?;

    builder.sign(&signer.0, MessageDigest::sha256())?;

    let cert = builder.build();
    Ok(cert)
}
