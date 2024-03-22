use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::net::IpAddr;
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

use futures::FutureExt;
use imbl_value::InternedString;
use libc::time_t;
use openssl::asn1::{Asn1Integer, Asn1Time};
use openssl::bn::{BigNum, MsbOption};
use openssl::ec::{EcGroup, EcKey};
use openssl::hash::MessageDigest;
use openssl::nid::Nid;
use openssl::pkey::{PKey, Private};
use openssl::x509::{X509Builder, X509Extension, X509NameBuilder, X509};
use openssl::*;
use patch_db::HasModel;
use serde::{Deserialize, Serialize};
use tokio::sync::Mutex;
use tracing::instrument;

use crate::account::AccountInfo;
use crate::hostname::Hostname;
use crate::init::check_time_is_synchronized;
use crate::prelude::*;
use crate::util::serde::Pem;
use crate::SOURCE_DATE;

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[model = "Model<Self>"]
#[serde(rename_all = "camelCase")]
pub struct CertStore {
    pub root_key: Pem<PKey<Private>>,
    pub root_cert: Pem<X509>,
    pub int_key: Pem<PKey<Private>>,
    pub int_cert: Pem<X509>,
    pub leaves: BTreeMap<JsonKey<BTreeSet<InternedString>>, CertData>,
}
impl CertStore {
    pub fn new(account: &AccountInfo) -> Result<Self, Error> {
        let int_key = generate_key()?;
        let int_cert = make_int_cert((&account.root_ca_key, &account.root_ca_cert), &int_key)?;
        Ok(Self {
            root_key: Pem::new(account.root_ca_key.clone()),
            root_cert: Pem::new(account.root_ca_cert.clone()),
            int_key: Pem::new(int_key),
            int_cert: Pem::new(int_cert),
            leaves: BTreeMap::new(),
        })
    }
}
impl Model<CertStore> {
    /// This function will grant any cert for any domain. It is up to the *caller* to enusure that the calling service has permission to sign a cert for the requested domain
    pub fn cert_for(
        &mut self,
        hostnames: &BTreeSet<InternedString>,
    ) -> Result<FullchainCertData, Error> {
        let keys = if let Some(cert_data) = self
            .as_leaves()
            .as_idx(JsonKey::new_ref(hostnames))
            .map(|m| m.de())
            .transpose()?
        {
            if cert_data
                .certs
                .ed25519
                .not_before()
                .compare(Asn1Time::days_from_now(0)?.as_ref())?
                == Ordering::Less
                && cert_data
                    .certs
                    .ed25519
                    .not_after()
                    .compare(Asn1Time::days_from_now(30)?.as_ref())?
                    == Ordering::Greater
                && cert_data
                    .certs
                    .nistp256
                    .not_before()
                    .compare(Asn1Time::days_from_now(0)?.as_ref())?
                    == Ordering::Less
                && cert_data
                    .certs
                    .nistp256
                    .not_after()
                    .compare(Asn1Time::days_from_now(30)?.as_ref())?
                    == Ordering::Greater
            {
                return Ok(FullchainCertData {
                    root: self.as_root_cert().de()?.0,
                    int: self.as_int_cert().de()?.0,
                    leaf: cert_data,
                });
            }
            cert_data.keys
        } else {
            PKeyPair {
                ed25519: PKey::generate_ed25519()?,
                nistp256: PKey::from_ec_key(EcKey::generate(&*EcGroup::from_curve_name(
                    Nid::X9_62_PRIME256V1,
                )?)?)?,
            }
        };
        let int_key = self.as_int_key().de()?.0;
        let int_cert = self.as_int_cert().de()?.0;
        let cert_data = CertData {
            certs: CertPair {
                ed25519: make_leaf_cert(
                    (&int_key, &int_cert),
                    (&keys.ed25519, &SANInfo::new(hostnames)),
                )?,
                nistp256: make_leaf_cert(
                    (&int_key, &int_cert),
                    (&keys.nistp256, &SANInfo::new(hostnames)),
                )?,
            },
            keys,
        };
        self.as_leaves_mut()
            .insert(JsonKey::new_ref(hostnames), &cert_data)?;
        Ok(FullchainCertData {
            root: self.as_root_cert().de()?.0,
            int: self.as_int_cert().de()?.0,
            leaf: cert_data,
        })
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CertData {
    pub keys: PKeyPair,
    pub certs: CertPair,
}

pub struct FullchainCertData {
    pub root: X509,
    pub int: X509,
    pub leaf: CertData,
}
impl FullchainCertData {
    pub fn fullchain_ed25519(&self) -> Vec<&X509> {
        vec![&self.leaf.certs.ed25519, &self.int, &self.root]
    }
    pub fn fullchain_nistp256(&self) -> Vec<&X509> {
        vec![&self.leaf.certs.nistp256, &self.int, &self.root]
    }
}

static CERTIFICATE_VERSION: i32 = 2; // X509 version 3 is actually encoded as '2' in the cert because fuck you.

fn unix_time(time: SystemTime) -> time_t {
    time.duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as time_t)
        .or_else(|_| UNIX_EPOCH.elapsed().map(|d| -(d.as_secs() as time_t)))
        .unwrap_or_default()
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PKeyPair {
    #[serde(with = "crate::util::serde::pem")]
    pub ed25519: PKey<Private>,
    #[serde(with = "crate::util::serde::pem")]
    pub nistp256: PKey<Private>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize)]
pub struct CertPair {
    #[serde(with = "crate::util::serde::pem")]
    pub ed25519: X509,
    #[serde(with = "crate::util::serde::pem")]
    pub nistp256: X509,
}

pub async fn root_ca_start_time() -> Result<SystemTime, Error> {
    Ok(if check_time_is_synchronized().await? {
        SystemTime::now()
    } else {
        *SOURCE_DATE
    })
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

#[instrument(skip_all)]
fn rand_serial() -> Result<Asn1Integer, Error> {
    let mut bn = BigNum::new()?;
    bn.rand(64, MsbOption::MAYBE_ZERO, false)?;
    let asn1 = Asn1Integer::from_bn(&bn)?;
    Ok(asn1)
}
#[instrument(skip_all)]
pub fn generate_key() -> Result<PKey<Private>, Error> {
    let new_key = EcKey::generate(EC_GROUP.as_ref())?;
    let key = PKey::from_ec_key(new_key)?;
    Ok(key)
}

#[instrument(skip_all)]
pub fn make_root_cert(
    root_key: &PKey<Private>,
    hostname: &Hostname,
    start_time: SystemTime,
) -> Result<X509, Error> {
    let mut builder = X509Builder::new()?;
    builder.set_version(CERTIFICATE_VERSION)?;

    let unix_start_time = unix_time(start_time);

    let embargo = Asn1Time::from_unix(unix_start_time - 86400)?;
    builder.set_not_before(&embargo)?;

    let expiration = Asn1Time::from_unix(unix_start_time + (10 * 364 * 86400))?;
    builder.set_not_after(&expiration)?;

    builder.set_serial_number(&*rand_serial()?)?;

    let mut subject_name_builder = X509NameBuilder::new()?;
    subject_name_builder.append_entry_by_text("CN", &format!("{} Local Root CA", &*hostname.0))?;
    subject_name_builder.append_entry_by_text("O", "Start9")?;
    subject_name_builder.append_entry_by_text("OU", "StartOS")?;
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
#[instrument(skip_all)]
pub fn make_int_cert(
    signer: (&PKey<Private>, &X509),
    applicant: &PKey<Private>,
) -> Result<X509, Error> {
    let mut builder = X509Builder::new()?;
    builder.set_version(CERTIFICATE_VERSION)?;

    builder.set_not_before(signer.1.not_before())?;

    builder.set_not_after(signer.1.not_after())?;

    builder.set_serial_number(&*rand_serial()?)?;

    let mut subject_name_builder = X509NameBuilder::new()?;
    subject_name_builder.append_entry_by_text("CN", "StartOS Local Intermediate CA")?;
    subject_name_builder.append_entry_by_text("O", "Start9")?;
    subject_name_builder.append_entry_by_text("OU", "StartOS")?;
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum MaybeWildcard {
    WithWildcard(String),
    WithoutWildcard(InternedString),
}
impl MaybeWildcard {
    pub fn as_str(&self) -> &str {
        match self {
            MaybeWildcard::WithWildcard(s) => s.as_str(),
            MaybeWildcard::WithoutWildcard(s) => &**s,
        }
    }
}
impl std::fmt::Display for MaybeWildcard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MaybeWildcard::WithWildcard(dns) => write!(f, "DNS:{dns},DNS:*.{dns}"),
            MaybeWildcard::WithoutWildcard(dns) => write!(f, "DNS:{dns}"),
        }
    }
}

#[derive(Debug)]
pub struct SANInfo {
    pub dns: BTreeSet<MaybeWildcard>,
    pub ips: BTreeSet<IpAddr>,
}
impl SANInfo {
    pub fn new(hostnames: &BTreeSet<InternedString>) -> Self {
        let mut dns = BTreeSet::new();
        let mut ips = BTreeSet::new();
        for hostname in hostnames {
            if let Ok(ip) = hostname.parse::<IpAddr>() {
                ips.insert(ip);
            } else {
                dns.insert(MaybeWildcard::WithoutWildcard(hostname.clone())); // TODO: wildcards?
            }
        }
        Self { dns, ips }
    }
}
impl std::fmt::Display for SANInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut written = false;
        for dns in &self.dns {
            if written {
                write!(f, ",")?;
            }
            written = true;
            write!(f, "{dns}")?;
        }
        for ip in &self.ips {
            if written {
                write!(f, ",")?;
            }
            written = true;
            write!(f, "IP:{ip}")?;
        }
        Ok(())
    }
}

#[instrument(skip_all)]
pub fn make_leaf_cert(
    signer: (&PKey<Private>, &X509),
    applicant: (&PKey<Private>, &SANInfo),
) -> Result<X509, Error> {
    let mut builder = X509Builder::new()?;
    builder.set_version(CERTIFICATE_VERSION)?;

    let embargo = Asn1Time::from_unix(unix_time(SystemTime::now()) - 86400)?;
    builder.set_not_before(&embargo)?;

    // Google Apple and Mozilla reject certificate horizons longer than 398 days
    // https://techbeacon.com/security/google-apple-mozilla-enforce-1-year-max-security-certifications
    let expiration = Asn1Time::days_from_now(397)?;
    builder.set_not_after(&expiration)?;

    builder.set_serial_number(&*rand_serial()?)?;

    let mut subject_name_builder = X509NameBuilder::new()?;
    subject_name_builder.append_entry_by_text(
        "CN",
        applicant
            .1
            .dns
            .first()
            .map(MaybeWildcard::as_str)
            .unwrap_or("localhost"),
    )?;
    subject_name_builder.append_entry_by_text("O", "Start9")?;
    subject_name_builder.append_entry_by_text("OU", "StartOS")?;
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

    let san_string = applicant.1.to_string();
    let subject_alt_name =
        X509Extension::new_nid(Some(&cfg), Some(&ctx), Nid::SUBJECT_ALT_NAME, &san_string)?;
    builder.append_extension(subject_key_identifier)?;
    builder.append_extension(authority_key_identifier)?;
    builder.append_extension(subject_alt_name)?;
    builder.append_extension(basic_constraints)?;
    builder.append_extension(key_usage)?;

    builder.sign(&signer.0, MessageDigest::sha256())?;

    let cert = builder.build();
    Ok(cert)
}
