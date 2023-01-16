use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::net::IpAddr;
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

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
use crate::net::dhcp::ips;
use crate::net::keys::{Key, KeyInfo};
use crate::s9pk::manifest::PackageId;
use crate::{Error, ErrorKind, ResultExt};

static CERTIFICATE_VERSION: i32 = 2; // X509 version 3 is actually encoded as '2' in the cert because fuck you.

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct CertPair {
    pub ed25519: X509,
    pub nistp256: X509,
}
impl CertPair {
    fn updated(
        pair: Option<&Self>,
        hostname: &Hostname,
        signer: (&PKey<Private>, &X509),
        applicant: &Key,
        ip: BTreeSet<IpAddr>,
    ) -> Result<(Self, bool), Error> {
        let mut updated = false;
        let mut updated_cert = |cert: Option<&X509>, osk: PKey<Private>| -> Result<X509, Error> {
            let mut ips = BTreeSet::new();
            if let Some(cert) = cert {
                ips.extend(
                    cert.subject_alt_names()
                        .iter()
                        .flatten()
                        .filter_map(|a| a.ipaddress())
                        .filter_map(|a| match a.len() {
                            4 => Some::<IpAddr>(<[u8; 4]>::try_from(a).unwrap().into()),
                            16 => Some::<IpAddr>(<[u8; 16]>::try_from(a).unwrap().into()),
                            _ => None,
                        }),
                );
                if cert
                    .not_after()
                    .compare(Asn1Time::days_from_now(30)?.as_ref())?
                    == Ordering::Greater
                    && ips.is_superset(&ip)
                {
                    return Ok(cert.clone());
                }
            }
            ips.extend(ip.iter().copied());
            updated = true;
            make_leaf_cert(signer, (&osk, &SANInfo::new(&applicant, hostname, ips)))
        };
        Ok((
            Self {
                ed25519: updated_cert(pair.map(|c| &c.ed25519), applicant.openssl_key_ed25519())?,
                nistp256: updated_cert(
                    pair.map(|c| &c.nistp256),
                    applicant.openssl_key_nistp256(),
                )?,
            },
            updated,
        ))
    }
}

#[derive(Debug)]
pub struct SslManager {
    hostname: Hostname,
    root_cert: X509,
    int_key: PKey<Private>,
    int_cert: X509,
    cert_cache: RwLock<BTreeMap<Key, CertPair>>,
}
impl SslManager {
    pub fn new(account: &AccountInfo) -> Result<Self, Error> {
        let int_key = generate_key()?;
        let int_cert = make_int_cert((&account.root_ca_key, &account.root_ca_cert), &int_key)?;
        Ok(Self {
            hostname: account.hostname.clone(),
            root_cert: account.root_ca_cert.clone(),
            int_key,
            int_cert,
            cert_cache: RwLock::new(BTreeMap::new()),
        })
    }
    pub async fn with_certs(&self, key: Key, ip: IpAddr) -> Result<KeyInfo, Error> {
        let mut ips = ips().await?;
        ips.insert(ip);
        let (pair, updated) = CertPair::updated(
            self.cert_cache.read().await.get(&key),
            &self.hostname,
            (&self.int_key, &self.int_cert),
            &key,
            ips,
        )?;
        if updated {
            self.cert_cache
                .write()
                .await
                .insert(key.clone(), pair.clone());
        }

        Ok(key.with_certs(pair, self.int_cert.clone(), self.root_cert.clone()))
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
#[instrument(skip_all)]
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum MaybeWildcard {
    WithWildcard(String),
    WithoutWildcard(String),
}
impl MaybeWildcard {
    pub fn as_str(&self) -> &str {
        match self {
            MaybeWildcard::WithWildcard(s) => s.as_str(),
            MaybeWildcard::WithoutWildcard(s) => s.as_str(),
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
    pub fn new(key: &Key, hostname: &Hostname, ips: BTreeSet<IpAddr>) -> Self {
        let mut dns = BTreeSet::new();
        if let Some((id, _)) = key.interface() {
            dns.insert(MaybeWildcard::WithWildcard(format!("{id}.embassy")));
            dns.insert(MaybeWildcard::WithWildcard(key.local_address().to_string()));
        } else {
            dns.insert(MaybeWildcard::WithoutWildcard("embassy".to_owned()));
            dns.insert(MaybeWildcard::WithWildcard(hostname.local_domain_name()));
            dns.insert(MaybeWildcard::WithoutWildcard(hostname.no_dot_host_name()));
            dns.insert(MaybeWildcard::WithoutWildcard("localhost".to_owned()));
        }
        dns.insert(MaybeWildcard::WithWildcard(key.tor_address().to_string()));
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

    let embargo = Asn1Time::from_unix(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs() as i64)
            .or_else(|_| UNIX_EPOCH.elapsed().map(|d| -(d.as_secs() as i64)))
            .unwrap_or_default()
            - 86400,
    )?;
    builder.set_not_before(&embargo)?;

    // Google Apple and Mozilla reject certificate horizons longer than 397 days
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
