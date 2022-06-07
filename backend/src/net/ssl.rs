use std::cmp::Ordering;
use std::path::Path;

use color_eyre::eyre::eyre;
use futures::FutureExt;
use openssl::asn1::{Asn1Integer, Asn1Time};
use openssl::bn::{BigNum, MsbOption};
use openssl::ec::{EcGroup, EcKey};
use openssl::hash::MessageDigest;
use openssl::nid::Nid;
use openssl::pkey::{PKey, Private};
use openssl::x509::{X509Builder, X509Extension, X509NameBuilder, X509};
use openssl::*;
use sqlx::SqlitePool;
use tokio::sync::Mutex;
use tracing::instrument;

use crate::s9pk::manifest::PackageId;
use crate::{Error, ErrorKind, ResultExt};

static CERTIFICATE_VERSION: i32 = 2; // X509 version 3 is actually encoded as '2' in the cert because fuck you.
pub const ROOT_CA_STATIC_PATH: &str = "/var/lib/embassy/ssl/root-ca.crt";

#[derive(Debug)]
pub struct SslManager {
    store: SslStore,
    root_cert: X509,
    int_key: PKey<Private>,
    int_cert: X509,
}

#[derive(Debug)]
struct SslStore {
    secret_store: SqlitePool,
}
impl SslStore {
    fn new(db: SqlitePool) -> Result<Self, Error> {
        Ok(SslStore { secret_store: db })
    }
    #[instrument(skip(self))]
    async fn save_root_certificate(&self, key: &PKey<Private>, cert: &X509) -> Result<(), Error> {
        let key_str = String::from_utf8(key.private_key_to_pem_pkcs8()?)?;
        let cert_str = String::from_utf8(cert.to_pem()?)?;
        let _n = sqlx::query!("INSERT INTO certificates (id, priv_key_pem, certificate_pem, lookup_string, created_at, updated_at) VALUES (0, ?, ?, NULL, datetime('now'), datetime('now'))", key_str, cert_str).execute(&self.secret_store).await?;
        Ok(())
    }
    #[instrument(skip(self))]
    async fn load_root_certificate(&self) -> Result<Option<(PKey<Private>, X509)>, Error> {
        let m_row =
            sqlx::query!("SELECT priv_key_pem, certificate_pem FROM certificates WHERE id = 0;")
                .fetch_optional(&self.secret_store)
                .await?;
        match m_row {
            None => Ok(None),
            Some(row) => {
                let priv_key = PKey::private_key_from_pem(&row.priv_key_pem.into_bytes())?;
                let certificate = X509::from_pem(&row.certificate_pem.into_bytes())?;
                Ok(Some((priv_key, certificate)))
            }
        }
    }
    #[instrument(skip(self))]
    async fn save_intermediate_certificate(
        &self,
        key: &PKey<Private>,
        cert: &X509,
    ) -> Result<(), Error> {
        let key_str = String::from_utf8(key.private_key_to_pem_pkcs8()?)?;
        let cert_str = String::from_utf8(cert.to_pem()?)?;
        let _n = sqlx::query!("INSERT INTO certificates (id, priv_key_pem, certificate_pem, lookup_string, created_at, updated_at) VALUES (1, ?, ?, NULL, datetime('now'), datetime('now'))", key_str, cert_str).execute(&self.secret_store).await?;
        Ok(())
    }
    async fn load_intermediate_certificate(&self) -> Result<Option<(PKey<Private>, X509)>, Error> {
        let m_row =
            sqlx::query!("SELECT priv_key_pem, certificate_pem FROM certificates WHERE id = 1;")
                .fetch_optional(&self.secret_store)
                .await?;
        match m_row {
            None => Ok(None),
            Some(row) => {
                let priv_key = PKey::private_key_from_pem(&row.priv_key_pem.into_bytes())?;
                let certificate = X509::from_pem(&row.certificate_pem.into_bytes())?;
                Ok(Some((priv_key, certificate)))
            }
        }
    }
    #[instrument(skip(self))]
    async fn import_root_certificate(
        &self,
        root_key: &PKey<Private>,
        root_cert: &X509,
    ) -> Result<(), Error> {
        // remove records for both root and intermediate CA
        sqlx::query!("DELETE FROM certificates WHERE id = 0 OR id = 1;")
            .execute(&self.secret_store)
            .await?;
        self.save_root_certificate(root_key, root_cert).await?;
        Ok(())
    }
    #[instrument(skip(self))]
    async fn save_certificate(
        &self,
        key: &PKey<Private>,
        cert: &X509,
        lookup_string: &str,
    ) -> Result<(), Error> {
        let key_str = String::from_utf8(key.private_key_to_pem_pkcs8()?)?;
        let cert_str = String::from_utf8(cert.to_pem()?)?;
        let _n = sqlx::query!("INSERT INTO certificates (priv_key_pem, certificate_pem, lookup_string, created_at, updated_at) VALUES (?, ?, ?, datetime('now'), datetime('now'))", key_str, cert_str, lookup_string).execute(&self.secret_store).await?;
        Ok(())
    }
    async fn load_certificate(
        &self,
        lookup_string: &str,
    ) -> Result<Option<(PKey<Private>, X509)>, Error> {
        let m_row = sqlx::query!(
            "SELECT priv_key_pem, certificate_pem FROM certificates WHERE lookup_string = ?",
            lookup_string
        )
        .fetch_optional(&self.secret_store)
        .await?;
        match m_row {
            None => Ok(None),
            Some(row) => {
                let priv_key = PKey::private_key_from_pem(&row.priv_key_pem.into_bytes())?;
                let certificate = X509::from_pem(&row.certificate_pem.into_bytes())?;
                Ok(Some((priv_key, certificate)))
            }
        }
    }
    #[instrument(skip(self))]
    async fn update_certificate(
        &self,
        key: &PKey<Private>,
        cert: &X509,
        lookup_string: &str,
    ) -> Result<(), Error> {
        let key_str = String::from_utf8(key.private_key_to_pem_pkcs8()?)?;
        let cert_str = String::from_utf8(cert.to_pem()?)?;
        let n = sqlx::query!("UPDATE certificates SET priv_key_pem = ?, certificate_pem = ?, updated_at = datetime('now') WHERE lookup_string = ?", key_str, cert_str, lookup_string).execute(&self.secret_store).await?;
        if n.rows_affected() == 0 {
            return Err(Error::new(
                eyre!(
                    "Attempted to update non-existent certificate: {}",
                    lookup_string
                ),
                ErrorKind::OpenSsl,
            ));
        }
        Ok(())
    }
}

const EC_CURVE_NAME: nid::Nid = nid::Nid::X9_62_PRIME256V1;
lazy_static::lazy_static! {
    static ref EC_GROUP: EcGroup = EcGroup::from_curve_name(EC_CURVE_NAME).unwrap();
    static ref SSL_MUTEX: Mutex<()> = Mutex::new(()); // TODO: make thread safe
}

impl SslManager {
    #[instrument(skip(db))]
    pub async fn init(db: SqlitePool) -> Result<Self, Error> {
        let store = SslStore::new(db)?;
        let (root_key, root_cert) = match store.load_root_certificate().await? {
            None => {
                let root_key = generate_key()?;
                let server_id = crate::hostname::get_id().await?;
                let root_cert = make_root_cert(&root_key, &server_id)?;
                store.save_root_certificate(&root_key, &root_cert).await?;
                Ok::<_, Error>((root_key, root_cert))
            }
            Some((key, cert)) => Ok((key, cert)),
        }?;
        // generate static file for download, this will get blown up on embassy restart so it's good to write it on
        // every ssl manager init
        tokio::fs::create_dir_all(
            Path::new(ROOT_CA_STATIC_PATH)
                .parent()
                .unwrap_or(Path::new("/")),
        )
        .await?;
        tokio::fs::write(ROOT_CA_STATIC_PATH, root_cert.to_pem()?).await?;
        let (int_key, int_cert) = match store.load_intermediate_certificate().await? {
            None => {
                let int_key = generate_key()?;
                let int_cert = make_int_cert((&root_key, &root_cert), &int_key)?;
                store
                    .save_intermediate_certificate(&int_key, &int_cert)
                    .await?;
                Ok::<_, Error>((int_key, int_cert))
            }
            Some((key, cert)) => Ok((key, cert)),
        }?;
        Ok(SslManager {
            store,
            root_cert,
            int_key,
            int_cert,
        })
    }

    // TODO: currently the burden of proof is on the caller to ensure that all of the arguments to this function are
    // consistent. The following properties are assumed and not verified:
    // 1. `root_cert` is self-signed and contains the public key that matches the private key `root_key`
    // 2. certificate is not past its expiration date
    // Warning: If this function ever fails, you must either call it again or regenerate your certificates from scratch
    // since it is possible for it to fail after successfully saving the root certificate but before successfully saving
    // the intermediate certificate
    #[instrument(skip(db))]
    pub async fn import_root_ca(
        db: SqlitePool,
        root_key: PKey<Private>,
        root_cert: X509,
    ) -> Result<Self, Error> {
        let store = SslStore::new(db)?;
        store.import_root_certificate(&root_key, &root_cert).await?;
        let int_key = generate_key()?;
        let int_cert = make_int_cert((&root_key, &root_cert), &int_key)?;
        store
            .save_intermediate_certificate(&int_key, &int_cert)
            .await?;
        Ok(SslManager {
            store,
            root_cert,
            int_key,
            int_cert,
        })
    }

    #[instrument(skip(self))]
    pub async fn export_root_ca(&self) -> Result<(PKey<Private>, X509), Error> {
        match self.store.load_root_certificate().await? {
            None => Err(Error::new(
                eyre!("Failed to export root certificate: root certificate has not been generated"),
                ErrorKind::OpenSsl,
            )),
            Some(a) => Ok(a),
        }
    }

    #[instrument(skip(self))]
    pub async fn certificate_for(
        &self,
        dns_base: &str,
        package_id: &PackageId,
    ) -> Result<(PKey<Private>, Vec<X509>), Error> {
        let (key, cert) = match self.store.load_certificate(dns_base).await? {
            None => {
                let key = generate_key()?;
                let cert = make_leaf_cert(
                    (&self.int_key, &self.int_cert),
                    (&key, dns_base, package_id),
                )?;
                self.store.save_certificate(&key, &cert, dns_base).await?;
                Ok::<_, Error>((key, cert))
            }
            Some((key, cert)) => {
                let window_end = Asn1Time::days_from_now(30)?;
                let expiration = cert.not_after();
                if expiration.compare(&window_end)? == Ordering::Less {
                    let key = generate_key()?;
                    let cert = make_leaf_cert(
                        (&self.int_key, &self.int_cert),
                        (&key, dns_base, package_id),
                    )?;
                    self.store.update_certificate(&key, &cert, dns_base).await?;
                    Ok((key, cert))
                } else {
                    Ok((key, cert))
                }
            }
        }?;
        Ok((
            key,
            vec![cert, self.int_cert.clone(), self.root_cert.clone()],
        ))
    }
}

pub async fn export_key(key: &PKey<Private>, target: &Path) -> Result<(), Error> {
    tokio::fs::write(target, key.private_key_to_pem_pkcs8()?)
        .map(|res| res.with_ctx(|_| (ErrorKind::Filesystem, target.display().to_string())))
        .await?;
    Ok(())
}
pub async fn export_cert(chain: &Vec<X509>, target: &Path) -> Result<(), Error> {
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
fn generate_key() -> Result<PKey<Private>, Error> {
    let new_key = EcKey::generate(EC_GROUP.as_ref())?;
    let key = PKey::from_ec_key(new_key)?;
    Ok(key)
}
#[instrument]
fn make_root_cert(root_key: &PKey<Private>, server_id: &str) -> Result<X509, Error> {
    let mut builder = X509Builder::new()?;
    builder.set_version(CERTIFICATE_VERSION)?;

    let embargo = Asn1Time::days_from_now(0)?;
    builder.set_not_before(&embargo)?;

    let expiration = Asn1Time::days_from_now(3650)?;
    builder.set_not_after(&expiration)?;

    builder.set_serial_number(&*rand_serial()?)?;

    let mut subject_name_builder = X509NameBuilder::new()?;
    subject_name_builder
        .append_entry_by_text("CN", &format!("Embassy Local Root CA ({})", server_id))?;
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
fn make_int_cert(
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
fn make_leaf_cert(
    signer: (&PKey<Private>, &X509),
    applicant: (&PKey<Private>, &str, &PackageId),
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

    let subject_alt_name = X509Extension::new_nid(
        Some(&cfg),
        Some(&ctx),
        Nid::SUBJECT_ALT_NAME,
        &format!(
            "DNS:{}.local,DNS:*.{}.local,DNS:{}.onion,DNS:*.{}.onion,DNS:{}.embassy,DNS:*.{}.embassy",
            &applicant.1, &applicant.1, &applicant.1, &applicant.1, &applicant.2, &applicant.2,
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

#[tokio::test]
async fn ca_details_persist() -> Result<(), Error> {
    let pool = sqlx::Pool::<sqlx::Sqlite>::connect("sqlite::memory:").await?;
    sqlx::query_file!("migrations/20210629193146_Init.sql")
        .execute(&pool)
        .await?;
    let mgr = SslManager::init(pool.clone()).await?;
    let root_cert0 = mgr.root_cert;
    let int_key0 = mgr.int_key;
    let int_cert0 = mgr.int_cert;
    let mgr = SslManager::init(pool).await?;
    let root_cert1 = mgr.root_cert;
    let int_key1 = mgr.int_key;
    let int_cert1 = mgr.int_cert;

    assert_eq!(root_cert0.to_pem()?, root_cert1.to_pem()?);
    assert_eq!(
        int_key0.private_key_to_pem_pkcs8()?,
        int_key1.private_key_to_pem_pkcs8()?
    );
    assert_eq!(int_cert0.to_pem()?, int_cert1.to_pem()?);
    Ok(())
}

#[tokio::test]
async fn certificate_details_persist() -> Result<(), Error> {
    let pool = sqlx::Pool::<sqlx::Sqlite>::connect("sqlite::memory:").await?;
    sqlx::query_file!("migrations/20210629193146_Init.sql")
        .execute(&pool)
        .await?;
    let mgr = SslManager::init(pool.clone()).await?;
    let package_id = "bitcoind".parse().unwrap();
    let (key0, cert_chain0) = mgr.certificate_for("start9", &package_id).await?;
    let (key1, cert_chain1) = mgr.certificate_for("start9", &package_id).await?;

    assert_eq!(
        key0.private_key_to_pem_pkcs8()?,
        key1.private_key_to_pem_pkcs8()?
    );
    assert_eq!(
        cert_chain0
            .iter()
            .map(|cert| cert.to_pem().unwrap())
            .collect::<Vec<Vec<u8>>>(),
        cert_chain1
            .iter()
            .map(|cert| cert.to_pem().unwrap())
            .collect::<Vec<Vec<u8>>>()
    );
    Ok(())
}
