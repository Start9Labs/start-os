use std::fs;
use std::net::Ipv4Addr;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use openssl::asn1::{Asn1Integer, Asn1Time};
use openssl::bn::{BigNum, MsbOption};
use openssl::ec::{EcGroup, EcKey};
use openssl::hash::MessageDigest;
use openssl::nid::Nid;
use openssl::pkey::{PKey, Private};
use openssl::x509::extension::{
    AuthorityKeyIdentifier, BasicConstraints, KeyUsage,
    SubjectAlternativeName, SubjectKeyIdentifier,
};
use openssl::x509::{X509Builder, X509};
use openssl::x509::X509NameBuilder;

use uciedit::openwrt::NetworkInterface;
use uciedit::{parse_all, Arena};

use crate::Error;

/// Directory for CA and server certificates.
const SSL_CERT_DIR: &str = "/etc/ssl/certs";
const SSL_KEY_DIR: &str = "/etc/ssl/private";

const CA_CERT_FILENAME: &str = "startwrt-ca.pem";
const CA_KEY_FILENAME: &str = "startwrt-ca.key";
const INT_CERT_FILENAME: &str = "startwrt-int.pem";
const INT_KEY_FILENAME: &str = "startwrt-int.key";
const SERVER_CERT_FILENAME: &str = "startwrt-server.pem";
const SERVER_KEY_FILENAME: &str = "startwrt-server.key";

/// Default SAN hostname for the router.
const ROUTER_HOSTNAME: &str = "router.lan";

/// Renew the leaf cert if it expires within this many days.
const RENEWAL_THRESHOLD_DAYS: u32 = 30;

/// X509 version 3 is encoded as 2.
const X509_VERSION_3: i32 = 2;

pub fn ca_cert_path() -> PathBuf {
    Path::new(SSL_CERT_DIR).join(CA_CERT_FILENAME)
}

fn ca_key_path() -> PathBuf {
    Path::new(SSL_KEY_DIR).join(CA_KEY_FILENAME)
}

fn int_cert_path() -> PathBuf {
    Path::new(SSL_CERT_DIR).join(INT_CERT_FILENAME)
}

fn int_key_path() -> PathBuf {
    Path::new(SSL_KEY_DIR).join(INT_KEY_FILENAME)
}

pub fn server_cert_path() -> PathBuf {
    Path::new(SSL_CERT_DIR).join(SERVER_CERT_FILENAME)
}

pub fn server_key_path() -> PathBuf {
    Path::new(SSL_KEY_DIR).join(SERVER_KEY_FILENAME)
}

/// Ensure SSL directories exist with appropriate permissions.
fn ensure_dirs() -> Result<(), Error> {
    use std::os::unix::fs::DirBuilderExt;

    // Create /etc/ssl/certs with world-readable
    let mut builder = fs::DirBuilder::new();
    builder.recursive(true).mode(0o755);
    builder
        .create(SSL_CERT_DIR)
        .map_err(|e| Error::other(format!("failed to create {SSL_CERT_DIR}: {e}")))?;

    // Create /etc/ssl/private with restricted access — rebuild builder
    // to avoid applying 0o700 to the intermediate /etc/ssl/ directory
    let mut builder = fs::DirBuilder::new();
    builder.recursive(true).mode(0o700);
    builder
        .create(SSL_KEY_DIR)
        .map_err(|e| Error::other(format!("failed to create {SSL_KEY_DIR}: {e}")))?;

    Ok(())
}

/// Generate a random 64-bit serial number.
fn rand_serial() -> Result<Asn1Integer, Error> {
    let mut bn = BigNum::new()
        .map_err(|e| Error::other(format!("failed to create BigNum: {e}")))?;
    bn.rand(64, MsbOption::MAYBE_ZERO, false)
        .map_err(|e| Error::other(format!("failed to generate random serial: {e}")))?;
    Asn1Integer::from_bn(&bn)
        .map_err(|e| Error::other(format!("failed to convert serial to ASN1: {e}")))
}

/// Generate a NIST P-256 EC key pair.
fn gen_ec_key() -> Result<PKey<Private>, Error> {
    let group = EcGroup::from_curve_name(Nid::X9_62_PRIME256V1)
        .map_err(|e| Error::other(format!("failed to get EC group: {e}")))?;
    let ec_key = EcKey::generate(&group)
        .map_err(|e| Error::other(format!("failed to generate EC key: {e}")))?;
    PKey::from_ec_key(ec_key)
        .map_err(|e| Error::other(format!("failed to wrap EC key: {e}")))
}

/// Current unix timestamp.
fn unix_now() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs() as i64
}

/// Generate a new Root CA key pair and self-signed certificate.
/// Returns (cert_pem, key_pem).
fn generate_root_ca() -> Result<(String, String), Error> {
    let key = gen_ec_key()?;

    let mut builder = X509Builder::new()
        .map_err(|e| Error::other(format!("failed to create X509 builder: {e}")))?;
    builder.set_version(X509_VERSION_3)
        .map_err(|e| Error::other(format!("failed to set version: {e}")))?;

    let now = unix_now();
    let not_before = Asn1Time::from_unix(now - 86400)
        .map_err(|e| Error::other(format!("failed to create not_before: {e}")))?;
    let not_after = Asn1Time::from_unix(now + 3650 * 86400)
        .map_err(|e| Error::other(format!("failed to create not_after: {e}")))?;
    builder.set_not_before(&not_before)
        .map_err(|e| Error::other(format!("failed to set not_before: {e}")))?;
    builder.set_not_after(&not_after)
        .map_err(|e| Error::other(format!("failed to set not_after: {e}")))?;

    builder.set_serial_number(&*rand_serial()?)
        .map_err(|e| Error::other(format!("failed to set serial: {e}")))?;

    let mut name_builder = X509NameBuilder::new()
        .map_err(|e| Error::other(format!("failed to create name builder: {e}")))?;
    name_builder.append_entry_by_text("CN", "StartWRT Local Root CA")
        .map_err(|e| Error::other(format!("failed to set CN: {e}")))?;
    name_builder.append_entry_by_text("O", "Start9")
        .map_err(|e| Error::other(format!("failed to set O: {e}")))?;
    name_builder.append_entry_by_text("OU", "StartWRT")
        .map_err(|e| Error::other(format!("failed to set OU: {e}")))?;
    let subject_name = name_builder.build();
    builder.set_subject_name(&subject_name)
        .map_err(|e| Error::other(format!("failed to set subject: {e}")))?;
    builder.set_issuer_name(&subject_name)
        .map_err(|e| Error::other(format!("failed to set issuer: {e}")))?;

    builder.set_pubkey(&key)
        .map_err(|e| Error::other(format!("failed to set pubkey: {e}")))?;

    // Extensions
    let cfg = openssl::conf::Conf::new(openssl::conf::ConfMethod::default())
        .map_err(|e| Error::other(format!("failed to create conf: {e}")))?;
    let ctx = builder.x509v3_context(None, Some(&cfg));

    let ski = SubjectKeyIdentifier::new().build(&ctx)
        .map_err(|e| Error::other(format!("failed to build SKI: {e}")))?;
    let aki = AuthorityKeyIdentifier::new().keyid(false).issuer(true).build(&ctx)
        .map_err(|e| Error::other(format!("failed to build AKI: {e}")))?;
    let bc = BasicConstraints::new().critical().ca().build()
        .map_err(|e| Error::other(format!("failed to build basic constraints: {e}")))?;
    let ku = KeyUsage::new()
        .critical()
        .digital_signature()
        .crl_sign()
        .key_cert_sign()
        .build()
        .map_err(|e| Error::other(format!("failed to build key usage: {e}")))?;

    builder.append_extension(ski)
        .map_err(|e| Error::other(format!("failed to append SKI: {e}")))?;
    builder.append_extension(aki)
        .map_err(|e| Error::other(format!("failed to append AKI: {e}")))?;
    builder.append_extension(bc)
        .map_err(|e| Error::other(format!("failed to append basic constraints: {e}")))?;
    builder.append_extension(ku)
        .map_err(|e| Error::other(format!("failed to append key usage: {e}")))?;

    builder.sign(&key, MessageDigest::sha256())
        .map_err(|e| Error::other(format!("failed to sign CA cert: {e}")))?;

    let cert = builder.build();
    let cert_pem = String::from_utf8(cert.to_pem()
        .map_err(|e| Error::other(format!("failed to encode CA cert PEM: {e}")))?)
        .map_err(|e| Error::other(format!("CA cert PEM is not UTF-8: {e}")))?;
    let key_pem = String::from_utf8(key.private_key_to_pem_pkcs8()
        .map_err(|e| Error::other(format!("failed to encode CA key PEM: {e}")))?)
        .map_err(|e| Error::other(format!("CA key PEM is not UTF-8: {e}")))?;

    Ok((cert_pem, key_pem))
}

/// Generate an intermediate CA signed by the root CA.
/// Returns (cert_pem, key_pem).
fn generate_intermediate_ca(
    ca_cert_pem: &str,
    ca_key_pem: &str,
) -> Result<(String, String), Error> {
    let ca_cert = X509::from_pem(ca_cert_pem.as_bytes())
        .map_err(|e| Error::other(format!("failed to parse CA cert: {e}")))?;
    let ca_key = PKey::private_key_from_pem(ca_key_pem.as_bytes())
        .map_err(|e| Error::other(format!("failed to parse CA key: {e}")))?;

    let key = gen_ec_key()?;

    let mut builder = X509Builder::new()
        .map_err(|e| Error::other(format!("failed to create X509 builder: {e}")))?;
    builder.set_version(X509_VERSION_3)
        .map_err(|e| Error::other(format!("failed to set version: {e}")))?;

    // Match root CA validity period
    builder.set_not_before(ca_cert.not_before())
        .map_err(|e| Error::other(format!("failed to set not_before: {e}")))?;
    builder.set_not_after(ca_cert.not_after())
        .map_err(|e| Error::other(format!("failed to set not_after: {e}")))?;

    builder.set_serial_number(&*rand_serial()?)
        .map_err(|e| Error::other(format!("failed to set serial: {e}")))?;

    let mut name_builder = X509NameBuilder::new()
        .map_err(|e| Error::other(format!("failed to create name builder: {e}")))?;
    name_builder.append_entry_by_text("CN", "StartWRT Local Intermediate CA")
        .map_err(|e| Error::other(format!("failed to set CN: {e}")))?;
    name_builder.append_entry_by_text("O", "Start9")
        .map_err(|e| Error::other(format!("failed to set O: {e}")))?;
    name_builder.append_entry_by_text("OU", "StartWRT")
        .map_err(|e| Error::other(format!("failed to set OU: {e}")))?;
    let subject_name = name_builder.build();
    builder.set_subject_name(&subject_name)
        .map_err(|e| Error::other(format!("failed to set subject: {e}")))?;

    // Issuer is the root CA's subject
    builder.set_issuer_name(ca_cert.subject_name())
        .map_err(|e| Error::other(format!("failed to set issuer: {e}")))?;

    builder.set_pubkey(&key)
        .map_err(|e| Error::other(format!("failed to set pubkey: {e}")))?;

    // Extensions
    let cfg = openssl::conf::Conf::new(openssl::conf::ConfMethod::default())
        .map_err(|e| Error::other(format!("failed to create conf: {e}")))?;
    let ctx = builder.x509v3_context(Some(&ca_cert), Some(&cfg));

    let ski = SubjectKeyIdentifier::new().build(&ctx)
        .map_err(|e| Error::other(format!("failed to build SKI: {e}")))?;
    let aki = AuthorityKeyIdentifier::new().keyid(true).issuer(true).build(&ctx)
        .map_err(|e| Error::other(format!("failed to build AKI: {e}")))?;
    let bc = BasicConstraints::new().critical().ca().pathlen(0).build()
        .map_err(|e| Error::other(format!("failed to build basic constraints: {e}")))?;
    let ku = KeyUsage::new()
        .critical()
        .digital_signature()
        .crl_sign()
        .key_cert_sign()
        .build()
        .map_err(|e| Error::other(format!("failed to build key usage: {e}")))?;

    builder.append_extension(ski)
        .map_err(|e| Error::other(format!("failed to append SKI: {e}")))?;
    builder.append_extension(aki)
        .map_err(|e| Error::other(format!("failed to append AKI: {e}")))?;
    builder.append_extension(bc)
        .map_err(|e| Error::other(format!("failed to append basic constraints: {e}")))?;
    builder.append_extension(ku)
        .map_err(|e| Error::other(format!("failed to append key usage: {e}")))?;

    // Sign with the root CA's key
    builder.sign(&ca_key, MessageDigest::sha256())
        .map_err(|e| Error::other(format!("failed to sign intermediate cert: {e}")))?;

    let cert = builder.build();
    let cert_pem = String::from_utf8(cert.to_pem()
        .map_err(|e| Error::other(format!("failed to encode intermediate cert PEM: {e}")))?)
        .map_err(|e| Error::other(format!("intermediate cert PEM is not UTF-8: {e}")))?;
    let key_pem = String::from_utf8(key.private_key_to_pem_pkcs8()
        .map_err(|e| Error::other(format!("failed to encode intermediate key PEM: {e}")))?)
        .map_err(|e| Error::other(format!("intermediate key PEM is not UTF-8: {e}")))?;

    Ok((cert_pem, key_pem))
}

/// Generate a leaf certificate signed by the intermediate CA.
/// SANs include the given LAN IP and `router.lan`.
/// Returns (leaf_cert_pem + int_cert_pem + ca_cert_pem chain, key_pem).
fn generate_leaf_cert(
    int_cert_pem: &str,
    int_key_pem: &str,
    ca_cert_pem: &str,
    lan_ip: Ipv4Addr,
) -> Result<(String, String), Error> {
    let ca_cert = X509::from_pem(int_cert_pem.as_bytes())
        .map_err(|e| Error::other(format!("failed to parse intermediate cert: {e}")))?;
    let ca_key = PKey::private_key_from_pem(int_key_pem.as_bytes())
        .map_err(|e| Error::other(format!("failed to parse intermediate key: {e}")))?;

    let leaf_key = gen_ec_key()?;

    let mut builder = X509Builder::new()
        .map_err(|e| Error::other(format!("failed to create X509 builder: {e}")))?;
    builder.set_version(X509_VERSION_3)
        .map_err(|e| Error::other(format!("failed to set version: {e}")))?;

    let now = unix_now();
    let not_before = Asn1Time::from_unix(now - 86400)
        .map_err(|e| Error::other(format!("failed to create not_before: {e}")))?;
    let not_after = Asn1Time::days_from_now(397)
        .map_err(|e| Error::other(format!("failed to create not_after: {e}")))?;
    builder.set_not_before(&not_before)
        .map_err(|e| Error::other(format!("failed to set not_before: {e}")))?;
    builder.set_not_after(&not_after)
        .map_err(|e| Error::other(format!("failed to set not_after: {e}")))?;

    builder.set_serial_number(&*rand_serial()?)
        .map_err(|e| Error::other(format!("failed to set serial: {e}")))?;

    let mut name_builder = X509NameBuilder::new()
        .map_err(|e| Error::other(format!("failed to create name builder: {e}")))?;
    name_builder.append_entry_by_text("CN", ROUTER_HOSTNAME)
        .map_err(|e| Error::other(format!("failed to set CN: {e}")))?;
    name_builder.append_entry_by_text("O", "Start9")
        .map_err(|e| Error::other(format!("failed to set O: {e}")))?;
    name_builder.append_entry_by_text("OU", "StartWRT")
        .map_err(|e| Error::other(format!("failed to set OU: {e}")))?;
    let subject_name = name_builder.build();
    builder.set_subject_name(&subject_name)
        .map_err(|e| Error::other(format!("failed to set subject: {e}")))?;

    // Issuer is the CA's subject
    builder.set_issuer_name(ca_cert.subject_name())
        .map_err(|e| Error::other(format!("failed to set issuer: {e}")))?;

    builder.set_pubkey(&leaf_key)
        .map_err(|e| Error::other(format!("failed to set pubkey: {e}")))?;

    // Extensions — pass CA cert for AKI derivation
    let cfg = openssl::conf::Conf::new(openssl::conf::ConfMethod::default())
        .map_err(|e| Error::other(format!("failed to create conf: {e}")))?;
    let ctx = builder.x509v3_context(Some(&ca_cert), Some(&cfg));

    let ski = SubjectKeyIdentifier::new().build(&ctx)
        .map_err(|e| Error::other(format!("failed to build SKI: {e}")))?;
    let aki = AuthorityKeyIdentifier::new().keyid(true).issuer(false).build(&ctx)
        .map_err(|e| Error::other(format!("failed to build AKI: {e}")))?;
    let san = SubjectAlternativeName::new()
        .dns(ROUTER_HOSTNAME)
        .ip(&lan_ip.to_string())
        .build(&ctx)
        .map_err(|e| Error::other(format!("failed to build SAN: {e}")))?;
    let bc = BasicConstraints::new().build()
        .map_err(|e| Error::other(format!("failed to build basic constraints: {e}")))?;
    let ku = KeyUsage::new()
        .critical()
        .digital_signature()
        .key_encipherment()
        .build()
        .map_err(|e| Error::other(format!("failed to build key usage: {e}")))?;
    builder.append_extension(ski)
        .map_err(|e| Error::other(format!("failed to append SKI: {e}")))?;
    builder.append_extension(aki)
        .map_err(|e| Error::other(format!("failed to append AKI: {e}")))?;
    builder.append_extension(san)
        .map_err(|e| Error::other(format!("failed to append SAN: {e}")))?;
    builder.append_extension(bc)
        .map_err(|e| Error::other(format!("failed to append basic constraints: {e}")))?;
    builder.append_extension(ku)
        .map_err(|e| Error::other(format!("failed to append key usage: {e}")))?;

    // Sign with the CA's key
    builder.sign(&ca_key, MessageDigest::sha256())
        .map_err(|e| Error::other(format!("failed to sign server cert: {e}")))?;

    let cert = builder.build();
    let leaf_pem = String::from_utf8(cert.to_pem()
        .map_err(|e| Error::other(format!("failed to encode leaf cert PEM: {e}")))?)
        .map_err(|e| Error::other(format!("leaf cert PEM is not UTF-8: {e}")))?;
    let key_pem = String::from_utf8(leaf_key.private_key_to_pem_pkcs8()
        .map_err(|e| Error::other(format!("failed to encode leaf key PEM: {e}")))?)
        .map_err(|e| Error::other(format!("leaf key PEM is not UTF-8: {e}")))?;

    // Build full chain: leaf cert + intermediate cert + root CA cert
    let chain_pem = format!("{}{}{}", leaf_pem, int_cert_pem, ca_cert_pem);

    Ok((chain_pem, key_pem))
}

/// Write a PEM file atomically (temp + rename), with the given permissions.
fn write_pem(path: &Path, content: &str, mode: u32) -> Result<(), Error> {
    use std::io::Write;

    let dir = path
        .parent()
        .ok_or_else(|| Error::other("no parent dir for cert path"))?;
    let tmp = tempfile::NamedTempFile::new_in(dir)
        .map_err(|e| Error::other(format!("failed to create temp file: {e}")))?;

    let file = tmp.as_file();

    // Set permissions before writing content
    let mut perms = file
        .metadata()
        .map_err(|e| Error::other(format!("failed to get temp file metadata: {e}")))?
        .permissions();
    std::os::unix::fs::PermissionsExt::set_mode(&mut perms, mode);
    file.set_permissions(perms)
        .map_err(|e| Error::other(format!("failed to set temp file permissions: {e}")))?;

    let mut writer = std::io::BufWriter::new(file);
    writer
        .write_all(content.as_bytes())
        .map_err(|e| Error::other(format!("failed to write cert: {e}")))?;
    writer
        .flush()
        .map_err(|e| Error::other(format!("failed to flush cert: {e}")))?;
    drop(writer);

    // Sync to disk before rename to survive power loss
    file.sync_all()
        .map_err(|e| Error::other(format!("failed to sync cert: {e}")))?;

    tmp.persist(path)
        .map_err(|e| Error::other(format!("failed to persist cert to {}: {e}", path.display())))?;

    Ok(())
}

/// Read the current LAN gateway IP from UCI config using the uciedit library.
pub fn read_lan_ip(uci_root: &Path) -> Ipv4Addr {
    let arena = Arena::new();
    let Ok(cfgs) = parse_all(uci_root, &arena, &["network"]) else {
        return Ipv4Addr::new(192, 168, 0, 1);
    };

    for section in &cfgs["network"].sections {
        if section.name().as_deref() == Some("lan") {
            if let Ok(Some(iface)) = section.get_typed::<NetworkInterface>() {
                if let Some(ip) = iface.ipaddr {
                    return ip;
                }
            }
        }
    }

    Ipv4Addr::new(192, 168, 0, 1)
}

/// Check whether the server cert expires within the renewal threshold.
fn cert_needs_renewal() -> bool {
    let cert_path = server_cert_path();
    let Ok(pem_data) = fs::read(&cert_path) else {
        return true;
    };

    let Ok(cert) = X509::from_pem(&pem_data) else {
        return true;
    };

    let Ok(threshold) = Asn1Time::days_from_now(RENEWAL_THRESHOLD_DAYS) else {
        return true;
    };

    // Renew if cert expires before the threshold (default to renewing on comparison error)
    cert.not_after()
        .compare(&threshold)
        .map_or(true, |ord| ord != std::cmp::Ordering::Greater)
}

/// Ensure Root CA exists (generate if missing). Returns the CA cert PEM.
pub fn ensure_root_ca() -> Result<String, Error> {
    ensure_dirs()?;

    let cert_path = ca_cert_path();
    let key_path = ca_key_path();

    if cert_path.exists() && key_path.exists() {
        return fs::read_to_string(&cert_path)
            .map_err(|e| Error::other(format!("failed to read CA cert: {e}")));
    }

    tracing::info!("generating new Root CA");
    let (cert_pem, key_pem) = generate_root_ca()?;
    write_pem(&cert_path, &cert_pem, 0o644)?;
    write_pem(&key_path, &key_pem, 0o600)?;

    Ok(cert_pem)
}

/// Ensure intermediate CA exists (generate if missing). Returns the intermediate cert PEM.
pub fn ensure_intermediate_ca() -> Result<String, Error> {
    ensure_dirs()?;

    let cert_path = int_cert_path();
    let key_path = int_key_path();

    if cert_path.exists() && key_path.exists() {
        return fs::read_to_string(&cert_path)
            .map_err(|e| Error::other(format!("failed to read intermediate cert: {e}")));
    }

    let ca_cert_pem = fs::read_to_string(ca_cert_path())
        .map_err(|e| Error::other(format!("failed to read CA cert: {e}")))?;
    let ca_key_pem = fs::read_to_string(ca_key_path())
        .map_err(|e| Error::other(format!("failed to read CA key: {e}")))?;

    tracing::info!("generating new intermediate CA");
    let (cert_pem, key_pem) = generate_intermediate_ca(&ca_cert_pem, &ca_key_pem)?;
    write_pem(&cert_path, &cert_pem, 0o644)?;
    write_pem(&key_path, &key_pem, 0o600)?;

    Ok(cert_pem)
}

/// Ensure server leaf cert exists and is valid (generate if missing, corrupt, or expiring).
pub fn ensure_server_cert(lan_ip: Ipv4Addr) -> Result<(), Error> {
    ensure_dirs()?;

    let cert_path = server_cert_path();
    let key_path = server_key_path();

    let needs_gen = !cert_path.exists() || !key_path.exists() || cert_needs_renewal();
    if !needs_gen {
        return Ok(());
    }

    generate_and_write_server_cert(lan_ip)
}

/// Force-regenerate the server leaf cert (e.g. after LAN IP change).
fn generate_and_write_server_cert(lan_ip: Ipv4Addr) -> Result<(), Error> {
    let int_cert_pem = fs::read_to_string(int_cert_path())
        .map_err(|e| Error::other(format!("failed to read intermediate cert: {e}")))?;
    let int_key_pem = fs::read_to_string(int_key_path())
        .map_err(|e| Error::other(format!("failed to read intermediate key: {e}")))?;
    let ca_cert_pem = fs::read_to_string(ca_cert_path())
        .map_err(|e| Error::other(format!("failed to read CA cert: {e}")))?;

    tracing::info!("generating server certificate for {} and {}", lan_ip, ROUTER_HOSTNAME);
    let (cert_pem, key_pem) = generate_leaf_cert(&int_cert_pem, &int_key_pem, &ca_cert_pem, lan_ip)?;

    write_pem(&server_cert_path(), &cert_pem, 0o644)?;
    write_pem(&server_key_path(), &key_pem, 0o600)?;

    Ok(())
}

/// Regenerate the server leaf cert with a new LAN IP.
pub fn regenerate_server_cert(lan_ip: Ipv4Addr) -> Result<(), Error> {
    generate_and_write_server_cert(lan_ip)
}

/// Validate that the server cert and key on disk form a valid TLS config.
/// Used at startup to detect corrupt files.
pub fn build_tls_config() -> Result<(), Error> {
    use rustls::ServerConfig;
    use rustls_pemfile::{certs, pkcs8_private_keys};
    use std::io::BufReader;

    let cert_file = fs::File::open(server_cert_path())
        .map_err(|e| Error::other(format!("failed to open server cert: {e}")))?;
    let key_file = fs::File::open(server_key_path())
        .map_err(|e| Error::other(format!("failed to open server key: {e}")))?;

    let cert_chain: Vec<_> = certs(&mut BufReader::new(cert_file))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| Error::other(format!("failed to parse server cert: {e}")))?;

    let key = pkcs8_private_keys(&mut BufReader::new(key_file))
        .next()
        .ok_or_else(|| Error::other("no private key found in server key file"))?
        .map_err(|e| Error::other(format!("failed to parse server key: {e}")))?;

    ServerConfig::builder()
        .with_no_client_auth()
        .with_single_cert(cert_chain, key.into())
        .map_err(|e| Error::other(format!("failed to build TLS config: {e}")))?;

    Ok(())
}

/// Read the Root CA certificate PEM from disk.
pub fn read_root_ca_pem() -> Result<String, Error> {
    fs::read_to_string(ca_cert_path())
        .map_err(|e| Error::other(format!("failed to read CA cert: {e}")))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_root_ca() {
        let (cert_pem, key_pem) = generate_root_ca().unwrap();
        assert!(cert_pem.contains("BEGIN CERTIFICATE"));
        assert!(key_pem.contains("BEGIN PRIVATE KEY"));

        // Verify it parses back and has expected extensions
        let cert = X509::from_pem(cert_pem.as_bytes()).unwrap();
        assert!(cert.subject_name().entries_by_nid(Nid::COMMONNAME).next().is_some());
    }

    #[test]
    fn test_generate_intermediate_ca() {
        let (ca_cert, ca_key) = generate_root_ca().unwrap();
        let (int_cert_pem, int_key_pem) = generate_intermediate_ca(&ca_cert, &ca_key).unwrap();
        assert!(int_cert_pem.contains("BEGIN CERTIFICATE"));
        assert!(int_key_pem.contains("BEGIN PRIVATE KEY"));

        let int_cert = X509::from_pem(int_cert_pem.as_bytes()).unwrap();

        // Verify CN
        let cn = int_cert.subject_name().entries_by_nid(Nid::COMMONNAME).next().unwrap();
        assert_eq!(cn.data().as_utf8().unwrap().to_string(), "StartWRT Local Intermediate CA");

        // Verify issuer matches root CA subject
        let ca = X509::from_pem(ca_cert.as_bytes()).unwrap();
        assert_eq!(
            int_cert.issuer_name().entries().next().unwrap().data().as_utf8().unwrap().to_string(),
            ca.subject_name().entries().next().unwrap().data().as_utf8().unwrap().to_string(),
        );

        // Verify it has AKI and SKI
        assert!(int_cert.subject_key_id().is_some(), "missing SKI");
        assert!(int_cert.authority_key_id().is_some(), "missing AKI");
    }

    #[test]
    fn test_generate_leaf_cert_includes_ca_in_chain() {
        let (ca_cert, ca_key) = generate_root_ca().unwrap();
        let (int_cert, int_key) = generate_intermediate_ca(&ca_cert, &ca_key).unwrap();
        let ip = Ipv4Addr::new(192, 168, 0, 1);
        let (cert_pem, key_pem) = generate_leaf_cert(&int_cert, &int_key, &ca_cert, ip).unwrap();
        assert!(key_pem.contains("BEGIN PRIVATE KEY"));
        // Chain should contain three certificates: leaf + intermediate + root CA
        let cert_count = cert_pem.matches("BEGIN CERTIFICATE").count();
        assert_eq!(cert_count, 3, "chain should contain leaf + intermediate + root CA");
    }

    #[test]
    fn test_leaf_cert_has_expected_extensions() {
        let (ca_cert, ca_key) = generate_root_ca().unwrap();
        let (int_cert, int_key) = generate_intermediate_ca(&ca_cert, &ca_key).unwrap();
        let ip = Ipv4Addr::new(192, 168, 0, 1);
        let (cert_pem, _) = generate_leaf_cert(&int_cert, &int_key, &ca_cert, ip).unwrap();

        // Parse just the leaf cert (first in chain)
        let leaf = X509::from_pem(cert_pem.as_bytes()).unwrap();

        // Verify key extensions are present
        assert!(leaf.subject_alt_names().is_some(), "missing SAN");
        assert!(leaf.subject_key_id().is_some(), "missing SKI");
        assert!(leaf.authority_key_id().is_some(), "missing AKI");
    }

    #[test]
    fn test_read_lan_ip_from_uci() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join("network"),
            "\
config interface 'lan'
\toption device 'br-lan'
\toption proto 'static'
\toption ipaddr '10.0.5.1'
\toption netmask '255.255.255.0'
",
        )
        .unwrap();

        let ip = read_lan_ip(dir.path());
        assert_eq!(ip, Ipv4Addr::new(10, 0, 5, 1));
    }

    #[test]
    fn test_read_lan_ip_default() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("network"), "").unwrap();

        let ip = read_lan_ip(dir.path());
        assert_eq!(ip, Ipv4Addr::new(192, 168, 0, 1));
    }

    #[test]
    fn test_write_pem_atomic() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.pem");
        write_pem(&path, "test content", 0o644).unwrap();
        assert_eq!(fs::read_to_string(&path).unwrap(), "test content");

        // Verify permissions
        use std::os::unix::fs::PermissionsExt;
        let mode = fs::metadata(&path).unwrap().permissions().mode() & 0o777;
        assert_eq!(mode, 0o644);
    }

    #[test]
    fn test_write_pem_key_restricted() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.key");
        write_pem(&path, "secret key", 0o600).unwrap();

        use std::os::unix::fs::PermissionsExt;
        let mode = fs::metadata(&path).unwrap().permissions().mode() & 0o777;
        assert_eq!(mode, 0o600);
    }
}
