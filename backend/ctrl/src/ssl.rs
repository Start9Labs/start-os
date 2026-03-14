use std::fs;
use std::net::Ipv4Addr;
use std::path::{Path, PathBuf};

use rcgen::{
    BasicConstraints, CertificateParams, DnType, ExtendedKeyUsagePurpose, IsCa, KeyPair,
    KeyUsagePurpose, SanType,
};
use time::OffsetDateTime;
use uciedit::openwrt::NetworkInterface;
use uciedit::{parse_all, Arena};

use crate::Error;

/// Directory for CA and server certificates.
const SSL_CERT_DIR: &str = "/etc/ssl/certs";
const SSL_KEY_DIR: &str = "/etc/ssl/private";

const CA_CERT_FILENAME: &str = "startwrt-ca.pem";
const CA_KEY_FILENAME: &str = "startwrt-ca.key";
const SERVER_CERT_FILENAME: &str = "startwrt-server.pem";
const SERVER_KEY_FILENAME: &str = "startwrt-server.key";

/// Default SAN hostname for the router.
const ROUTER_HOSTNAME: &str = "router.lan";

/// Renew the leaf cert if it expires within this many days.
const RENEWAL_THRESHOLD_DAYS: i64 = 30;

pub fn ca_cert_path() -> PathBuf {
    Path::new(SSL_CERT_DIR).join(CA_CERT_FILENAME)
}

fn ca_key_path() -> PathBuf {
    Path::new(SSL_KEY_DIR).join(CA_KEY_FILENAME)
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

/// Generate a new Root CA key pair and self-signed certificate.
/// Returns (cert_pem, key_pem).
fn generate_root_ca() -> Result<(String, String), Error> {
    let key_pair = KeyPair::generate_for(&rcgen::PKCS_ECDSA_P256_SHA256)
        .map_err(|e| Error::other(format!("failed to generate CA key: {e}")))?;

    let mut params = CertificateParams::default();
    params
        .distinguished_name
        .push(DnType::CommonName, "StartWRT Local Root CA");
    params
        .distinguished_name
        .push(DnType::OrganizationName, "Start9");
    params
        .distinguished_name
        .push(DnType::OrganizationalUnitName, "StartWRT");
    // pathlen:0 — can only sign leaf certs, not sub-CAs
    params.is_ca = IsCa::Ca(BasicConstraints::Constrained(0));
    params.key_usages = vec![
        KeyUsagePurpose::DigitalSignature,
        KeyUsagePurpose::KeyCertSign,
        KeyUsagePurpose::CrlSign,
    ];

    // Valid for ~10 years
    let now = OffsetDateTime::now_utc();
    params.not_before = now - time::Duration::days(1);
    params.not_after = now + time::Duration::days(3650);

    let cert = params
        .self_signed(&key_pair)
        .map_err(|e| Error::other(format!("failed to generate CA cert: {e}")))?;

    Ok((cert.pem(), key_pair.serialize_pem()))
}

/// Generate a leaf certificate signed by the Root CA.
/// SANs include the given LAN IP and `router.lan`.
/// Returns (leaf_cert_pem + ca_cert_pem chain, key_pem).
fn generate_leaf_cert(
    ca_cert_pem: &str,
    ca_key_pem: &str,
    lan_ip: Ipv4Addr,
) -> Result<(String, String), Error> {
    let ca_key = KeyPair::from_pem(ca_key_pem)
        .map_err(|e| Error::other(format!("failed to parse CA key: {e}")))?;
    let ca_params = CertificateParams::from_ca_cert_pem(ca_cert_pem)
        .map_err(|e| Error::other(format!("failed to parse CA cert params: {e}")))?;
    let ca_cert = ca_params
        .self_signed(&ca_key)
        .map_err(|e| Error::other(format!("failed to reconstruct CA cert: {e}")))?;

    let leaf_key = KeyPair::generate_for(&rcgen::PKCS_ECDSA_P256_SHA256)
        .map_err(|e| Error::other(format!("failed to generate server key: {e}")))?;

    let mut params = CertificateParams::default();
    params
        .distinguished_name
        .push(DnType::CommonName, ROUTER_HOSTNAME);
    params
        .distinguished_name
        .push(DnType::OrganizationName, "Start9");
    params
        .distinguished_name
        .push(DnType::OrganizationalUnitName, "StartWRT");

    params.subject_alt_names = vec![
        SanType::DnsName(ROUTER_HOSTNAME.try_into().map_err(|e| {
            Error::other(format!("invalid hostname: {e}"))
        })?),
        SanType::IpAddress(lan_ip.into()),
    ];

    params.key_usages = vec![
        KeyUsagePurpose::DigitalSignature,
        KeyUsagePurpose::KeyEncipherment,
    ];
    params.extended_key_usages = vec![ExtendedKeyUsagePurpose::ServerAuth];
    params.is_ca = IsCa::NoCa;

    // Valid for 397 days (CA/Browser Forum limit)
    let now = OffsetDateTime::now_utc();
    params.not_before = now - time::Duration::days(1);
    params.not_after = now + time::Duration::days(397);

    let cert = params
        .signed_by(&leaf_key, &ca_cert, &ca_key)
        .map_err(|e| Error::other(format!("failed to sign server cert: {e}")))?;

    // Build full chain: leaf cert + CA cert
    let chain_pem = format!("{}{}", cert.pem(), ca_cert_pem);

    Ok((chain_pem, leaf_key.serialize_pem()))
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

    // Parse the first certificate from the PEM chain
    let mut reader = std::io::BufReader::new(pem_data.as_slice());
    let certs: Vec<_> = rustls_pemfile::certs(&mut reader)
        .collect::<Result<Vec<_>, _>>()
        .unwrap_or_default();
    let Some(cert_der) = certs.first() else {
        return true;
    };

    // Use x509-parser to check the notAfter field
    match x509_parser::parse_x509_certificate(cert_der.as_ref()) {
        Ok((_, cert)) => {
            let not_after = cert.validity().not_after.timestamp();
            let now = OffsetDateTime::now_utc().unix_timestamp();
            let remaining_days = (not_after - now) / 86400;
            remaining_days < RENEWAL_THRESHOLD_DAYS
        }
        Err(_) => true,
    }
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
    let ca_cert_pem = fs::read_to_string(ca_cert_path())
        .map_err(|e| Error::other(format!("failed to read CA cert: {e}")))?;
    let ca_key_pem = fs::read_to_string(ca_key_path())
        .map_err(|e| Error::other(format!("failed to read CA key: {e}")))?;

    tracing::info!("generating server certificate for {} and {}", lan_ip, ROUTER_HOSTNAME);
    let (cert_pem, key_pem) = generate_leaf_cert(&ca_cert_pem, &ca_key_pem, lan_ip)?;

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
    }

    #[test]
    fn test_generate_leaf_cert_includes_ca_in_chain() {
        let (ca_cert, ca_key) = generate_root_ca().unwrap();
        let ip = Ipv4Addr::new(192, 168, 0, 1);
        let (cert_pem, key_pem) = generate_leaf_cert(&ca_cert, &ca_key, ip).unwrap();
        assert!(key_pem.contains("BEGIN PRIVATE KEY"));
        // Chain should contain two certificates: leaf + CA
        let cert_count = cert_pem.matches("BEGIN CERTIFICATE").count();
        assert_eq!(cert_count, 2, "chain should contain leaf cert + CA cert");
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
