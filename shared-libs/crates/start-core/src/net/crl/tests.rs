use rcgen::{
    BasicConstraints, Certificate, CertificateParams, CertificateRevocationListParams,
    CrlDistributionPoint, CrlIssuingDistributionPoint, DistinguishedName, DnType, IsCa,
    KeyIdMethod, KeyUsagePurpose, RevokedCertParams, SerialNumber, date_time_ymd,
};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpListener;
use tokio_rustls::TlsAcceptor;
use tokio_rustls::rustls::pki_types::PrivatePkcs8KeyDer;
use tokio_rustls::rustls::{CertificateError, Error as TlsError, ServerConfig};

use super::*;
use crate::util::Invoke;

fn authority(name: &str, urls: &[&str], serial: u64) -> Certificate {
    let mut params = CertificateParams::new(Vec::new());
    params.is_ca = IsCa::Ca(BasicConstraints::Unconstrained);
    params.key_usages = vec![KeyUsagePurpose::KeyCertSign, KeyUsagePurpose::CrlSign];
    params.serial_number = Some(serial.into());
    params.distinguished_name = DistinguishedName::new();
    params.distinguished_name.push(DnType::CommonName, name);
    params.crl_distribution_points = points(urls);
    Certificate::from_params(params).unwrap()
}

fn points(urls: &[&str]) -> Vec<CrlDistributionPoint> {
    urls.iter()
        .map(|url| CrlDistributionPoint {
            uris: vec![(*url).into()],
        })
        .collect()
}

fn leaf(urls: &[&str], serial: u64) -> Certificate {
    let mut params = CertificateParams::new(vec!["localhost".into()]);
    params.serial_number = Some(serial.into());
    params.crl_distribution_points = points(urls);
    Certificate::from_params(params).unwrap()
}

fn crl_params(url: &str, number: u64, revoked: &[u64]) -> CertificateRevocationListParams {
    CertificateRevocationListParams {
        this_update: date_time_ymd(2024, 1, 1),
        next_update: date_time_ymd(2024, 1, 2),
        crl_number: SerialNumber::from(number),
        issuing_distribution_point: Some(CrlIssuingDistributionPoint {
            distribution_point: CrlDistributionPoint {
                uris: vec![url.into()],
            },
            scope: None,
        }),
        revoked_certs: revoked
            .iter()
            .map(|serial| RevokedCertParams {
                serial_number: (*serial).into(),
                revocation_time: date_time_ymd(2024, 1, 1),
                reason_code: None,
                invalidity_date: None,
            })
            .collect(),
        alg: &rcgen::PKCS_ECDSA_P256_SHA256,
        key_identifier_method: KeyIdMethod::Sha256,
    }
}

fn signed_entry(
    issuer: &Certificate,
    url: &str,
    params: CertificateRevocationListParams,
) -> CachedCrl {
    let crl = rcgen::CertificateRevocationList::from_params(params).unwrap();
    CachedCrl {
        source: Source {
            url: Url::parse(url).unwrap(),
            issuer: Base64(issuer.serialize_der().unwrap()),
        },
        der: Base64(crl.serialize_der_with_signer(issuer).unwrap()),
    }
}

fn entry(issuer: &Certificate, url: &str, number: u64, revoked: &[u64]) -> CachedCrl {
    signed_entry(issuer, url, crl_params(url, number, revoked))
}

fn verifier(cache: &Arc<CrlCache>, root: Vec<u8>) -> Arc<DiscoveringVerifier> {
    let certificates = vec![CertificateDer::from(root)];
    let mut roots = RootCertStore::empty();
    roots.add(certificates[0].clone()).unwrap();
    let roots = Arc::new(roots);
    Arc::new(DiscoveringVerifier {
        cache: cache.clone(),
        certificates,
        roots,
    })
}

fn verify(
    verifier: &DiscoveringVerifier,
    leaf: Vec<u8>,
    intermediates: &[CertificateDer<'_>],
) -> Result<ServerCertVerified, TlsError> {
    verifier.verify_server_cert(
        &CertificateDer::from(leaf),
        intermediates,
        &ServerName::try_from("localhost").unwrap(),
        &[],
        UnixTime::now(),
    )
}

#[test]
fn refreshes_before_expiry_without_retrying_stale_data_in_a_loop() {
    assert_eq!(refresh_delay(7200, 0), Duration::from_secs(3600));
    assert_eq!(refresh_delay(1200, 0), Duration::from_secs(600));
    assert_eq!(refresh_delay(1, 0), Duration::from_secs(60));
    assert_eq!(refresh_delay(0, 1), Duration::from_secs(3600));
}

#[test]
fn stale_crls_block_revoked_certificates_without_blocking_unknown_shards() {
    let ca = authority("root", &[], 1);
    let first = "http://example.com/first.crl";
    let second = "http://example.com/second.crl";
    let (cache, _receiver) = CrlCache::new();
    let verifier = verifier(&cache, ca.serialize_der().unwrap());
    let revoked = leaf(&[first], 42).serialize_der_with_signer(&ca).unwrap();
    assert!(verify(&verifier, revoked.clone(), &[]).is_ok());
    cache.insert(entry(&ca, first, 1, &[42])).unwrap();
    assert!(matches!(
        verify(&verifier, revoked, &[]),
        Err(TlsError::InvalidCertificate(CertificateError::Revoked))
    ));
    assert!(
        verify(
            &verifier,
            leaf(&[second], 42).serialize_der_with_signer(&ca).unwrap(),
            &[]
        )
        .is_ok()
    );
    assert!(
        verify(
            &verifier,
            leaf(&[first], 43).serialize_der_with_signer(&ca).unwrap(),
            &[]
        )
        .is_ok()
    );
}

#[test]
fn checks_intermediates_and_preserves_normal_certificate_validation() {
    let url = "http://example.com/ca.crl";
    let ca = authority("root", &[], 1);
    let intermediate = authority("intermediate", &[url], 2);
    let intermediate_der = intermediate.serialize_der_with_signer(&ca).unwrap();
    let peer = leaf(&[], 42)
        .serialize_der_with_signer(&intermediate)
        .unwrap();
    let (cache, mut receiver) = CrlCache::new();
    let verifier = verifier(&cache, ca.serialize_der().unwrap());
    assert!(
        verify(
            &verifier,
            peer.clone(),
            &[CertificateDer::from(intermediate_der.clone())]
        )
        .is_ok()
    );
    assert_eq!(receiver.try_recv().unwrap().url.as_str(), url);
    cache.insert(entry(&ca, url, 1, &[2])).unwrap();
    assert!(matches!(
        verify(&verifier, peer, &[CertificateDer::from(intermediate_der)]),
        Err(TlsError::InvalidCertificate(CertificateError::Revoked))
    ));
    let unrelated = authority("untrusted", &[], 1);
    assert!(
        verify(
            &verifier,
            leaf(&[], 42).serialize_der_with_signer(&unrelated).unwrap(),
            &[]
        )
        .is_err()
    );
    let peer = leaf(&[], 43).serialize_der_with_signer(&ca).unwrap();
    assert!(
        verifier
            .verify_server_cert(
                &CertificateDer::from(peer),
                &[],
                &ServerName::try_from("wrong.example").unwrap(),
                &[],
                UnixTime::now()
            )
            .is_err()
    );
}

#[test]
fn invalid_and_rollback_updates_preserve_accepted_revocations() {
    let url = "http://example.com/revoked.crl";
    let ca = authority("root", &[], 1);
    let (cache, _receiver) = CrlCache::new();
    let accepted = entry(&ca, url, 2, &[42]);
    assert!(cache.insert(accepted.clone()).unwrap());
    assert!(!cache.insert(accepted).unwrap());
    assert!(cache.insert(entry(&ca, url, 1, &[])).is_err());
    let other = authority("root", &[], 1);
    let mut forged = entry(&other, url, 3, &[]);
    forged.source.issuer = Base64(ca.serialize_der().unwrap());
    assert!(cache.insert(forged).is_err());
    let mut wrong_shard = entry(&ca, "http://example.com/other.crl", 3, &[]);
    wrong_shard.source.url = Url::parse(url).unwrap();
    assert!(cache.insert(wrong_shard).is_err());
    let verifier = verifier(&cache, ca.serialize_der().unwrap());
    let peer = leaf(&[url], 42).serialize_der_with_signer(&ca).unwrap();
    assert!(verify(&verifier, peer.clone(), &[]).is_err());
    cache.insert(entry(&ca, url, 3, &[])).unwrap();
    assert!(verify(&verifier, peer, &[]).is_ok());
}

#[test]
fn issuer_key_rotation_does_not_mix_same_named_crls() {
    let url = "http://example.com/list.crl";
    let old = authority("same name", &[], 1);
    let new = authority("same name", &[], 2);
    let (cache, _receiver) = CrlCache::new();
    cache.insert(entry(&old, url, 1, &[42])).unwrap();
    cache.insert(entry(&new, url, 2, &[])).unwrap();
    let old_verifier = verifier(&cache, old.serialize_der().unwrap());
    let new_verifier = verifier(&cache, new.serialize_der().unwrap());
    assert!(matches!(
        verify(
            &old_verifier,
            leaf(&[url], 42).serialize_der_with_signer(&old).unwrap(),
            &[]
        ),
        Err(TlsError::InvalidCertificate(CertificateError::Revoked))
    ));
    assert!(
        verify(
            &new_verifier,
            leaf(&[url], 42).serialize_der_with_signer(&new).unwrap(),
            &[]
        )
        .is_ok()
    );
}

#[test]
fn only_authenticated_peers_discover_http_distribution_points() {
    let ca = authority("root", &[], 1);
    let (cache, mut receiver) = CrlCache::new();
    let verifier = verifier(&cache, ca.serialize_der().unwrap());
    let urls = [
        "http://example.com/list.crl",
        "https://example.com/list.crl",
        "file:///etc/passwd",
        "ldap://example.com/list",
        "http://user:pass@example.com/list",
    ];
    let peer = leaf(&urls, 42).serialize_der_with_signer(&ca).unwrap();
    assert!(
        verifier
            .verify_server_cert(
                &CertificateDer::from(peer.clone()),
                &[],
                &ServerName::try_from("wrong.example").unwrap(),
                &[],
                UnixTime::now()
            )
            .is_err()
    );
    assert!(receiver.try_recv().is_err());
    verify(&verifier, peer, &[]).unwrap();
    assert_eq!(receiver.try_recv().unwrap().url.as_str(), urls[0]);
    assert_eq!(receiver.try_recv().unwrap().url.as_str(), urls[1]);
    assert!(receiver.try_recv().is_err());
}

async fn http_server(body: Arc<SyncRwLock<Vec<u8>>>) -> (String, tokio::task::JoinHandle<()>) {
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let url = format!("http://{}/list.crl", listener.local_addr().unwrap());
    let task = tokio::spawn(async move {
        loop {
            let (mut stream, _) = listener.accept().await.unwrap();
            let mut request = [0; 4096];
            if stream.read(&mut request).await.is_err() {
                continue;
            }
            let bytes = body.read();
            let header = format!(
                "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                bytes.len()
            );
            if stream.write_all(header.as_bytes()).await.is_ok() {
                let _ = stream.write_all(&bytes).await;
            }
        }
    });
    (url, task)
}

async fn tls_server(ca: &Certificate, peer: &Certificate) -> (String, tokio::task::JoinHandle<()>) {
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let url = format!(
        "https://localhost:{}",
        listener.local_addr().unwrap().port()
    );
    let key = PrivatePkcs8KeyDer::from(peer.serialize_private_key_der());
    let config = ServerConfig::builder_with_provider(provider())
        .with_safe_default_protocol_versions()
        .unwrap()
        .with_no_client_auth()
        .with_single_cert(
            vec![CertificateDer::from(
                peer.serialize_der_with_signer(ca).unwrap(),
            )],
            key.into(),
        )
        .unwrap();
    let acceptor = TlsAcceptor::from(Arc::new(config));
    let task = tokio::spawn(async move {
        loop {
            let (stream, _) = listener.accept().await.unwrap();
            let Ok(mut stream) = acceptor.accept(stream).await else {
                continue;
            };
            let mut request = [0; 4096];
            if stream.read(&mut request).await.is_ok() {
                let _ = stream
                    .write_all(b"HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n")
                    .await;
            }
        }
    });
    (url, task)
}

async fn socks_proxy() -> (String, crate::net::socks::SocksController) {
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let address = listener.local_addr().unwrap();
    drop(listener);
    let controller = crate::net::socks::SocksController::new(address).unwrap();
    tokio::time::timeout(Duration::from_secs(5), async {
        while tokio::net::TcpStream::connect(address).await.is_err() {
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    })
    .await
    .unwrap();
    (format!("socks5h://{address}"), controller)
}

#[tokio::test]
async fn reqwest_discovers_syncs_and_rejects_revocation_on_existing_client() {
    let directory = tempfile::tempdir().unwrap();
    let body = Arc::new(SyncRwLock::new(Vec::new()));
    let (crl_url, http_task) = http_server(body.clone()).await;
    let ca = authority("root", &[], 1);
    let peer = leaf(&[&crl_url], 42);
    body.replace(entry(&ca, &crl_url, 1, &[42]).der.0);
    let (url, tls_task) = tls_server(&ca, &peer).await;
    let (proxy, _controller) = socks_proxy().await;
    let sync = CrlSync::new(directory.path().join("crls.json"), &proxy)
        .await
        .unwrap();
    let cache = sync.cache.clone();
    let config = cache
        .client_config(vec![CertificateDer::from(ca.serialize_der().unwrap())])
        .unwrap();
    let client = Client::builder()
        .use_preconfigured_tls(config)
        .proxy(Proxy::all(&proxy).unwrap())
        .build()
        .unwrap();
    client.get(&url).send().await.unwrap();
    let worker = tokio::spawn(sync.run());
    tokio::time::timeout(Duration::from_secs(5), async {
        while cache.snapshot.peek(|snapshot| snapshot.generation) == 0 {
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    })
    .await
    .unwrap();
    let error = client.get(&url).send().await.unwrap_err();
    assert!(format!("{error:?}").contains("Revoked"), "{error:?}");
    worker.abort();
    http_task.abort();
    tls_task.abort();
}

#[tokio::test]
async fn failed_fetch_and_restart_retain_stale_signed_crls() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("crls.json");
    let body = Arc::new(SyncRwLock::new(Vec::new()));
    let (url, server) = http_server(body.clone()).await;
    let ca = authority("root", &[], 1);
    let accepted = entry(&ca, &url, 1, &[42]);
    body.replace(accepted.der.0.clone());
    let (proxy, _controller) = socks_proxy().await;
    let sync = CrlSync::new(path.clone(), &proxy).await.unwrap();
    sync.refresh(&accepted.source).await.unwrap();
    body.replace(b"not a CRL".to_vec());
    assert!(sync.refresh(&accepted.source).await.is_err());
    let mut future = crl_params(&url, 2, &[]);
    future.this_update = date_time_ymd(2090, 1, 1);
    future.next_update = date_time_ymd(2090, 1, 2);
    body.replace(signed_entry(&ca, &url, future).der.0);
    assert!(sync.refresh(&accepted.source).await.is_err());
    body.replace(vec![0; MAX_CRL_BYTES + 1]);
    assert!(sync.refresh(&accepted.source).await.is_err());
    sync.persist().await.unwrap();
    let restarted = CrlSync::new(path, "socks5h://127.0.0.1:1").await.unwrap();
    let verifier = verifier(&restarted.cache, ca.serialize_der().unwrap());
    assert!(matches!(
        verify(
            &verifier,
            leaf(&[&url], 42).serialize_der_with_signer(&ca).unwrap(),
            &[]
        ),
        Err(TlsError::InvalidCertificate(CertificateError::Revoked))
    ));
    server.abort();
}

#[tokio::test]
async fn native_roots_include_imported_ca() {
    const PROBE_URL: &str = "STARTOS_CRL_ROOT_PROBE_URL";
    if let Ok(url) = std::env::var(PROBE_URL) {
        let client = Client::builder()
            .use_preconfigured_tls(CrlCache::empty().native_client_config().unwrap())
            .no_proxy()
            .build()
            .unwrap();
        client.get(url).send().await.unwrap();
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    let root_file = directory.path().join("root.pem");
    let ca = authority("imported root", &[], 1);
    tokio::fs::write(&root_file, ca.serialize_pem().unwrap())
        .await
        .unwrap();
    let (url, server) = tls_server(&ca, &leaf(&[], 42)).await;
    tokio::process::Command::new(std::env::current_exe().unwrap())
        .args([
            "--exact",
            "net::crl::tests::native_roots_include_imported_ca",
            "--nocapture",
        ])
        .env("SSL_CERT_FILE", &root_file)
        .env_remove("SSL_CERT_DIR")
        .env(PROBE_URL, url)
        .invoke(ErrorKind::Unknown)
        .await
        .unwrap();
    server.abort();
}
