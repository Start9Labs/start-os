use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use color_eyre::eyre::{Result as CrlResult, ensure};
use reqwest::{Client, Proxy, Url};
use serde::{Deserialize, Serialize};
use tokio::io::AsyncReadExt;
use tokio::sync::mpsc;
use tokio_rustls::rustls::client::WebPkiServerVerifier;
use tokio_rustls::rustls::client::danger::{
    HandshakeSignatureValid, ServerCertVerified, ServerCertVerifier,
};
use tokio_rustls::rustls::pki_types::{
    CertificateDer, CertificateRevocationListDer, ServerName, UnixTime,
};
use tokio_rustls::rustls::{ClientConfig, DigitallySignedStruct, RootCertStore, SignatureScheme};
use x509_parser::extensions::{DistributionPointName, GeneralName, ParsedExtension};
use x509_parser::prelude::{CertificateRevocationList, FromDer, X509Certificate};

use crate::prelude::*;
use crate::util::io::{maybe_open_file, write_file_atomic};
use crate::util::serde::Base64;
use crate::util::sync::SyncRwLock;

const MAX_SOURCES: usize = 128;
const MAX_CRL_BYTES: usize = 8 * 1024 * 1024;
const MAX_CACHE_BYTES: usize = 32 * 1024 * 1024;
const REFRESH_INTERVAL: Duration = Duration::from_secs(3600);

#[cfg(test)]
mod tests;

type SourceKey = (Url, [u8; 32]);

#[derive(Clone, Debug, Deserialize, Serialize)]
struct Source {
    url: Url,
    issuer: Base64<Vec<u8>>,
}

impl Source {
    fn key(&self) -> CrlResult<SourceKey> {
        Ok((self.url.clone(), issuer_key(&self.issuer)?))
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
struct CachedCrl {
    source: Source,
    der: Base64<Vec<u8>>,
}

struct CrlMetadata {
    issued: i64,
    next: i64,
    number: Vec<u8>,
    scope: Vec<u8>,
}

impl CachedCrl {
    fn metadata(&self) -> CrlResult<CrlMetadata> {
        let (_, issuer) =
            X509Certificate::from_der(&self.source.issuer).map_err(|e| eyre!("{e}"))?;
        let (remaining, crl) =
            CertificateRevocationList::from_der(&self.der).map_err(|e| eyre!("{e}"))?;
        ensure!(remaining.is_empty() && self.der.len() <= MAX_CRL_BYTES);
        ensure!(crl.issuer() == issuer.subject());
        crl.verify_signature(issuer.public_key())
            .map_err(|e| eyre!("{e}"))?;
        if let Some(usage) = issuer.key_usage().map_err(|e| eyre!("{e}"))? {
            ensure!(usage.value.crl_sign());
        }
        let next = crl
            .next_update()
            .ok_or_else(|| eyre!("CRL has no nextUpdate"))?;
        ensure!(next > crl.last_update());
        let mut scope = Vec::new();
        for extension in crl.extensions() {
            if let ParsedExtension::IssuingDistributionPoint(point) = extension.parsed_extension() {
                scope = extension.value.to_vec();
                if let Some(DistributionPointName::FullName(names)) = &point.distribution_point {
                    ensure!(names.iter().any(|name| matches!(name,
                        GeneralName::URI(uri) if *uri == self.source.url.as_str()
                    )));
                }
            }
        }
        Ok(CrlMetadata {
            issued: crl.last_update().timestamp(),
            next: next.timestamp(),
            number: crl
                .crl_number()
                .ok_or_else(|| eyre!("CRL has no number"))?
                .to_bytes_be(),
            scope,
        })
    }
}

#[derive(Debug)]
struct Snapshot {
    generation: u64,
    crls: BTreeMap<SourceKey, CachedCrl>,
}

#[derive(Debug)]
pub(crate) struct CrlCache {
    snapshot: SyncRwLock<Snapshot>,
    discover: mpsc::Sender<Source>,
}

impl CrlCache {
    fn new() -> (Arc<Self>, mpsc::Receiver<Source>) {
        let (discover, receiver) = mpsc::channel(MAX_SOURCES);
        (
            Arc::new(Self {
                snapshot: SyncRwLock::new(Snapshot {
                    generation: 0,
                    crls: BTreeMap::new(),
                }),
                discover,
            }),
            receiver,
        )
    }

    #[cfg(test)]
    pub(crate) fn empty() -> Arc<Self> {
        Self::new().0
    }

    pub(crate) fn native_client_config(self: &Arc<Self>) -> Result<ClientConfig, Error> {
        let native = rustls_native_certs::load_native_certs();
        for error in native.errors {
            tracing::warn!("{error}");
        }
        self.client_config(native.certs)
            .with_kind(ErrorKind::TLSInit)
    }

    fn client_config(
        self: &Arc<Self>,
        certificates: Vec<CertificateDer<'static>>,
    ) -> CrlResult<ClientConfig> {
        let mut roots = RootCertStore::empty();
        roots.add_parsable_certificates(certificates.iter().cloned());
        let roots = Arc::new(roots);
        self.verifier(roots.clone(), &[])?;
        let verifier = Arc::new(DiscoveringVerifier {
            cache: self.clone(),
            roots,
            certificates,
        });
        let mut config = ClientConfig::builder_with_provider(provider())
            .with_safe_default_protocol_versions()?
            .dangerous()
            .with_custom_certificate_verifier(verifier)
            .with_no_client_auth();
        // Resumed sessions skip certificate verification against subsequently learned CRLs.
        config.resumption = tokio_rustls::rustls::client::Resumption::disabled();
        Ok(config)
    }

    fn verifier(
        &self,
        roots: Arc<RootCertStore>,
        issuers: &[[u8; 32]],
    ) -> CrlResult<Arc<WebPkiServerVerifier>> {
        self.snapshot.peek(|snapshot| {
            let mut crls = snapshot
                .crls
                .iter()
                .filter(|((_, key), _)| issuers.contains(key))
                .map(|(_, entry)| {
                    let metadata = entry.metadata()?;
                    Ok((
                        (metadata.issued, metadata.number.len(), metadata.number),
                        entry.der.0.clone(),
                    ))
                })
                .collect::<CrlResult<Vec<_>>>()?;
            // webpki uses the first applicable CRL, including overlapping mirrors.
            crls.sort_by(|(a, _), (b, _)| b.cmp(a));
            // Expired CRLs remain authoritative for known revocations.
            let verifier = WebPkiServerVerifier::builder_with_provider(roots, provider())
                .with_crls(
                    crls.into_iter()
                        .map(|(_, der)| CertificateRevocationListDer::from(der)),
                )
                .allow_unknown_revocation_status()
                .build()?;
            Ok(verifier)
        })
    }

    fn insert(&self, entry: CachedCrl) -> CrlResult<bool> {
        let metadata = entry.metadata()?;
        let key = entry.source.key()?;
        self.snapshot.mutate(|snapshot| {
            if let Some(previous) = snapshot.crls.get(&key) {
                if previous.der == entry.der {
                    return Ok(false);
                }
                let old = previous.metadata()?;
                ensure!(metadata.scope == old.scope && metadata.issued >= old.issued);
                ensure!(
                    (metadata.number.len(), &metadata.number) > (old.number.len(), &old.number)
                );
            }
            ensure!(snapshot.crls.contains_key(&key) || snapshot.crls.len() < MAX_SOURCES);
            let size: usize = snapshot
                .crls
                .iter()
                .filter(|(old_key, _)| **old_key != key)
                .map(|(_, old)| old.der.len())
                .sum();
            ensure!(size + entry.der.len() <= MAX_CACHE_BYTES);
            // Parsing here prevents unsupported CRLs from poisoning every later handshake.
            let mut roots = RootCertStore::empty();
            roots.add(CertificateDer::from(entry.source.issuer.0.clone()))?;
            WebPkiServerVerifier::builder_with_provider(Arc::new(roots), provider())
                .with_crls([CertificateRevocationListDer::from(entry.der.0.clone())])
                .build()?;
            snapshot.crls.insert(key, entry);
            snapshot.generation += 1;
            Ok(true)
        })
    }
}

fn issuer_key(der: &[u8]) -> CrlResult<[u8; 32]> {
    let (_, issuer) = X509Certificate::from_der(der).map_err(|e| eyre!("{e}"))?;
    let mut hash = blake3::Hasher::new();
    hash.update(issuer.subject().as_raw());
    hash.update(issuer.public_key().raw);
    Ok(*hash.finalize().as_bytes())
}

fn provider() -> Arc<tokio_rustls::rustls::crypto::CryptoProvider> {
    Arc::new(tokio_rustls::rustls::crypto::ring::default_provider())
}

#[derive(Debug)]
struct DiscoveringVerifier {
    cache: Arc<CrlCache>,
    roots: Arc<RootCertStore>,
    certificates: Vec<CertificateDer<'static>>,
}

impl DiscoveringVerifier {
    fn certificate_issuers<'a>(
        &'a self,
        leaf: &'a CertificateDer<'_>,
        intermediates: &'a [CertificateDer<'_>],
    ) -> Vec<(&'a [u8], &'a [u8])> {
        let mut chain = Vec::new();
        let mut current = leaf.as_ref();
        for _ in 0..=intermediates.len() {
            let Ok((_, certificate)) = X509Certificate::from_der(current) else {
                break;
            };
            let issuer = intermediates
                .iter()
                .chain(self.certificates.iter())
                .find(|candidate| {
                    if candidate.as_ref() == current {
                        return false;
                    }
                    let Ok((_, issuer)) = X509Certificate::from_der(candidate) else {
                        return false;
                    };
                    certificate.issuer() == issuer.subject()
                        && certificate
                            .verify_signature(Some(issuer.public_key()))
                            .is_ok()
                });
            let Some(issuer) = issuer else { break };
            chain.push((current, issuer.as_ref()));
            if self
                .certificates
                .iter()
                .any(|root| root.as_ref() == issuer.as_ref())
            {
                break;
            }
            current = issuer.as_ref();
        }
        chain
    }

    fn discover(&self, chain: &[(&[u8], &[u8])]) {
        for (certificate, issuer) in chain {
            let Ok((_, certificate)) = X509Certificate::from_der(certificate) else {
                continue;
            };
            for extension in certificate.extensions() {
                let ParsedExtension::CRLDistributionPoints(points) = extension.parsed_extension()
                else {
                    continue;
                };
                for point in &points.points {
                    if point.crl_issuer.is_some() || point.reasons.is_some() {
                        continue;
                    }
                    let Some(DistributionPointName::FullName(names)) = &point.distribution_point
                    else {
                        continue;
                    };
                    for name in names {
                        let GeneralName::URI(uri) = name else {
                            continue;
                        };
                        let Ok(url) = Url::parse(uri) else { continue };
                        if !valid_source_url(&url) {
                            continue;
                        }
                        let _ = self.cache.discover.try_send(Source {
                            url,
                            issuer: Base64(issuer.to_vec()),
                        });
                    }
                }
            }
        }
    }
}

impl ServerCertVerifier for DiscoveringVerifier {
    fn verify_server_cert(
        &self,
        end_entity: &CertificateDer<'_>,
        intermediates: &[CertificateDer<'_>],
        server_name: &ServerName<'_>,
        ocsp_response: &[u8],
        now: UnixTime,
    ) -> Result<ServerCertVerified, tokio_rustls::rustls::Error> {
        let chain = self.certificate_issuers(end_entity, intermediates);
        // Different signing keys can share an issuer name and distribution point.
        let issuers = chain
            .iter()
            .filter_map(|(_, issuer)| issuer_key(issuer).ok())
            .collect::<Vec<_>>();
        let verifier = self
            .cache
            .verifier(self.roots.clone(), &issuers)
            .map_err(|e| tokio_rustls::rustls::Error::General(e.to_string()))?;
        let result = verifier.verify_server_cert(
            end_entity,
            intermediates,
            server_name,
            ocsp_response,
            now,
        )?;
        self.discover(&chain);
        Ok(result)
    }

    fn verify_tls12_signature(
        &self,
        message: &[u8],
        cert: &CertificateDer<'_>,
        dss: &DigitallySignedStruct,
    ) -> Result<HandshakeSignatureValid, tokio_rustls::rustls::Error> {
        tokio_rustls::rustls::crypto::verify_tls12_signature(
            message,
            cert,
            dss,
            &provider().signature_verification_algorithms,
        )
    }

    fn verify_tls13_signature(
        &self,
        message: &[u8],
        cert: &CertificateDer<'_>,
        dss: &DigitallySignedStruct,
    ) -> Result<HandshakeSignatureValid, tokio_rustls::rustls::Error> {
        tokio_rustls::rustls::crypto::verify_tls13_signature(
            message,
            cert,
            dss,
            &provider().signature_verification_algorithms,
        )
    }

    fn supported_verify_schemes(&self) -> Vec<SignatureScheme> {
        provider()
            .signature_verification_algorithms
            .supported_schemes()
    }
}

fn valid_source_url(url: &Url) -> bool {
    matches!(url.scheme(), "http" | "https")
        && url.host_str().is_some()
        && url.username().is_empty()
        && url.password().is_none()
        && url.fragment().is_none()
        && url.as_str().len() <= 2048
}

fn refresh_delay(next: i64, now: i64) -> Duration {
    if next <= now {
        REFRESH_INTERVAL
    } else {
        Duration::from_secs(((next - now) / 2).max(60) as u64).min(REFRESH_INTERVAL)
    }
}

pub(crate) struct CrlSync {
    pub cache: Arc<CrlCache>,
    receiver: mpsc::Receiver<Source>,
    proxy: Proxy,
    path: PathBuf,
}

impl CrlSync {
    pub(crate) async fn new(path: PathBuf, proxy: &str) -> Result<Self, Error> {
        let (cache, receiver) = CrlCache::new();
        let loaded: CrlResult<()> = async {
            let Some(file) = maybe_open_file(&path).await.map_err(|e| eyre!("{e}"))? else {
                return Ok(());
            };
            let mut bytes = Vec::new();
            file.take((MAX_CACHE_BYTES * 2 + 1) as u64)
                .read_to_end(&mut bytes)
                .await?;
            ensure!(bytes.len() <= MAX_CACHE_BYTES * 2);
            for entry in serde_json::from_slice::<Vec<CachedCrl>>(&bytes)? {
                ensure!(valid_source_url(&entry.source.url));
                cache.insert(entry)?;
            }
            Ok(())
        }
        .await;
        if let Err(error) = loaded {
            tracing::warn!("CRL cache: {error}");
        }
        Ok(Self {
            cache,
            receiver,
            proxy: Proxy::all(proxy)?,
            path,
        })
    }

    pub(crate) async fn run(mut self) {
        let mut sources = self.cache.snapshot.peek(|snapshot| {
            snapshot
                .crls
                .iter()
                .map(|(key, entry)| {
                    (
                        key.clone(),
                        (entry.source.clone(), tokio::time::Instant::now()),
                    )
                })
                .collect::<BTreeMap<_, _>>()
        });
        let mut saved_generation = self.cache.snapshot.peek(|snapshot| snapshot.generation);
        let mut interval = tokio::time::interval(Duration::from_secs(60));
        interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
        loop {
            tokio::select! {
                source = self.receiver.recv() => {
                    let Some(source) = source else { break };
                    if sources.len() < MAX_SOURCES {
                        if let Ok(key) = source.key() {
                            sources.entry(key).or_insert_with(|| (source, tokio::time::Instant::now()));
                        }
                    }
                }
                _ = interval.tick() => (),
            }
            let due = sources
                .iter()
                .filter(|(_, (_, next))| *next <= tokio::time::Instant::now())
                .map(|(key, (source, _))| (key.clone(), source.clone()))
                .collect::<Vec<_>>();
            if due.is_empty() {
                continue;
            }
            for (key, source) in due {
                let delay = match self.refresh(&source).await {
                    Ok(delay) => delay,
                    Err(error) => {
                        tracing::warn!(url = %source.url, "CRL refresh: {error}");
                        REFRESH_INTERVAL
                    }
                };
                sources.insert(key, (source, tokio::time::Instant::now() + delay));
            }
            let generation = self.cache.snapshot.peek(|snapshot| snapshot.generation);
            if generation != saved_generation {
                match self.persist().await {
                    Ok(()) => saved_generation = generation,
                    Err(error) => tracing::warn!("CRL cache: {error}"),
                }
            }
        }
    }

    async fn refresh(&self, source: &Source) -> CrlResult<Duration> {
        // A fresh bootstrap client observes trust-store changes without recursive CRL discovery.
        let client = Client::builder()
            .use_native_tls()
            .proxy(self.proxy.clone())
            .redirect(reqwest::redirect::Policy::none())
            .timeout(Duration::from_secs(30))
            .build()?;
        let mut response = client
            .get(source.url.clone())
            .send()
            .await?
            .error_for_status()?;
        ensure!(
            response
                .content_length()
                .is_none_or(|len| len <= MAX_CRL_BYTES as u64)
        );
        let mut bytes = Vec::new();
        while let Some(chunk) = response.chunk().await? {
            ensure!(bytes.len() + chunk.len() <= MAX_CRL_BYTES);
            bytes.extend_from_slice(&chunk);
        }
        let entry = CachedCrl {
            source: source.clone(),
            der: Base64(bytes),
        };
        let metadata = entry.metadata()?;
        let now = chrono::Utc::now().timestamp();
        ensure!(metadata.issued <= now);
        if metadata.next <= now {
            tracing::warn!(url = %source.url, "CRL publisher returned expired data; retaining revocation checks");
        }
        self.cache.insert(entry)?;
        Ok(refresh_delay(metadata.next, now))
    }

    async fn persist(&self) -> CrlResult<()> {
        let entries = self
            .cache
            .snapshot
            .peek(|snapshot| snapshot.crls.values().cloned().collect::<Vec<_>>());
        write_file_atomic(&self.path, serde_json::to_vec(&entries)?)
            .await
            .map_err(|e| eyre!("{e}"))?;
        Ok(())
    }
}
