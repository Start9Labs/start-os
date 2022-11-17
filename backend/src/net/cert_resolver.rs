use std::collections::BTreeMap;
use std::io;
use std::pin::Pin;
use std::str::FromStr;
use std::sync::{Arc, RwLock};
use std::task::{Context, Poll};

use color_eyre::eyre::eyre;
use futures::{ready, Future};
use hyper::server::accept::Accept;
use hyper::server::conn::{AddrIncoming, AddrStream};
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use tokio::io::{AsyncRead, AsyncWrite, ReadBuf};
use tokio_rustls::rustls::server::ResolvesServerCert;
use tokio_rustls::rustls::sign::{any_supported_type, CertifiedKey};
use tokio_rustls::rustls::{Certificate, PrivateKey, ServerConfig};

use crate::net::net_utils::ResourceFqdn;
use crate::Error;

enum State {
    Handshaking(tokio_rustls::Accept<AddrStream>),
    Streaming(tokio_rustls::server::TlsStream<AddrStream>),
}

// tokio_rustls::server::TlsStream doesn't expose constructor methods,
// so we have to TlsAcceptor::accept and handshake to have access to it
// TlsStream implements AsyncRead/AsyncWrite handshaking tokio_rustls::Accept first
pub struct TlsStream {
    state: State,
}

impl TlsStream {
    fn new(stream: AddrStream, config: Arc<ServerConfig>) -> TlsStream {
        let accept = tokio_rustls::TlsAcceptor::from(config).accept(stream);
        TlsStream {
            state: State::Handshaking(accept),
        }
    }
}

impl AsyncRead for TlsStream {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context,
        buf: &mut ReadBuf,
    ) -> Poll<io::Result<()>> {
        let pin = self.get_mut();
        match pin.state {
            State::Handshaking(ref mut accept) => match ready!(Pin::new(accept).poll(cx)) {
                Ok(mut stream) => {
                    let result = Pin::new(&mut stream).poll_read(cx, buf);
                    pin.state = State::Streaming(stream);
                    result
                }
                Err(err) => Poll::Ready(Err(err)),
            },
            State::Streaming(ref mut stream) => Pin::new(stream).poll_read(cx, buf),
        }
    }
}

impl AsyncWrite for TlsStream {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<io::Result<usize>> {
        let pin = self.get_mut();
        match pin.state {
            State::Handshaking(ref mut accept) => match ready!(Pin::new(accept).poll(cx)) {
                Ok(mut stream) => {
                    let result = Pin::new(&mut stream).poll_write(cx, buf);
                    pin.state = State::Streaming(stream);
                    result
                }
                Err(err) => Poll::Ready(Err(err)),
            },
            State::Streaming(ref mut stream) => Pin::new(stream).poll_write(cx, buf),
        }
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        match self.state {
            State::Handshaking(_) => Poll::Ready(Ok(())),
            State::Streaming(ref mut stream) => Pin::new(stream).poll_flush(cx),
        }
    }

    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        match self.state {
            State::Handshaking(_) => Poll::Ready(Ok(())),
            State::Streaming(ref mut stream) => Pin::new(stream).poll_shutdown(cx),
        }
    }
}

impl ResolvesServerCert for EmbassyCertResolver {
    fn resolve(
        &self,
        client_hello: tokio_rustls::rustls::server::ClientHello,
    ) -> Option<Arc<tokio_rustls::rustls::sign::CertifiedKey>> {
        let hostname_raw = client_hello.server_name();

        match hostname_raw {
            Some(hostname_str) => {
                let full_fqdn = match ResourceFqdn::from_str(hostname_str) {
                    Ok(fqdn) => fqdn,
                    Err(_) => {
                        tracing::error!("Error converting {} to fqdn struct", hostname_str);
                        return None;
                    }
                };
                let lock = self.cert_mapping.read();

                match lock {
                    Ok(lock) => lock
                        .get(&full_fqdn)
                        .map(|cert_key| Arc::new(cert_key.to_owned())),
                    Err(err) => {
                        tracing::error!("resolve fn Error: {}", err);
                        None
                    }
                }
            }
            None => None,
        }
    }
}

#[derive(Clone, Default)]
pub struct EmbassyCertResolver {
    cert_mapping: Arc<RwLock<BTreeMap<ResourceFqdn, CertifiedKey>>>,
}

impl EmbassyCertResolver {
    pub fn new() -> Self {
        Self::default()
    }
    pub async fn add_certificate_to_resolver(
        &mut self,
        service_resource_fqdn: ResourceFqdn,
        package_cert_data: (PKey<Private>, Vec<X509>),
    ) -> Result<(), Error> {
        let x509_cert_chain = package_cert_data.1;
        let private_keys = package_cert_data
            .0
            .private_key_to_der()
            .map_err(|err| Error::new(eyre!("err {}", err), crate::ErrorKind::BytesError))?;

        let mut full_rustls_certs = Vec::new();
        for cert in x509_cert_chain.iter() {
            let cert =
                Certificate(cert.to_der().map_err(|err| {
                    Error::new(eyre!("err: {}", err), crate::ErrorKind::BytesError)
                })?);

            full_rustls_certs.push(cert);
        }

        let pre_sign_key = PrivateKey(private_keys);
        let actual_sign_key = any_supported_type(&pre_sign_key)
            .map_err(|err| Error::new(eyre!("{}", err), crate::ErrorKind::SignError))?;

        let cert_key = CertifiedKey::new(full_rustls_certs, actual_sign_key);

        let mut lock = self
            .cert_mapping
            .write()
            .map_err(|err| Error::new(eyre!("{}", err), crate::ErrorKind::Network))?;
        lock.insert(service_resource_fqdn, cert_key);

        Ok(())
    }

    pub async fn remove_cert(&mut self, hostname: ResourceFqdn) -> Result<(), Error> {
        let mut lock = self
            .cert_mapping
            .write()
            .map_err(|err| Error::new(eyre!("{}", err), crate::ErrorKind::Network))?;

        lock.remove(&hostname);

        Ok(())
    }
}

pub struct TlsAcceptor {
    config: Arc<ServerConfig>,
    incoming: AddrIncoming,
}

impl TlsAcceptor {
    pub fn new(config: Arc<ServerConfig>, incoming: AddrIncoming) -> TlsAcceptor {
        TlsAcceptor { config, incoming }
    }
}

impl Accept for TlsAcceptor {
    type Conn = TlsStream;
    type Error = io::Error;

    fn poll_accept(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<Self::Conn, Self::Error>>> {
        let pin = self.get_mut();
        match ready!(Pin::new(&mut pin.incoming).poll_accept(cx)) {
            Some(Ok(sock)) => Poll::Ready(Some(Ok(TlsStream::new(sock, pin.config.clone())))),
            Some(Err(e)) => Poll::Ready(Some(Err(e))),
            None => Poll::Ready(None),
        }
    }
}
