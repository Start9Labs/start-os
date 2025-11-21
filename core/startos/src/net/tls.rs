use std::sync::Arc;
use std::task::{Poll, ready};

use futures::future::BoxFuture;
use futures::stream::FuturesUnordered;
use futures::{FutureExt, StreamExt};
use imbl_value::InternedString;
use openssl::x509::X509Ref;
use tokio::io::AsyncWriteExt;
use tokio_rustls::LazyConfigAcceptor;
use tokio_rustls::rustls::crypto::CryptoProvider;
use tokio_rustls::rustls::pki_types::CertificateDer;
use tokio_rustls::rustls::server::{Acceptor, ClientHello, ResolvesServerCert};
use tokio_rustls::rustls::sign::CertifiedKey;
use tokio_rustls::rustls::{ClientConfig, RootCertStore, ServerConfig};
use visit_rs::{Visit, VisitFields};

use crate::net::web_server::{Accept, AcceptStream, MetadataVisitor};
use crate::prelude::*;
use crate::util::io::{BackTrackingIO, ReadWriter};
use crate::util::serde::MaybeUtf8String;
use crate::util::sync::SyncMutex;

#[derive(Debug, Clone, VisitFields)]
pub struct TlsMetadata<M> {
    pub inner: M,
    pub tls_info: TlsHandshakeInfo,
}
impl<V: MetadataVisitor<Result = ()>, M: Visit<V>> Visit<V> for TlsMetadata<M> {
    fn visit(&self, visitor: &mut V) -> <V as visit_rs::Visitor>::Result {
        self.visit_fields(visitor).collect()
    }
}

#[derive(Debug, Clone)]
pub struct TlsHandshakeInfo {
    pub sni: Option<InternedString>,
    pub alpn: Vec<MaybeUtf8String>,
}
impl<V: MetadataVisitor> Visit<V> for TlsHandshakeInfo {
    fn visit(&self, visitor: &mut V) -> <V as visit_rs::Visitor>::Result {
        visitor.visit(self)
    }
}

pub trait TlsHandler<'a, A: Accept> {
    fn get_config(
        &'a mut self,
        hello: &'a ClientHello<'a>,
        metadata: &'a A::Metadata,
    ) -> impl Future<Output = Option<ServerConfig>> + Send + 'a;
}

#[derive(Clone)]
pub struct ChainedHandler<H0, H1>(pub H0, pub H1);
impl<'a, A, H0, H1> TlsHandler<'a, A> for ChainedHandler<H0, H1>
where
    A: Accept + 'a,
    <A as Accept>::Metadata: Send + Sync,
    H0: TlsHandler<'a, A> + Send,
    H1: TlsHandler<'a, A> + Send,
{
    async fn get_config(
        &'a mut self,
        hello: &'a ClientHello<'a>,
        metadata: &'a <A as Accept>::Metadata,
    ) -> Option<ServerConfig> {
        if let Some(config) = self.0.get_config(hello, metadata).await {
            return Some(config);
        }
        self.1.get_config(hello, metadata).await
    }
}

#[derive(Clone)]
pub struct TlsHandlerWrapper<I, W> {
    pub inner: I,
    pub wrapper: W,
}

pub trait WrapTlsHandler<A: Accept> {
    fn wrap<'a>(
        &'a mut self,
        prev: ServerConfig,
        hello: &'a ClientHello<'a>,
        metadata: &'a <A as Accept>::Metadata,
    ) -> impl Future<Output = Option<ServerConfig>> + Send + 'a
    where
        Self: 'a;
}

impl<'a, A, I, W> TlsHandler<'a, A> for TlsHandlerWrapper<I, W>
where
    A: Accept + 'a,
    <A as Accept>::Metadata: Send + Sync,
    I: TlsHandler<'a, A> + Send,
    W: WrapTlsHandler<A> + Send,
{
    async fn get_config(
        &'a mut self,
        hello: &'a ClientHello<'a>,
        metadata: &'a <A as Accept>::Metadata,
    ) -> Option<ServerConfig> {
        let prev = self.inner.get_config(hello, metadata).await?;
        self.wrapper.wrap(prev, hello, metadata).await
    }
}

#[derive(Debug)]
pub struct SingleCertResolver(pub Arc<CertifiedKey>);
impl ResolvesServerCert for SingleCertResolver {
    fn resolve(&self, _: ClientHello) -> Option<Arc<CertifiedKey>> {
        Some(self.0.clone())
    }
}

pub struct TlsListener<A: Accept, H: for<'a> TlsHandler<'a, A>> {
    pub accept: A,
    pub tls_handler: H,
    in_progress: SyncMutex<
        FuturesUnordered<
            BoxFuture<
                'static,
                (
                    H,
                    Result<Option<(TlsMetadata<A::Metadata>, AcceptStream)>, Error>,
                ),
            >,
        >,
    >,
}
impl<A: Accept, H: for<'a> TlsHandler<'a, A>> TlsListener<A, H> {
    pub fn new(accept: A, cert_handler: H) -> Self {
        Self {
            accept,
            tls_handler: cert_handler,
            in_progress: SyncMutex::new(FuturesUnordered::new()),
        }
    }
}
impl<A, H> Accept for TlsListener<A, H>
where
    A: Accept + 'static,
    A::Metadata: Send + 'static,
    for<'a> H: TlsHandler<'a, A> + Clone + Send + 'static,
{
    type Metadata = TlsMetadata<A::Metadata>;
    fn poll_accept(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(Self::Metadata, AcceptStream), Error>> {
        self.in_progress.mutate(|in_progress| {
            loop {
                if !in_progress.is_empty() {
                    if let Poll::Ready(Some((handler, res))) = in_progress.poll_next_unpin(cx) {
                        if let Some(res) = res.transpose() {
                            self.tls_handler = handler;
                            return Poll::Ready(res);
                        }
                        continue;
                    }
                }

                let (metadata, stream) = ready!(self.accept.poll_accept(cx)?);
                let mut tls_handler = self.tls_handler.clone();
                let mut fut = async move {
                    let res = async {
                        let mut acceptor = LazyConfigAcceptor::new(
                            Acceptor::default(),
                            BackTrackingIO::new(stream),
                        );
                        let mut mid: tokio_rustls::StartHandshake<BackTrackingIO<AcceptStream>> =
                            match (&mut acceptor).await {
                                Ok(a) => a,
                                Err(e) => {
                                    let mut stream =
                                        acceptor.take_io().or_not_found("acceptor io")?;
                                    let (_, buf) = stream.rewind();
                                    if std::str::from_utf8(buf)
                                        .ok()
                                        .and_then(|buf| {
                                            buf.lines()
                                                .map(|l| l.trim())
                                                .filter(|l| !l.is_empty())
                                                .next()
                                        })
                                        .map_or(false, |buf| {
                                            regex::Regex::new("[A-Z]+ (.+) HTTP/1")
                                                .unwrap()
                                                .is_match(buf)
                                        })
                                    {
                                        handle_http_on_https(stream).await.log_err();

                                        return Ok(None);
                                    } else {
                                        return Err(e).with_kind(ErrorKind::Network);
                                    }
                                }
                            };
                        let hello = mid.client_hello();
                        if let Some(cfg) = tls_handler.get_config(&hello, &metadata).await {
                            let metadata = TlsMetadata {
                                inner: metadata,
                                tls_info: TlsHandshakeInfo {
                                    sni: hello.server_name().map(InternedString::intern),
                                    alpn: hello
                                        .alpn()
                                        .into_iter()
                                        .flatten()
                                        .map(|a| MaybeUtf8String(a.to_vec()))
                                        .collect(),
                                },
                            };
                            let buffered = mid.io.stop_buffering();
                            mid.io
                                .write_all(&buffered)
                                .await
                                .with_kind(ErrorKind::Network)?;
                            let stream = match mid.into_stream(Arc::new(cfg)).await {
                                Ok(stream) => Box::pin(stream) as AcceptStream,
                                Err(e) => {
                                    tracing::trace!("Error completing TLS handshake: {e}");
                                    tracing::trace!("{e:?}");
                                    return Ok(None);
                                }
                            };
                            return Ok(Some((metadata, stream)));
                        }

                        Ok(None)
                    }
                    .await;
                    (tls_handler, res)
                }
                .boxed();
                match fut.poll_unpin(cx) {
                    Poll::Pending => {
                        in_progress.push(fut);
                        return Poll::Pending;
                    }
                    Poll::Ready((handler, res)) => {
                        if let Some(res) = res.transpose() {
                            self.tls_handler = handler;
                            return Poll::Ready(res);
                        }
                    }
                };
            }
        })
    }
}

async fn handle_http_on_https(stream: impl ReadWriter + Unpin + 'static) -> Result<(), Error> {
    use axum::body::Body;
    use axum::extract::Request;
    use axum::response::Response;
    use http::Uri;

    use crate::net::static_server::server_error;

    hyper_util::server::conn::auto::Builder::new(hyper_util::rt::TokioExecutor::new())
        .serve_connection(
            hyper_util::rt::TokioIo::new(stream),
            hyper_util::service::TowerToHyperService::new(axum::Router::new().fallback(
                axum::routing::method_routing::any(move |req: Request| async move {
                    match async move {
                        let host = req
                            .headers()
                            .get(http::header::HOST)
                            .and_then(|host| host.to_str().ok());
                        if let Some(host) = host {
                            let uri = Uri::from_parts({
                                let mut parts = req.uri().to_owned().into_parts();
                                parts.scheme = Some("https".parse()?);
                                parts.authority = Some(host.parse()?);
                                parts
                            })?;
                            Response::builder()
                                .status(http::StatusCode::TEMPORARY_REDIRECT)
                                .header(http::header::LOCATION, uri.to_string())
                                .body(Body::default())
                        } else {
                            Response::builder()
                                .status(http::StatusCode::BAD_REQUEST)
                                .body(Body::from("Host header required"))
                        }
                    }
                    .await
                    {
                        Ok(a) => a,
                        Err(e) => {
                            tracing::warn!("Error redirecting http request on ssl port: {e}");
                            tracing::error!("{e:?}");
                            server_error(Error::new(e, ErrorKind::Network))
                        }
                    }
                }),
            )),
        )
        .await
        .map_err(|e| Error::new(color_eyre::eyre::Report::msg(e), ErrorKind::Network))
}

pub fn client_config<'a, I: IntoIterator<Item = &'a X509Ref>>(
    crypto_provider: Arc<CryptoProvider>,
    root_certs: I,
) -> Result<ClientConfig, Error> {
    let mut certs = RootCertStore::empty();
    for cert in root_certs {
        certs
            .add(CertificateDer::from_slice(&cert.to_der()?))
            .with_kind(ErrorKind::OpenSsl)?;
    }
    Ok(ClientConfig::builder_with_provider(crypto_provider.clone())
        .with_safe_default_protocol_versions()
        .with_kind(ErrorKind::OpenSsl)?
        .with_root_certificates(certs)
        .with_no_client_auth())
}
