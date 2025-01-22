use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, SocketAddr};
use std::sync::{Arc, Weak};
use std::time::Duration;

use async_acme::acme::{Identifier, ACME_TLS_ALPN_NAME};
use axum::body::Body;
use axum::extract::Request;
use axum::response::Response;
use color_eyre::eyre::eyre;
use futures::FutureExt;
use helpers::NonDetachingJoinHandle;
use http::Uri;
use imbl_value::InternedString;
use models::ResultExt;
use rpc_toolkit::{from_fn, Context, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWriteExt;
use tokio::net::TcpStream;
use tokio::sync::watch;
use tokio_rustls::rustls::crypto::CryptoProvider;
use tokio_rustls::rustls::pki_types::{
    CertificateDer, PrivateKeyDer, PrivatePkcs8KeyDer, ServerName,
};
use tokio_rustls::rustls::server::{Acceptor, ResolvesServerCert};
use tokio_rustls::rustls::sign::CertifiedKey;
use tokio_rustls::rustls::{RootCertStore, ServerConfig};
use tokio_rustls::{LazyConfigAcceptor, TlsConnector};
use tokio_stream::wrappers::WatchStream;
use tokio_stream::StreamExt;
use tracing::instrument;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::db::model::Database;
use crate::net::acme::{AcmeCertCache, AcmeProvider};
use crate::net::network_interface::{
    Accepted, NetworkInterfaceController, NetworkInterfaceListener,
};
use crate::net::static_server::server_error;
use crate::prelude::*;
use crate::util::io::BackTrackingIO;
use crate::util::serde::{display_serializable, HandlerExtSerde, MaybeUtf8String};
use crate::util::sync::SyncMutex;

pub fn vhost_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand(
        "dump-table",
        from_fn(|ctx: RpcContext| Ok(ctx.net_controller.vhost.dump_table()))
            .with_display_serializable()
            .with_custom_display_fn(|HandlerArgs { params, .. }, res| {
                use prettytable::*;

                if let Some(format) = params.format {
                    display_serializable(format, res);
                    return Ok::<_, Error>(());
                }

                let mut table = Table::new();
                table.add_row(row![bc => "FROM", "TO", "PUBLIC", "ACME", "CONNECT SSL", "ACTIVE"]);

                for (external, targets) in res {
                    for (host, targets) in targets {
                        for (idx, target) in targets.into_iter().enumerate() {
                            table.add_row(row![
                                format!(
                                    "{}:{}",
                                    host.as_ref().map(|s| &**s).unwrap_or("*"),
                                    external.0
                                ),
                                target.addr,
                                target.public,
                                target.acme.as_ref().map(|a| a.0.as_str()).unwrap_or("NONE"),
                                target.connect_ssl.is_ok(),
                                idx == 0
                            ]);
                        }
                    }
                }

                table.print_tty(false)?;

                Ok(())
            })
            .with_call_remote::<CliContext>(),
    )
}

#[derive(Debug)]
struct SingleCertResolver(Arc<CertifiedKey>);
impl ResolvesServerCert for SingleCertResolver {
    fn resolve(&self, _: tokio_rustls::rustls::server::ClientHello) -> Option<Arc<CertifiedKey>> {
        Some(self.0.clone())
    }
}

// not allowed: <=1024, >=32768, 5355, 5432, 9050, 6010, 9051, 5353

pub struct VHostController {
    db: TypedPatchDb<Database>,
    interfaces: Arc<NetworkInterfaceController>,
    crypto_provider: Arc<CryptoProvider>,
    acme_tls_alpn_cache: AcmeTlsAlpnCache,
    servers: SyncMutex<BTreeMap<u16, VHostServer>>,
}
impl VHostController {
    pub fn new(db: TypedPatchDb<Database>, interfaces: Arc<NetworkInterfaceController>) -> Self {
        Self {
            db,
            interfaces,
            crypto_provider: Arc::new(tokio_rustls::rustls::crypto::ring::default_provider()),
            acme_tls_alpn_cache: Arc::new(SyncMutex::new(BTreeMap::new())),
            servers: SyncMutex::new(BTreeMap::new()),
        }
    }
    #[instrument(skip_all)]
    pub fn add(
        &self,
        hostname: Option<InternedString>,
        external: u16,
        TargetInfo {
            public,
            acme,
            addr,
            connect_ssl,
        }: TargetInfo,
    ) -> Result<Arc<()>, Error> {
        self.servers.mutate(|writable| {
            let server = if let Some(server) = writable.remove(&external) {
                server
            } else {
                VHostServer::new(
                    external,
                    self.db.clone(),
                    self.interfaces.clone(),
                    self.crypto_provider.clone(),
                    self.acme_tls_alpn_cache.clone(),
                )?
            };
            let rc = server.add(
                hostname,
                TargetInfo {
                    public,
                    acme,
                    addr,
                    connect_ssl,
                },
            );
            writable.insert(external, server);
            Ok(rc?)
        })
    }

    pub fn dump_table(
        &self,
    ) -> BTreeMap<JsonKey<u16>, BTreeMap<JsonKey<Option<InternedString>>, BTreeSet<TargetInfo>>>
    {
        self.servers.peek(|s| {
            s.iter()
                .map(|(k, v)| {
                    (
                        JsonKey::new(*k),
                        v.mapping
                            .borrow()
                            .iter()
                            .map(|(k, v)| {
                                (
                                    JsonKey::new(k.clone()),
                                    v.iter()
                                        .filter(|(_, v)| v.strong_count() > 0)
                                        .map(|(k, _)| k)
                                        .cloned()
                                        .collect(),
                                )
                            })
                            .collect(),
                    )
                })
                .collect()
        })
    }

    #[instrument(skip_all)]
    pub fn gc(&self, hostname: Option<InternedString>, external: u16) {
        self.servers.mutate(|writable| {
            if let Some(server) = writable.remove(&external) {
                server.gc(hostname);
                if !server.is_empty() {
                    writable.insert(external, server);
                }
            }
        })
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct TargetInfo {
    pub public: bool,
    pub acme: Option<AcmeProvider>,
    pub addr: SocketAddr,
    pub connect_ssl: Result<(), AlpnInfo>, // Ok: yes, connect using ssl, pass through alpn; Err: connect tcp, use provided strategy for alpn
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum AlpnInfo {
    Reflect,
    Specified(Vec<MaybeUtf8String>),
}
impl Default for AlpnInfo {
    fn default() -> Self {
        Self::Reflect
    }
}

type AcmeTlsAlpnCache =
    Arc<SyncMutex<BTreeMap<InternedString, watch::Receiver<Option<Arc<CertifiedKey>>>>>>;
type Mapping = BTreeMap<Option<InternedString>, BTreeMap<TargetInfo, Weak<()>>>;

struct VHostServer {
    mapping: watch::Sender<Mapping>,
    _thread: NonDetachingJoinHandle<()>,
}

impl VHostServer {
    async fn accept(
        listener: &mut NetworkInterfaceListener,
        mut mapping: watch::Receiver<Mapping>,
        db: TypedPatchDb<Database>,
        acme_tls_alpn_cache: AcmeTlsAlpnCache,
        crypto_provider: Arc<CryptoProvider>,
    ) -> Result<(), Error> {
        let accepted;

        loop {
            let any_public = mapping
                .borrow()
                .iter()
                .any(|(_, targets)| targets.iter().any(|(target, _)| target.public));

            let changed_public = mapping
                .wait_for(|m| {
                    m.iter()
                        .any(|(_, targets)| targets.iter().any(|(target, _)| target.public))
                        != any_public
                })
                .boxed();

            tokio::select! {
                a = listener.accept(any_public) => {
                    accepted = a?;
                    break;
                }
                _ = changed_public => {
                    tracing::debug!("port {} {} public bindings", listener.port(), if any_public { "no longer has" } else { "now has" });
                }
            }
        }

        if let Err(e) = socket2::SockRef::from(&accepted.stream).set_tcp_keepalive(
            &socket2::TcpKeepalive::new()
                .with_time(Duration::from_secs(900))
                .with_interval(Duration::from_secs(60))
                .with_retries(5),
        ) {
            tracing::error!("Failed to set tcp keepalive: {e}");
            tracing::debug!("{e:?}");
        }

        tokio::spawn(async move {
            let bind = accepted.bind;
            if let Err(e) =
                Self::handle_stream(accepted, mapping, db, acme_tls_alpn_cache, crypto_provider)
                    .await
            {
                tracing::error!("Error in VHostController on {bind}: {e}");
                tracing::debug!("{e:?}")
            }
        });
        Ok(())
    }

    async fn handle_stream(
        Accepted {
            stream,
            is_public,
            wan_ip,
            bind,
            ..
        }: Accepted,
        mapping: watch::Receiver<Mapping>,
        db: TypedPatchDb<Database>,
        acme_tls_alpn_cache: AcmeTlsAlpnCache,
        crypto_provider: Arc<CryptoProvider>,
    ) -> Result<(), Error> {
        let mut stream = BackTrackingIO::new(stream);
        let mid: tokio_rustls::StartHandshake<&mut BackTrackingIO<TcpStream>> =
            match LazyConfigAcceptor::new(Acceptor::default(), &mut stream).await {
                Ok(a) => a,
                Err(e) => {
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
                        return hyper_util::server::conn::auto::Builder::new(
                            hyper_util::rt::TokioExecutor::new(),
                        )
                        .serve_connection(
                            hyper_util::rt::TokioIo::new(stream),
                            hyper_util::service::TowerToHyperService::new(
                                axum::Router::new().fallback(axum::routing::method_routing::any(
                                    move |req: Request| async move {
                                        match async move {
                                            let host = req
                                                .headers()
                                                .get(http::header::HOST)
                                                .and_then(|host| host.to_str().ok());
                                            if let Some(host) = host {
                                                let uri = Uri::from_parts({
                                                    let mut parts =
                                                        req.uri().to_owned().into_parts();
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
                                                tracing::warn!(
                                                "Error redirecting http request on ssl port: {e}"
                                            );
                                                tracing::error!("{e:?}");
                                                server_error(Error::new(e, ErrorKind::Network))
                                            }
                                        }
                                    },
                                )),
                            ),
                        )
                        .await
                        .map_err(|e| {
                            Error::new(color_eyre::eyre::Report::msg(e), ErrorKind::Network)
                        });
                    } else {
                        return Err(e).with_kind(ErrorKind::Network);
                    }
                }
            };
        let target_name: Option<InternedString> =
            mid.client_hello().server_name().map(|s| s.into());
        if let Some(domain) = target_name.as_ref() {
            if mid
                .client_hello()
                .alpn()
                .into_iter()
                .flatten()
                .any(|alpn| alpn == ACME_TLS_ALPN_NAME)
            {
                let cert = WatchStream::new(
                    acme_tls_alpn_cache
                        .peek(|c| c.get(&**domain).cloned())
                        .ok_or_else(|| {
                            Error::new(
                                eyre!("No challenge recv available for {domain}"),
                                ErrorKind::OpenSsl,
                            )
                        })?,
                );
                tracing::info!("Waiting for verification cert for {domain}");
                let cert = cert
                    .filter(|c| c.is_some())
                    .next()
                    .await
                    .flatten()
                    .ok_or_else(|| {
                        Error::new(
                            eyre!("No challenge available for {domain}"),
                            ErrorKind::OpenSsl,
                        )
                    })?;
                tracing::info!("Verification cert received for {domain}");
                let mut cfg = ServerConfig::builder_with_provider(crypto_provider.clone())
                    .with_safe_default_protocol_versions()
                    .with_kind(crate::ErrorKind::OpenSsl)?
                    .with_no_client_auth()
                    .with_cert_resolver(Arc::new(SingleCertResolver(cert)));

                cfg.alpn_protocols = vec![ACME_TLS_ALPN_NAME.to_vec()];
                tracing::info!("performing ACME auth challenge");
                let mut accept = mid.into_stream(Arc::new(cfg));
                let io = accept.get_mut().unwrap();
                let buffered = io.stop_buffering();
                io.write_all(&buffered).await?;
                accept.await?;
                tracing::info!("ACME auth challenge completed");
                return Ok(());
            }
        }
        let target = {
            let m = mapping.borrow();
            m.get(&target_name)
                .into_iter()
                .flatten()
                .find(|(_, rc)| rc.strong_count() > 0)
                .or_else(|| {
                    if target_name
                        .as_ref()
                        .map(|s| s.parse::<IpAddr>().is_ok())
                        .unwrap_or(true)
                    {
                        m.get(&None)
                            .into_iter()
                            .flatten()
                            .find(|(_, rc)| rc.strong_count() > 0)
                    } else {
                        None
                    }
                })
                .map(|(target, _)| target.clone())
        };
        if let Some(target) = target {
            if is_public && !target.public {
                log::warn!(
                    "Rejecting connection from public interface to private bind: {bind} -> {target:?}"
                );
                return Ok(());
            }
            let peek = db.peek().await;
            let root = peek
                .as_private()
                .as_key_store()
                .as_local_certs()
                .as_root_cert()
                .de()?;
            let mut cfg = async {
                if let Some((domain, provider, settings)) =
                    target_name.as_ref().and_then(|domain| {
                        target.acme.as_ref().and_then(|a| {
                            peek.as_public()
                                .as_server_info()
                                .as_acme()
                                .as_idx(a)
                                .map(|s| (domain, a, s))
                        })
                    })
                {
                    let acme_settings = settings.de()?;
                    let mut identifiers = vec![Identifier::Dns(domain.to_string())];
                    if false
                    // Requires RFC 8738
                    {
                        if let Some(wan_ip) = wan_ip {
                            identifiers.push(Identifier::Ip(wan_ip.into()));
                        }
                    }
                    let (send, recv) = watch::channel(None);
                    acme_tls_alpn_cache.mutate(|c| c.insert(domain.clone(), recv));
                    let cert = async_acme::rustls_helper::order(
                        |_, cert| {
                            send.send_replace(Some(Arc::new(cert)));
                            Ok(())
                        },
                        provider.0.as_str(),
                        &identifiers,
                        Some(&AcmeCertCache(&db)),
                        &acme_settings.contact,
                    )
                    .await
                    .with_kind(ErrorKind::OpenSsl)?;
                    return Ok(ServerConfig::builder_with_provider(crypto_provider.clone())
                        .with_safe_default_protocol_versions()
                        .with_kind(crate::ErrorKind::OpenSsl)?
                        .with_no_client_auth()
                        .with_cert_resolver(Arc::new(SingleCertResolver(Arc::new(cert)))));
                }

                let hostnames = target_name
                    .into_iter()
                    .chain([InternedString::from_display(&bind.ip())])
                    .chain(wan_ip.as_ref().map(InternedString::from_display))
                    .collect();
                let key = db
                    .mutate(|v| {
                        v.as_private_mut()
                            .as_key_store_mut()
                            .as_local_certs_mut()
                            .cert_for(&hostnames)
                    })
                    .await?;
                let cfg = ServerConfig::builder_with_provider(crypto_provider.clone())
                    .with_safe_default_protocol_versions()
                    .with_kind(crate::ErrorKind::OpenSsl)?
                    .with_no_client_auth();
                if mid
                    .client_hello()
                    .signature_schemes()
                    .contains(&tokio_rustls::rustls::SignatureScheme::ED25519)
                {
                    cfg.with_single_cert(
                        key.fullchain_ed25519()
                            .into_iter()
                            .map(|c| {
                                Ok(tokio_rustls::rustls::pki_types::CertificateDer::from(
                                    c.to_der()?,
                                ))
                            })
                            .collect::<Result<_, Error>>()?,
                        PrivateKeyDer::from(PrivatePkcs8KeyDer::from(
                            key.leaf.keys.ed25519.private_key_to_pkcs8()?,
                        )),
                    )
                } else {
                    cfg.with_single_cert(
                        key.fullchain_nistp256()
                            .into_iter()
                            .map(|c| {
                                Ok(tokio_rustls::rustls::pki_types::CertificateDer::from(
                                    c.to_der()?,
                                ))
                            })
                            .collect::<Result<_, Error>>()?,
                        PrivateKeyDer::from(PrivatePkcs8KeyDer::from(
                            key.leaf.keys.nistp256.private_key_to_pkcs8()?,
                        )),
                    )
                }
                .with_kind(crate::ErrorKind::OpenSsl)
            }
            .await?;
            let mut tcp_stream = TcpStream::connect(target.addr).await?;
            match target.connect_ssl {
                Ok(()) => {
                    let mut client_cfg =
                        tokio_rustls::rustls::ClientConfig::builder_with_provider(crypto_provider)
                            .with_safe_default_protocol_versions()
                            .with_kind(crate::ErrorKind::OpenSsl)?
                            .with_root_certificates({
                                let mut store = RootCertStore::empty();
                                store
                                    .add(CertificateDer::from(root.to_der()?))
                                    .with_kind(crate::ErrorKind::OpenSsl)?;
                                store
                            })
                            .with_no_client_auth();
                    client_cfg.alpn_protocols = mid
                        .client_hello()
                        .alpn()
                        .into_iter()
                        .flatten()
                        .map(|x| x.to_vec())
                        .collect();
                    let mut target_stream = TlsConnector::from(Arc::new(client_cfg))
                        .connect_with(
                            ServerName::IpAddress(target.addr.ip().into()),
                            tcp_stream,
                            |conn| {
                                cfg.alpn_protocols
                                    .extend(conn.alpn_protocol().into_iter().map(|p| p.to_vec()))
                            },
                        )
                        .await
                        .with_kind(crate::ErrorKind::OpenSsl)?;
                    let mut accept = mid.into_stream(Arc::new(cfg));
                    let io = accept.get_mut().unwrap();
                    let buffered = io.stop_buffering();
                    io.write_all(&buffered).await?;
                    let mut tls_stream = match accept.await {
                        Ok(a) => a,
                        Err(e) => {
                            tracing::trace!(
                                "VHostController: failed to accept TLS connection on {bind}: {e}"
                            );
                            tracing::trace!("{e:?}");
                            return Ok(());
                        }
                    };
                    tokio::io::copy_bidirectional(&mut tls_stream, &mut target_stream).await
                }
                Err(AlpnInfo::Reflect) => {
                    for proto in mid.client_hello().alpn().into_iter().flatten() {
                        cfg.alpn_protocols.push(proto.into());
                    }
                    let mut accept = mid.into_stream(Arc::new(cfg));
                    let io = accept.get_mut().unwrap();
                    let buffered = io.stop_buffering();
                    io.write_all(&buffered).await?;
                    let mut tls_stream = match accept.await {
                        Ok(a) => a,
                        Err(e) => {
                            tracing::trace!(
                                "VHostController: failed to accept TLS connection on {bind}: {e}"
                            );
                            tracing::trace!("{e:?}");
                            return Ok(());
                        }
                    };
                    tokio::io::copy_bidirectional(&mut tls_stream, &mut tcp_stream).await
                }
                Err(AlpnInfo::Specified(alpn)) => {
                    cfg.alpn_protocols = alpn.into_iter().map(|a| a.0).collect();
                    let mut accept = mid.into_stream(Arc::new(cfg));
                    let io = accept.get_mut().unwrap();
                    let buffered = io.stop_buffering();
                    io.write_all(&buffered).await?;
                    let mut tls_stream = match accept.await {
                        Ok(a) => a,
                        Err(e) => {
                            tracing::trace!(
                                "VHostController: failed to accept TLS connection on {bind}: {e}"
                            );
                            tracing::trace!("{e:?}");
                            return Ok(());
                        }
                    };
                    tokio::io::copy_bidirectional(&mut tls_stream, &mut tcp_stream).await
                }
            }
            .map_or_else(
                |e| {
                    use std::io::ErrorKind as E;
                    match e.kind() {
                        E::UnexpectedEof
                        | E::BrokenPipe
                        | E::ConnectionAborted
                        | E::ConnectionReset
                        | E::ConnectionRefused
                        | E::TimedOut
                        | E::Interrupted
                        | E::NotConnected => Ok(()),
                        _ => Err(e),
                    }
                },
                |_| Ok(()),
            )?;
        } else {
            // 503
        }
        Ok::<_, Error>(())
    }

    #[instrument(skip_all)]
    fn new(
        port: u16,
        db: TypedPatchDb<Database>,
        iface_ctrl: Arc<NetworkInterfaceController>,
        crypto_provider: Arc<CryptoProvider>,
        acme_tls_alpn_cache: AcmeTlsAlpnCache,
    ) -> Result<Self, Error> {
        let mut listener = iface_ctrl.bind(port).with_kind(crate::ErrorKind::Network)?;
        let (map_send, map_recv) = watch::channel(BTreeMap::new());
        Ok(Self {
            mapping: map_send,
            _thread: tokio::spawn(async move {
                loop {
                    if let Err(e) = Self::accept(
                        &mut listener,
                        map_recv.clone(),
                        db.clone(),
                        acme_tls_alpn_cache.clone(),
                        crypto_provider.clone(),
                    )
                    .await
                    {
                        tracing::error!(
                            "VHostController: failed to accept connection on {port}: {e}"
                        );
                        tracing::debug!("{e:?}");
                    }
                }
            })
            .into(),
        })
    }
    fn add(&self, hostname: Option<InternedString>, target: TargetInfo) -> Result<Arc<()>, Error> {
        let mut res = Ok(Arc::new(()));
        self.mapping.send_if_modified(|writable| {
            let mut changed = false;
            let mut targets = writable.remove(&hostname).unwrap_or_default();
            let rc = if let Some(rc) = Weak::upgrade(&targets.remove(&target).unwrap_or_default()) {
                rc
            } else {
                changed = true;
                Arc::new(())
            };
            targets.insert(target, Arc::downgrade(&rc));
            writable.insert(hostname, targets);
            res = Ok(rc);
            changed
        });
        if !self.mapping.is_closed() {
            res
        } else {
            Err(Error::new(
                eyre!("VHost Service Thread has exited"),
                crate::ErrorKind::Network,
            ))
        }
    }
    fn gc(&self, hostname: Option<InternedString>) {
        self.mapping.send_if_modified(|writable| {
            let mut targets = writable.remove(&hostname).unwrap_or_default();
            let pre = targets.len();
            targets = targets
                .into_iter()
                .filter(|(_, rc)| rc.strong_count() > 0)
                .collect();
            let post = targets.len();
            if !targets.is_empty() {
                writable.insert(hostname, targets);
            }
            pre == post
        });
    }
    fn is_empty(&self) -> bool {
        self.mapping.borrow().is_empty()
    }
}
