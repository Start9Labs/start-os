use std::any::Any;
use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, SocketAddr};
use std::sync::{Arc, Weak};
use std::task::{Poll, ready};

use async_acme::acme::ACME_TLS_ALPN_NAME;
use color_eyre::eyre::eyre;
use futures::FutureExt;
use futures::future::BoxFuture;
use helpers::NonDetachingJoinHandle;
use imbl_value::{InOMap, InternedString};
use models::ResultExt;
use rpc_toolkit::{Context, HandlerArgs, HandlerExt, ParentHandler, from_fn};
use serde::{Deserialize, Serialize};
use tokio::net::TcpStream;
use tokio_rustls::TlsConnector;
use tokio_rustls::rustls::crypto::CryptoProvider;
use tokio_rustls::rustls::pki_types::ServerName;
use tokio_rustls::rustls::server::ClientHello;
use tokio_rustls::rustls::{ClientConfig, ServerConfig};
use tracing::instrument;
use ts_rs::TS;
use visit_rs::Visit;

use crate::context::{CliContext, RpcContext};
use crate::db::model::Database;
use crate::db::model::public::AcmeSettings;
use crate::db::{DbAccessByKey, DbAccessMut};
use crate::net::acme::{
    AcmeCertStore, AcmeProvider, AcmeTlsAlpnCache, AcmeTlsHandler, GetAcmeProvider,
};
use crate::net::gateway::{
    AnyFilter, BindTcp, DynInterfaceFilter, GatewayInfo, InterfaceFilter,
    NetworkInterfaceController, NetworkInterfaceListener,
};
use crate::net::ssl::{CertStore, RootCaTlsHandler};
use crate::net::tls::{
    ChainedHandler, TlsHandlerWrapper, TlsListener, TlsMetadata, WrapTlsHandler,
};
use crate::net::web_server::{Accept, AcceptStream, ExtractVisitor, TcpMetadata, extract};
use crate::prelude::*;
use crate::util::collections::EqSet;
use crate::util::serde::{HandlerExtSerde, MaybeUtf8String, display_serializable};
use crate::util::sync::{SyncMutex, Watch};

pub fn vhost_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand(
        "dump-table",
        from_fn(|ctx: RpcContext| Ok(ctx.net_controller.vhost.dump_table()))
            .with_display_serializable()
            .with_custom_display_fn(|HandlerArgs { params, .. }, res| {
                use prettytable::*;

                if let Some(format) = params.format {
                    display_serializable(format, res)?;
                    return Ok::<_, Error>(());
                }

                let mut table = Table::new();
                table.add_row(row![bc => "FROM", "TO", "ACTIVE"]);

                for (external, targets) in res {
                    for (host, targets) in targets {
                        for (idx, target) in targets.into_iter().enumerate() {
                            table.add_row(row![
                                format!(
                                    "{}:{}",
                                    host.as_ref().map(|s| &**s).unwrap_or("*"),
                                    external.0
                                ),
                                target,
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

// not allowed: <=1024, >=32768, 5355, 5432, 9050, 6010, 9051, 5353

pub struct VHostController {
    db: TypedPatchDb<Database>,
    interfaces: Arc<NetworkInterfaceController>,
    crypto_provider: Arc<CryptoProvider>,
    acme_cache: AcmeTlsAlpnCache,
    servers: SyncMutex<BTreeMap<u16, VHostServer<NetworkInterfaceListener>>>,
}
impl VHostController {
    pub fn new(
        db: TypedPatchDb<Database>,
        interfaces: Arc<NetworkInterfaceController>,
        crypto_provider: Arc<CryptoProvider>,
    ) -> Self {
        Self {
            db,
            interfaces,
            crypto_provider,
            acme_cache: Arc::new(SyncMutex::new(BTreeMap::new())),
            servers: SyncMutex::new(BTreeMap::new()),
        }
    }
    #[instrument(skip_all)]
    pub fn add(
        &self,
        hostname: Option<InternedString>,
        external: u16,
        target: DynVHostTarget<NetworkInterfaceListener>,
    ) -> Result<Arc<()>, Error> {
        self.servers.mutate(|writable| {
            let server = if let Some(server) = writable.remove(&external) {
                server
            } else {
                VHostServer::new(
                    self.interfaces.watcher.bind(BindTcp, external)?,
                    self.db.clone(),
                    self.crypto_provider.clone(),
                    self.acme_cache.clone(),
                )
            };
            let rc = server.add(hostname, target);
            writable.insert(external, server);
            Ok(rc?)
        })
    }

    pub fn dump_table(
        &self,
    ) -> BTreeMap<JsonKey<u16>, BTreeMap<JsonKey<Option<InternedString>>, EqSet<String>>> {
        let ip_info = self.interfaces.watcher.ip_info();
        self.servers.peek(|s| {
            s.iter()
                .map(|(k, v)| {
                    (
                        JsonKey::new(*k),
                        v.mapping.peek(|m| {
                            m.iter()
                                .map(|(k, v)| {
                                    (
                                        JsonKey::new(k.clone()),
                                        v.iter()
                                            .filter(|(_, v)| v.strong_count() > 0)
                                            .map(|(k, _)| format!("{k:?}"))
                                            .collect(),
                                    )
                                })
                                .collect()
                        }),
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

pub trait VHostTarget<A: Accept>: std::fmt::Debug + Eq {
    type PreprocessRes: Send + 'static;
    #[allow(unused_variables)]
    fn filter(&self, metadata: &<A as Accept>::Metadata) -> bool {
        true
    }
    fn acme(&self) -> Option<&AcmeProvider> {
        None
    }
    fn preprocess<'a>(
        &'a self,
        prev: ServerConfig,
        hello: &'a ClientHello<'a>,
        metadata: &'a <A as Accept>::Metadata,
    ) -> impl Future<Output = Option<(ServerConfig, Self::PreprocessRes)>> + Send + 'a;
    fn handle_stream(&self, stream: AcceptStream, prev: Self::PreprocessRes);
}

pub trait DynVHostTargetT<A: Accept>: std::fmt::Debug + Any {
    fn filter(&self, metadata: &<A as Accept>::Metadata) -> bool;
    fn acme(&self) -> Option<&AcmeProvider>;
    fn preprocess<'a>(
        &'a self,
        prev: ServerConfig,
        hello: &'a ClientHello<'a>,
        metadata: &'a <A as Accept>::Metadata,
    ) -> BoxFuture<'a, Option<(ServerConfig, Box<dyn Any + Send>)>>;
    fn handle_stream(&self, stream: AcceptStream, prev: Box<dyn Any + Send>);
    fn eq(&self, other: &dyn DynVHostTargetT<A>) -> bool;
}
impl<A: Accept, T: VHostTarget<A> + 'static> DynVHostTargetT<A> for T {
    fn filter(&self, metadata: &<A as Accept>::Metadata) -> bool {
        VHostTarget::filter(self, metadata)
    }
    fn acme(&self) -> Option<&AcmeProvider> {
        VHostTarget::acme(self)
    }
    fn preprocess<'a>(
        &'a self,
        prev: ServerConfig,
        hello: &'a ClientHello<'a>,
        metadata: &'a <A as Accept>::Metadata,
    ) -> BoxFuture<'a, Option<(ServerConfig, Box<dyn Any + Send>)>> {
        VHostTarget::preprocess(self, prev, hello, metadata)
            .map(|o| o.map(|(cfg, res)| (cfg, Box::new(res) as Box<dyn Any + Send>)))
            .boxed()
    }
    fn handle_stream(&self, stream: AcceptStream, prev: Box<dyn Any + Send>) {
        if let Ok(prev) = prev.downcast() {
            VHostTarget::handle_stream(self, stream, *prev);
        }
    }
    fn eq(&self, other: &dyn DynVHostTargetT<A>) -> bool {
        Some(self) == (other as &dyn Any).downcast_ref()
    }
}

pub struct DynVHostTarget<A: Accept>(Arc<dyn DynVHostTargetT<A> + Send + Sync>);
impl<A: Accept> DynVHostTarget<A> {
    pub fn new<T: VHostTarget<A> + Send + Sync + 'static>(target: T) -> Self {
        Self(Arc::new(target))
    }
}
impl<A: Accept> Clone for DynVHostTarget<A> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<A: Accept> std::fmt::Debug for DynVHostTarget<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl<A: Accept + 'static> PartialEq for DynVHostTarget<A> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&*other.0)
    }
}
impl<A: Accept + 'static> Eq for DynVHostTarget<A> {}
struct Preprocessed<A: Accept>(DynVHostTarget<A>, Box<dyn Any + Send>);
impl<A: Accept + 'static> DynVHostTarget<A> {
    async fn into_preprocessed(
        self,
        prev: ServerConfig,
        hello: &ClientHello<'_>,
        metadata: &<A as Accept>::Metadata,
    ) -> Option<(ServerConfig, Preprocessed<A>)> {
        let (cfg, res) = self.0.preprocess(prev, hello, metadata).await?;
        Some((cfg, Preprocessed(self, res)))
    }
}
impl<A: Accept + 'static> Preprocessed<A> {
    fn finish(self, stream: AcceptStream) {
        (self.0).0.handle_stream(stream, self.1);
    }
}

#[derive(Debug, Clone)]
pub struct ProxyTarget {
    pub filter: DynInterfaceFilter,
    pub acme: Option<AcmeProvider>,
    pub addr: SocketAddr,
    pub connect_ssl: Result<Arc<ClientConfig>, AlpnInfo>, // Ok: yes, connect using ssl, pass through alpn; Err: connect tcp, use provided strategy for alpn
}
impl PartialEq for ProxyTarget {
    fn eq(&self, other: &Self) -> bool {
        self.filter == other.filter
            && self.addr == other.addr
            && self.connect_ssl.as_ref().map(Arc::as_ptr)
                == other.connect_ssl.as_ref().map(Arc::as_ptr)
    }
}
impl Eq for ProxyTarget {}

impl<A> VHostTarget<A> for ProxyTarget
where
    A: Accept + 'static,
    <A as Accept>::Metadata: Visit<ExtractVisitor<GatewayInfo>> + Clone + Send + Sync,
{
    type PreprocessRes = AcceptStream;
    fn filter(&self, metadata: &<A as Accept>::Metadata) -> bool {
        let info = extract::<GatewayInfo, _>(metadata);
        info.as_ref()
            .map_or(true, |i| self.filter.filter(&i.id, &i.info))
    }
    fn acme(&self) -> Option<&AcmeProvider> {
        self.acme.as_ref()
    }
    async fn preprocess<'a>(
        &'a self,
        mut prev: ServerConfig,
        hello: &'a ClientHello<'a>,
        metadata: &'a <A as Accept>::Metadata,
    ) -> Option<(ServerConfig, Self::PreprocessRes)> {
        let tcp_stream = TcpStream::connect(self.addr)
            .await
            .with_ctx(|_| (ErrorKind::Network, self.addr))
            .log_err()?;
        match &self.connect_ssl {
            Ok(client_cfg) => {
                let mut client_cfg = (&**client_cfg).clone();
                client_cfg.alpn_protocols = hello
                    .alpn()
                    .into_iter()
                    .flatten()
                    .map(|x| x.to_vec())
                    .collect();
                let target_stream = TlsConnector::from(Arc::new(client_cfg))
                    .connect_with(
                        ServerName::IpAddress(self.addr.ip().into()),
                        tcp_stream,
                        |conn| {
                            prev.alpn_protocols
                                .extend(conn.alpn_protocol().into_iter().map(|p| p.to_vec()))
                        },
                    )
                    .await
                    .log_err()?;
                return Some((prev, Box::pin(target_stream)));
            }
            Err(AlpnInfo::Reflect) => {
                for alpn in hello.alpn().into_iter().flatten() {
                    prev.alpn_protocols.push(alpn.to_vec());
                }
            }
            Err(AlpnInfo::Specified(a)) => {
                for alpn in a {
                    prev.alpn_protocols.push(alpn.0.clone());
                }
            }
        }
        Some((prev, Box::pin(tcp_stream)))
    }
    fn handle_stream(&self, mut stream: AcceptStream, mut prev: Self::PreprocessRes) {
        tokio::spawn(async move { tokio::io::copy_bidirectional(&mut stream, &mut prev).await });
    }
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

type Mapping<A: Accept> = BTreeMap<Option<InternedString>, InOMap<DynVHostTarget<A>, Weak<()>>>;

pub struct GetVHostAcmeProvider<A: Accept + 'static>(pub Watch<Mapping<A>>);
impl<A: Accept + 'static> Clone for GetVHostAcmeProvider<A> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<A: Accept + 'static> GetAcmeProvider for GetVHostAcmeProvider<A> {
    async fn get_provider<'a, 'b: 'a>(
        &'b self,
        san_info: &'a BTreeSet<InternedString>,
    ) -> Option<impl AsRef<AcmeProvider> + Send + 'b> {
        self.0.peek(|m| -> Option<AcmeProvider> {
            san_info
                .iter()
                .fold(Some::<Option<&AcmeProvider>>(None), |acc, x| {
                    let acc = acc?;
                    if x.parse::<IpAddr>().is_ok() {
                        return Some(acc);
                    }
                    let (t, _) = m
                        .get(&Some(x.clone()))?
                        .iter()
                        .find(|(_, rc)| rc.strong_count() > 0)?;
                    let acme = t.0.acme()?;
                    Some(if let Some(acc) = acc {
                        if acme == acc {
                            // all must match
                            Some(acme)
                        } else {
                            None
                        }
                    } else {
                        Some(acme)
                    })
                })
                .flatten()
                .cloned()
        })
    }
}

pub struct VHostConnector<A: Accept + 'static>(Watch<Mapping<A>>, Option<Preprocessed<A>>);
impl<A: Accept + 'static> Clone for VHostConnector<A> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), None)
    }
}

impl<A> WrapTlsHandler<A> for VHostConnector<A>
where
    A: Accept + 'static,
    <A as Accept>::Metadata: Visit<ExtractVisitor<GatewayInfo>> + Send + Sync,
{
    async fn wrap<'a>(
        &'a mut self,
        prev: ServerConfig,
        hello: &'a ClientHello<'a>,
        metadata: &'a <A as Accept>::Metadata,
    ) -> Option<ServerConfig>
    where
        Self: 'a,
    {
        if hello
            .alpn()
            .into_iter()
            .flatten()
            .any(|a| a == ACME_TLS_ALPN_NAME)
        {
            return Some(prev);
        }

        let target = self.0.peek(|m| {
            m.get(&hello.server_name().map(InternedString::from))
                .into_iter()
                .flatten()
                .filter(|(_, rc)| rc.strong_count() > 0)
                .find(|(t, _)| t.0.filter(metadata))
                .map(|(e, _)| e.clone())
        })?;

        let (prev, store) = target.into_preprocessed(prev, hello, metadata).await?;

        self.1 = Some(store);

        Some(prev)
    }
}

struct VHostListener<M, A>(
    TlsListener<
        A,
        TlsHandlerWrapper<
            ChainedHandler<Arc<AcmeTlsHandler<M, GetVHostAcmeProvider<A>>>, RootCaTlsHandler<M>>,
            VHostConnector<A>,
        >,
    >,
)
where
    for<'a> M: HasModel<Model = Model<M>>
        + DbAccessMut<CertStore>
        + DbAccessMut<AcmeCertStore>
        + DbAccessByKey<AcmeSettings, Key<'a> = &'a AcmeProvider>
        + Send
        + Sync,
    A: Accept + 'static,
    <A as Accept>::Metadata: Visit<ExtractVisitor<TcpMetadata>>
        + Visit<ExtractVisitor<GatewayInfo>>
        + Clone
        + Send
        + Sync
        + 'static;
struct VHostListenerMetadata<A: Accept> {
    inner: TlsMetadata<A::Metadata>,
    preprocessed: Preprocessed<A>,
}
impl<M, A> Accept for VHostListener<M, A>
where
    for<'a> M: HasModel<Model = Model<M>>
        + DbAccessMut<CertStore>
        + DbAccessMut<AcmeCertStore>
        + DbAccessByKey<AcmeSettings, Key<'a> = &'a AcmeProvider>
        + Send
        + Sync
        + 'static,
    A: Accept + 'static,
    <A as Accept>::Metadata: Visit<ExtractVisitor<TcpMetadata>>
        + Visit<ExtractVisitor<GatewayInfo>>
        + Clone
        + Send
        + Sync
        + 'static,
{
    type Metadata = VHostListenerMetadata<A>;
    fn poll_accept(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(Self::Metadata, AcceptStream), Error>> {
        let (metadata, stream) = ready!(self.0.poll_accept(cx)?);
        let preprocessed = self.0.tls_handler.wrapper.1.take();
        Poll::Ready(Ok((
            VHostListenerMetadata {
                inner: metadata,
                preprocessed: preprocessed.ok_or_else(|| {
                    Error::new(
                        eyre!("tlslistener yielded but preprocessed isn't set"),
                        ErrorKind::Incoherent,
                    )
                })?,
            },
            stream,
        )))
    }
}
impl<M, A> VHostListener<M, A>
where
    for<'a> M: HasModel<Model = Model<M>>
        + DbAccessMut<CertStore>
        + DbAccessMut<AcmeCertStore>
        + DbAccessByKey<AcmeSettings, Key<'a> = &'a AcmeProvider>
        + Send
        + Sync
        + 'static,
    A: Accept + 'static,
    <A as Accept>::Metadata: Visit<ExtractVisitor<TcpMetadata>>
        + Visit<ExtractVisitor<GatewayInfo>>
        + Clone
        + Send
        + Sync
        + 'static,
{
    async fn handle_next(&mut self) -> Result<(), Error> {
        let (metadata, stream) = futures::future::poll_fn(|cx| self.poll_accept(cx)).await?;

        metadata.preprocessed.finish(stream);

        Ok(())
    }
}

struct VHostServer<A: Accept + 'static> {
    mapping: Watch<Mapping<A>>,
    _thread: NonDetachingJoinHandle<()>,
}

impl<'a> From<&'a BTreeMap<Option<InternedString>, BTreeMap<ProxyTarget, Weak<()>>>> for AnyFilter {
    fn from(value: &'a BTreeMap<Option<InternedString>, BTreeMap<ProxyTarget, Weak<()>>>) -> Self {
        Self(
            value
                .iter()
                .flat_map(|(_, v)| {
                    v.iter()
                        .filter(|(_, r)| r.strong_count() > 0)
                        .map(|(t, _)| t.filter.clone())
                })
                .collect(),
        )
    }
}

impl<A: Accept> VHostServer<A> {
    #[instrument(skip_all)]
    fn new<M: HasModel>(
        listener: A,
        db: TypedPatchDb<M>,
        crypto_provider: Arc<CryptoProvider>,
        acme_cache: AcmeTlsAlpnCache,
    ) -> Self
    where
        for<'a> M: HasModel<Model = Model<M>>
            + DbAccessMut<CertStore>
            + DbAccessMut<AcmeCertStore>
            + DbAccessByKey<AcmeSettings, Key<'a> = &'a AcmeProvider>
            + Send
            + Sync
            + 'static,
        A: Accept + Send + 'static,
        <A as Accept>::Metadata: Visit<ExtractVisitor<TcpMetadata>>
            + Visit<ExtractVisitor<GatewayInfo>>
            + Clone
            + Send
            + Sync
            + 'static,
    {
        let mapping = Watch::new(BTreeMap::new());
        Self {
            mapping: mapping.clone(),
            _thread: tokio::spawn(async move {
                let mut listener = VHostListener(TlsListener::new(
                    listener,
                    TlsHandlerWrapper {
                        inner: ChainedHandler(
                            Arc::new(AcmeTlsHandler {
                                db: db.clone(),
                                acme_cache,
                                crypto_provider: crypto_provider.clone(),
                                get_provider: GetVHostAcmeProvider(mapping.clone()),
                                in_progress: Watch::new(BTreeSet::new()),
                            }),
                            RootCaTlsHandler {
                                db,
                                crypto_provider,
                            },
                        ),
                        wrapper: VHostConnector(mapping, None),
                    },
                ));
                loop {
                    if let Err(e) = listener.handle_next().await {
                        tracing::error!("VHostServer: failed to accept connection: {e}");
                        tracing::debug!("{e:?}");
                    }
                }
            })
            .into(),
        }
    }
    fn add(
        &self,
        hostname: Option<InternedString>,
        target: DynVHostTarget<A>,
    ) -> Result<Arc<()>, Error> {
        let target = target.into();
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
        if self.mapping.watcher_count() > 1 {
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
        self.mapping.peek(|m| m.is_empty())
    }
}
