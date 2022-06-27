use std::borrow::Borrow;
use std::collections::{BTreeMap, BTreeSet};
use std::net::{Ipv4Addr, SocketAddr};
use std::sync::Arc;
use std::time::Duration;

use futures::TryFutureExt;
use helpers::NonDetachingJoinHandle;
use models::PackageId;
use tokio::net::{TcpListener, UdpSocket};
use tokio::sync::RwLock;
use trust_dns_server::authority::MessageResponseBuilder;
use trust_dns_server::client::op::{Header, ResponseCode};
use trust_dns_server::client::rr::{Name, Record, RecordType};
use trust_dns_server::server::{Request, RequestHandler, ResponseHandler, ResponseInfo};
use trust_dns_server::ServerFuture;

use crate::net::mdns::resolve_mdns;
use crate::{Error, ErrorKind, ResultExt};

pub struct DnsController {
    services: Arc<RwLock<BTreeMap<PackageId, BTreeSet<Ipv4Addr>>>>,
    #[allow(dead_code)]
    dns_server: NonDetachingJoinHandle<Result<(), Error>>,
}

struct Resolver {
    services: Arc<RwLock<BTreeMap<PackageId, BTreeSet<Ipv4Addr>>>>,
}
impl Resolver {
    async fn resolve(&self, name: &Name) -> Option<Vec<Ipv4Addr>> {
        match name.iter().next_back() {
            Some(b"local") => match resolve_mdns(&format!(
                "{}.local",
                name.iter()
                    .rev()
                    .skip(1)
                    .next()
                    .and_then(|v| std::str::from_utf8(v).ok())
                    .unwrap_or_default()
            ))
            .await
            {
                Ok(ip) => Some(vec![ip]),
                Err(e) => {
                    tracing::error!("{}", e);
                    tracing::debug!("{:?}", e);
                    None
                }
            },
            Some(b"embassy") => {
                if let Some(pkg) = name.iter().rev().skip(1).next() {
                    if let Some(ip) = self
                        .services
                        .read()
                        .await
                        .get(std::str::from_utf8(pkg).unwrap_or_default())
                    {
                        Some(ip.iter().copied().collect())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

#[async_trait::async_trait]
impl RequestHandler for Resolver {
    async fn handle_request<R: ResponseHandler>(
        &self,
        request: &Request,
        mut response_handle: R,
    ) -> ResponseInfo {
        let query = request.request_info().query;
        if let Some(ip) = self.resolve(query.name().borrow()).await {
            if query.query_type() != RecordType::A {
                tracing::warn!("Non A-Record requested for {}", query.name());
            }
            response_handle
                .send_response(
                    MessageResponseBuilder::from_message_request(&*request).build(
                        Header::response_from_request(request.header()),
                        &ip.into_iter()
                            .map(|ip| {
                                Record::from_rdata(
                                    request.request_info().query.name().to_owned().into(),
                                    0,
                                    trust_dns_server::client::rr::RData::A(ip),
                                )
                            })
                            .collect::<Vec<_>>(),
                        [],
                        [],
                        [],
                    ),
                )
                .await
        } else {
            let mut res = Header::response_from_request(request.header());
            res.set_response_code(ResponseCode::NXDomain);
            response_handle
                .send_response(
                    MessageResponseBuilder::from_message_request(&*request).build(
                        res.into(),
                        [],
                        [],
                        [],
                        [],
                    ),
                )
                .await
        }
        .unwrap_or_else(|e| {
            tracing::error!("{}", e);
            tracing::debug!("{:?}", e);
            let mut res = Header::response_from_request(request.header());
            res.set_response_code(ResponseCode::ServFail);
            res.into()
        })
    }
}

impl DnsController {
    pub async fn init(bind: &[SocketAddr]) -> Result<Self, Error> {
        let services = Arc::new(RwLock::new(BTreeMap::new()));

        let mut server = ServerFuture::new(Resolver {
            services: services.clone(),
        });
        server.register_listener(
            TcpListener::bind(bind)
                .await
                .with_kind(ErrorKind::Network)?,
            Duration::from_secs(30),
        );
        server.register_socket(UdpSocket::bind(bind).await.with_kind(ErrorKind::Network)?);

        let dns_server = tokio::spawn(
            server
                .block_until_done()
                .map_err(|e| Error::new(e, ErrorKind::Network)),
        )
        .into();

        Ok(Self {
            services,
            dns_server,
        })
    }

    pub async fn add(&self, pkg_id: &PackageId, ip: Ipv4Addr) {
        let mut writable = self.services.write().await;
        let mut ips = writable.remove(pkg_id).unwrap_or_default();
        ips.insert(ip);
        writable.insert(pkg_id.clone(), ips);
    }

    pub async fn remove(&self, pkg_id: &PackageId, ip: Ipv4Addr) {
        let mut writable = self.services.write().await;
        let mut ips = writable.remove(pkg_id).unwrap_or_default();
        ips.remove(&ip);
        if !ips.is_empty() {
            writable.insert(pkg_id.clone(), ips);
        }
    }
}
