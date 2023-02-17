use std::borrow::Borrow;
use std::collections::BTreeMap;
use std::net::{Ipv4Addr, SocketAddr};
use std::sync::{Arc, Weak};
use std::time::Duration;

use color_eyre::eyre::eyre;
use futures::TryFutureExt;
use helpers::NonDetachingJoinHandle;
use models::PackageId;
use tokio::net::{TcpListener, UdpSocket};
use tokio::process::Command;
use tokio::sync::RwLock;
use tracing::instrument;
use trust_dns_server::authority::MessageResponseBuilder;
use trust_dns_server::client::op::{Header, ResponseCode};
use trust_dns_server::client::rr::{Name, Record, RecordType};
use trust_dns_server::server::{Request, RequestHandler, ResponseHandler, ResponseInfo};
use trust_dns_server::ServerFuture;

use crate::prelude::*;
use crate::util::Invoke;

pub struct DnsController {
    services: Weak<RwLock<BTreeMap<Option<PackageId>, BTreeMap<Ipv4Addr, Weak<()>>>>>,
    #[allow(dead_code)]
    dns_server: NonDetachingJoinHandle<Result<(), Error>>,
}

struct Resolver {
    services: Arc<RwLock<BTreeMap<Option<PackageId>, BTreeMap<Ipv4Addr, Weak<()>>>>>,
}
impl Resolver {
    async fn resolve(&self, name: &Name) -> Option<Vec<Ipv4Addr>> {
        match name.iter().next_back() {
            Some(b"embassy") => {
                if let Some(pkg) = name.iter().rev().skip(1).next() {
                    if let Some(ip) = self.services.read().await.get(&Some(
                        std::str::from_utf8(pkg)
                            .unwrap_or_default()
                            .parse()
                            .unwrap_or_default(),
                    )) {
                        Some(
                            ip.iter()
                                .filter(|(_, rc)| rc.strong_count() > 0)
                                .map(|(ip, _)| *ip)
                                .collect(),
                        )
                    } else {
                        None
                    }
                } else {
                    if let Some(ip) = self.services.read().await.get(&None) {
                        Some(
                            ip.iter()
                                .filter(|(_, rc)| rc.strong_count() > 0)
                                .map(|(ip, _)| *ip)
                                .collect(),
                        )
                    } else {
                        None
                    }
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
            match query.query_type() {
                RecordType::A => {
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
                }
                a => {
                    if a != RecordType::AAAA {
                        tracing::warn!(
                            "Non A-Record requested for {}: {:?}",
                            query.name(),
                            query.query_type()
                        );
                    }
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
            }
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
    #[instrument(skip_all)]
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

        Command::new("resolvectl")
            .arg("dns")
            .arg("br-start9")
            .arg("127.0.0.1")
            .invoke(ErrorKind::Network)
            .await?;
        Command::new("resolvectl")
            .arg("domain")
            .arg("br-start9")
            .arg("embassy")
            .invoke(ErrorKind::Network)
            .await?;

        let dns_server = tokio::spawn(
            server
                .block_until_done()
                .map_err(|e| Error::new(e, ErrorKind::Network)),
        )
        .into();

        Ok(Self {
            services: Arc::downgrade(&services),
            dns_server,
        })
    }

    pub async fn add(&self, pkg_id: Option<PackageId>, ip: Ipv4Addr) -> Result<Arc<()>, Error> {
        if let Some(services) = Weak::upgrade(&self.services) {
            let mut writable = services.write().await;
            let mut ips = writable.remove(&pkg_id).unwrap_or_default();
            let rc = if let Some(rc) = Weak::upgrade(&ips.remove(&ip).unwrap_or_default()) {
                rc
            } else {
                Arc::new(())
            };
            ips.insert(ip, Arc::downgrade(&rc));
            writable.insert(pkg_id, ips);
            Ok(rc)
        } else {
            Err(Error::new(
                eyre!("DNS Server Thread has exited"),
                ErrorKind::Network,
            ))
        }
    }

    pub async fn gc(&self, pkg_id: Option<PackageId>, ip: Ipv4Addr) -> Result<(), Error> {
        if let Some(services) = Weak::upgrade(&self.services) {
            let mut writable = services.write().await;
            let mut ips = writable.remove(&pkg_id).unwrap_or_default();
            if let Some(rc) = Weak::upgrade(&ips.remove(&ip).unwrap_or_default()) {
                ips.insert(ip, Arc::downgrade(&rc));
            }
            if !ips.is_empty() {
                writable.insert(pkg_id, ips);
            }
            Ok(())
        } else {
            Err(Error::new(
                eyre!("DNS Server Thread has exited"),
                ErrorKind::Network,
            ))
        }
    }
}
