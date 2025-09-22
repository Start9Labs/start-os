use std::borrow::Borrow;
use std::collections::{BTreeMap, VecDeque};
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr};
use std::str::FromStr;
use std::sync::{Arc, Weak};
use std::time::Duration;

use clap::Parser;
use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::{FutureExt, StreamExt};
use helpers::NonDetachingJoinHandle;
use hickory_client::client::Client;
use hickory_client::proto::runtime::TokioRuntimeProvider;
use hickory_client::proto::tcp::TcpClientStream;
use hickory_client::proto::udp::UdpClientStream;
use hickory_client::proto::xfer::DnsRequestOptions;
use hickory_client::proto::DnsHandle;
use hickory_server::authority::MessageResponseBuilder;
use hickory_server::proto::op::{Header, ResponseCode};
use hickory_server::proto::rr::{Name, Record, RecordType};
use hickory_server::server::{Request, RequestHandler, ResponseHandler, ResponseInfo};
use hickory_server::ServerFuture;
use imbl::OrdMap;
use imbl_value::InternedString;
use itertools::Itertools;
use models::{GatewayId, OptionExt, PackageId};
use patch_db::json_ptr::JsonPointer;
use rpc_toolkit::{
    from_fn_async, from_fn_blocking, Context, HandlerArgs, HandlerExt, ParentHandler,
};
use serde::{Deserialize, Serialize};
use tokio::net::{TcpListener, UdpSocket};
use tracing::instrument;

use crate::context::RpcContext;
use crate::db::model::public::NetworkInterfaceInfo;
use crate::db::model::Database;
use crate::net::gateway::NetworkInterfaceWatcher;
use crate::prelude::*;
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::io::file_string_stream;
use crate::util::serde::{display_serializable, HandlerExtSerde};
use crate::util::sync::{SyncRwLock, Watch};

pub fn dns_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "query",
            from_fn_blocking(query_dns::<C>)
                .with_display_serializable()
                .with_custom_display_fn(|HandlerArgs { params, .. }, res| {
                    if let Some(format) = params.format {
                        return display_serializable(format, res);
                    }

                    if let Some(ip) = res {
                        println!("{}", ip)
                    }

                    Ok(())
                })
                .with_about("Test the DNS configuration for a domain"),
        )
        .subcommand(
            "set-static",
            from_fn_async(set_static_dns)
                .no_display()
                .with_about("Set static DNS servers"),
        )
}

#[derive(Deserialize, Serialize, Parser)]
pub struct QueryDnsParams {
    pub fqdn: InternedString,
}

pub fn query_dns<C: Context>(
    _: C,
    QueryDnsParams { fqdn }: QueryDnsParams,
) -> Result<Option<Ipv4Addr>, Error> {
    let hints = dns_lookup::AddrInfoHints {
        flags: 0,
        address: libc::AF_INET,
        socktype: 0,
        protocol: 0,
    };
    dns_lookup::getaddrinfo(Some(&*fqdn), None, Some(hints))
        .map(Some)
        .or_else(|e| {
            if matches!(
                e.kind(),
                dns_lookup::LookupErrorKind::NoName | dns_lookup::LookupErrorKind::NoData
            ) {
                Ok(None)
            } else {
                Err(std::io::Error::from(e))
            }
        })
        .with_kind(ErrorKind::Network)?
        .into_iter()
        .flatten()
        .find_map(|a| match a.map(|a| a.sockaddr.ip()) {
            Ok(IpAddr::V4(a)) => Some(Ok(a)),
            Err(e) => Some(Err(e)),
            _ => None,
        })
        .transpose()
        .map_err(Error::from)
}

#[derive(Deserialize, Serialize, Parser)]
pub struct SetStaticDnsParams {
    pub servers: Option<Vec<String>>,
}

pub async fn set_static_dns(
    ctx: RpcContext,
    SetStaticDnsParams { servers }: SetStaticDnsParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_server_info_mut()
                .as_network_mut()
                .as_dns_mut()
                .as_static_servers_mut()
                .ser(
                    &servers
                        .map(|s| {
                            s.into_iter()
                                .map(|s| {
                                    s.parse::<SocketAddr>()
                                        .or_else(|_| s.parse::<IpAddr>().map(|a| (a, 53).into()))
                                })
                                .collect()
                        })
                        .transpose()?,
                )
        })
        .await
        .result
}

#[derive(Default)]
struct ResolveMap {
    private_domains: BTreeMap<InternedString, Weak<()>>,
    services: BTreeMap<Option<PackageId>, BTreeMap<Ipv4Addr, Weak<()>>>,
}

pub struct DnsController {
    resolve: Weak<SyncRwLock<ResolveMap>>,
    #[allow(dead_code)]
    dns_server: NonDetachingJoinHandle<()>,
}

struct DnsClient {
    client: Arc<SyncRwLock<Vec<(SocketAddr, hickory_client::client::Client)>>>,
    _thread: NonDetachingJoinHandle<()>,
}
impl DnsClient {
    pub fn new(db: TypedPatchDb<Database>) -> Self {
        let client = Arc::new(SyncRwLock::new(Vec::new()));
        Self {
            client: client.clone(),
            _thread: tokio::spawn(async move {
                let (bg, mut runner) = BackgroundJobQueue::new();
                runner
                    .run_while(async move {
                        let dhcp_ns_db = db.clone();
                        bg.add_job(async move {
                            loop {
                                if let Err(e) = async {
                                    let mut stream =
                                        file_string_stream("/run/systemd/resolve/resolv.conf")
                                            .filter_map(|a| futures::future::ready(a.transpose()))
                                            .boxed();
                                    while let Some(conf) = stream.next().await {
                                        let conf: String = conf?;
                                        let mut nameservers = conf
                                            .lines()
                                            .map(|l| l.trim())
                                            .filter_map(|l| l.strip_prefix("nameserver "))
                                            .map(|n| {
                                                n.parse::<SocketAddr>().or_else(|_| {
                                                    n.parse::<IpAddr>().map(|a| (a, 53).into())
                                                })
                                            })
                                            .collect::<Result<VecDeque<_>, _>>()?;
                                        if nameservers
                                            .front()
                                            .map_or(false, |addr| addr.ip().is_loopback())
                                        {
                                            nameservers.pop_front();
                                        }
                                        if nameservers.front().map_or(false, |addr| {
                                            addr.ip() == IpAddr::from([1, 1, 1, 1])
                                        }) {
                                            nameservers.pop_front();
                                        }
                                        dhcp_ns_db
                                            .mutate(|db| {
                                                let dns = db
                                                    .as_public_mut()
                                                    .as_server_info_mut()
                                                    .as_network_mut()
                                                    .as_dns_mut();
                                                dns.as_dhcp_servers_mut().ser(&nameservers)
                                            })
                                            .await
                                            .result?
                                    }
                                    Ok::<_, Error>(())
                                }
                                .await
                                {
                                    tracing::error!("{e}");
                                    tracing::debug!("{e:?}");
                                    tokio::time::sleep(Duration::from_secs(1)).await;
                                }
                            }
                        });
                        loop {
                            if let Err::<(), Error>(e) = async {
                                let mut static_changed = db
                                    .subscribe(
                                        "/public/serverInfo/network/dns/staticServers"
                                            .parse::<JsonPointer>()
                                            .with_kind(ErrorKind::Database)?,
                                    )
                                    .await;
                                let mut prev_nameservers = VecDeque::new();
                                let mut bg = BTreeMap::<SocketAddr, BoxFuture<_>>::new();
                                loop {
                                    let dns = db
                                        .peek()
                                        .await
                                        .into_public()
                                        .into_server_info()
                                        .into_network()
                                        .into_dns();
                                    let nameservers = dns
                                        .as_static_servers()
                                        .transpose_ref()
                                        .unwrap_or_else(|| dns.as_dhcp_servers())
                                        .de()?;
                                    if nameservers != prev_nameservers {
                                        let mut existing: BTreeMap<_, _> =
                                            client.peek(|c| c.iter().cloned().collect());
                                        let mut new = Vec::with_capacity(nameservers.len());
                                        for addr in &nameservers {
                                            if let Some(existing) = existing.remove(addr) {
                                                new.push((*addr, existing));
                                            } else {
                                                let client = if let Ok((client, bg_thread)) =
                                                    Client::connect(
                                                        UdpClientStream::builder(
                                                            *addr,
                                                            TokioRuntimeProvider::new(),
                                                        )
                                                        .build(),
                                                    )
                                                    .await
                                                {
                                                    bg.insert(*addr, bg_thread.boxed());
                                                    client
                                                } else {
                                                    let (stream, sender) = TcpClientStream::new(
                                                        *addr,
                                                        None,
                                                        Some(Duration::from_secs(30)),
                                                        TokioRuntimeProvider::new(),
                                                    );
                                                    let (client, bg_thread) =
                                                        Client::new(stream, sender, None)
                                                            .await
                                                            .with_kind(ErrorKind::Network)?;
                                                    bg.insert(*addr, bg_thread.boxed());
                                                    client
                                                };
                                                new.push((*addr, client));
                                            }
                                        }
                                        bg.retain(|n, _| nameservers.iter().any(|a| a == n));
                                        prev_nameservers = nameservers;
                                        client.replace(new);
                                    }
                                    futures::future::select(
                                        static_changed.recv().boxed(),
                                        futures::future::join(
                                            futures::future::join_all(bg.values_mut()),
                                            futures::future::pending::<()>(),
                                        ),
                                    )
                                    .await;
                                }
                            }
                            .await
                            {
                                tracing::error!("{e}");
                                tracing::debug!("{e:?}");
                            }
                            tokio::time::sleep(Duration::from_secs(1)).await;
                        }
                    })
                    .await;
            })
            .into(),
        }
    }
    fn lookup(
        &self,
        query: hickory_client::proto::op::Query,
        options: DnsRequestOptions,
    ) -> Vec<hickory_client::proto::xfer::DnsExchangeSend> {
        self.client.peek(|c| {
            c.iter()
                .map(|(_, c)| c.lookup(query.clone(), options.clone()))
                .collect()
        })
    }
}

lazy_static::lazy_static! {
    static ref LOCALHOST: Name = Name::from_ascii("localhost").unwrap();
    static ref STARTOS: Name = Name::from_ascii("startos").unwrap();
    static ref EMBASSY: Name = Name::from_ascii("embassy").unwrap();
}

struct Resolver {
    client: DnsClient,
    net_iface: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
    resolve: Arc<SyncRwLock<ResolveMap>>,
}
impl Resolver {
    fn resolve(&self, name: &Name, src: IpAddr) -> Option<Vec<IpAddr>> {
        if name.zone_of(&*LOCALHOST) {
            return Some(vec![Ipv4Addr::LOCALHOST.into(), Ipv6Addr::LOCALHOST.into()]);
        }
        self.resolve.peek(|r| {
            if r.private_domains
                .get(&*name.to_lowercase().to_utf8().trim_end_matches('.'))
                .map_or(false, |d| d.strong_count() > 0)
            {
                if let Some(res) = self.net_iface.peek(|i| {
                    i.values()
                        .chain([NetworkInterfaceInfo::lxc_bridge().1])
                        .flat_map(|i| i.ip_info.as_ref())
                        .find(|i| i.subnets.iter().any(|s| s.contains(&src)))
                        .map(|ip_info| {
                            let mut res = ip_info.subnets.iter().collect::<Vec<_>>();
                            res.sort_by_cached_key(|a| !a.contains(&src));
                            res.into_iter().map(|s| s.addr()).collect()
                        })
                }) {
                    return Some(res);
                }
            }
            if STARTOS.zone_of(name) || EMBASSY.zone_of(name) {
                let Ok(pkg) = name
                    .trim_to(2)
                    .iter()
                    .next()
                    .map(std::str::from_utf8)
                    .transpose()
                    .map_err(|_| ())
                    .and_then(|s| s.map(PackageId::from_str).transpose().map_err(|_| ()))
                else {
                    return None;
                };
                if let Some(ip) = r.services.get(&pkg) {
                    Some(
                        ip.iter()
                            .filter(|(_, rc)| rc.strong_count() > 0)
                            .map(|(ip, _)| (*ip).into())
                            .collect(),
                    )
                } else {
                    None
                }
            } else {
                None
            }
        })
    }
}

#[async_trait::async_trait]
impl RequestHandler for Resolver {
    async fn handle_request<R: ResponseHandler>(
        &self,
        request: &Request,
        mut response_handle: R,
    ) -> ResponseInfo {
        match async {
            let req = request.request_info()?;
            let query = req.query;
            if let Some(ip) = self.resolve(query.name().borrow(), req.src.ip()) {
                match query.query_type() {
                    RecordType::A => {
                        let mut header = Header::response_from_request(request.header());
                        header.set_recursion_available(true);
                        response_handle
                            .send_response(
                                MessageResponseBuilder::from_message_request(&*request).build(
                                    header,
                                    &ip.into_iter()
                                        .filter_map(|a| {
                                            if let IpAddr::V4(a) = a {
                                                Some(a)
                                            } else {
                                                None
                                            }
                                        })
                                        .map(|ip| {
                                            Record::from_rdata(
                                                query.name().to_owned().into(),
                                                0,
                                                hickory_server::proto::rr::RData::A(ip.into()),
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
                    RecordType::AAAA => {
                        let mut header = Header::response_from_request(request.header());
                        header.set_recursion_available(true);
                        response_handle
                            .send_response(
                                MessageResponseBuilder::from_message_request(&*request).build(
                                    header,
                                    &ip.into_iter()
                                        .filter_map(|a| {
                                            if let IpAddr::V6(a) = a {
                                                Some(a)
                                            } else {
                                                None
                                            }
                                        })
                                        .map(|ip| {
                                            Record::from_rdata(
                                                query.name().to_owned().into(),
                                                0,
                                                hickory_server::proto::rr::RData::AAAA(ip.into()),
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
                    _ => {
                        let mut header = Header::response_from_request(request.header());
                        header.set_recursion_available(true);
                        response_handle
                            .send_response(
                                MessageResponseBuilder::from_message_request(&*request).build(
                                    header.into(),
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
                let query = query.original().clone();
                let mut opt = DnsRequestOptions::default();
                opt.recursion_desired = request.recursion_desired();
                let mut streams = self.client.lookup(query, opt);
                let mut err = None;
                for stream in streams.iter_mut() {
                    match tokio::time::timeout(Duration::from_secs(5), stream.next()).await {
                        Ok(Some(Err(e))) => err = Some(e),
                        Ok(Some(Ok(msg))) => {
                            let mut header = msg.header().clone();
                            header.set_id(request.id());
                            header.set_checking_disabled(request.checking_disabled());
                            header.set_recursion_available(true);
                            return response_handle
                                .send_response(
                                    MessageResponseBuilder::from_message_request(&*request).build(
                                        header,
                                        msg.answers(),
                                        msg.name_servers(),
                                        &msg.soa().map(|s| s.to_owned().into_record_of_rdata()),
                                        msg.additionals(),
                                    ),
                                )
                                .await;
                        }
                        _ => (),
                    }
                }
                if let Some(e) = err {
                    tracing::error!("{e}");
                    tracing::debug!("{e:?}");
                }
                let mut header = Header::response_from_request(request.header());
                header.set_recursion_available(true);
                header.set_response_code(ResponseCode::ServFail);
                response_handle
                    .send_response(
                        MessageResponseBuilder::from_message_request(&*request).build(
                            header,
                            [],
                            [],
                            [],
                            [],
                        ),
                    )
                    .await
            }
        }
        .await
        {
            Ok(a) => a,
            Err(e) => {
                tracing::error!("{}", e);
                tracing::debug!("{:?}", e);
                let mut header = Header::response_from_request(request.header());
                header.set_recursion_available(true);
                header.set_response_code(ResponseCode::ServFail);
                response_handle
                    .send_response(
                        MessageResponseBuilder::from_message_request(&*request).build(
                            header,
                            [],
                            [],
                            [],
                            [],
                        ),
                    )
                    .await
                    .unwrap_or(header.into())
            }
        }
    }
}

impl DnsController {
    #[instrument(skip_all)]
    pub async fn init(
        db: TypedPatchDb<Database>,
        watcher: &NetworkInterfaceWatcher,
    ) -> Result<Self, Error> {
        let resolve = Arc::new(SyncRwLock::new(ResolveMap::default()));

        let mut server = ServerFuture::new(Resolver {
            client: DnsClient::new(db),
            net_iface: watcher.subscribe(),
            resolve: resolve.clone(),
        });

        let dns_server = tokio::spawn(
            async move {
                server.register_listener(
                    TcpListener::bind((Ipv6Addr::UNSPECIFIED, 53))
                        .await
                        .with_kind(ErrorKind::Network)?,
                    Duration::from_secs(30),
                );
                server.register_socket(
                    UdpSocket::bind((Ipv6Addr::UNSPECIFIED, 53))
                        .await
                        .with_kind(ErrorKind::Network)?,
                );

                server
                    .block_until_done()
                    .await
                    .with_kind(ErrorKind::Network)
            }
            .map(|r| {
                r.log_err();
            }),
        )
        .into();

        Ok(Self {
            resolve: Arc::downgrade(&resolve),
            dns_server,
        })
    }

    pub fn add_service(&self, pkg_id: Option<PackageId>, ip: Ipv4Addr) -> Result<Arc<()>, Error> {
        if let Some(resolve) = Weak::upgrade(&self.resolve) {
            resolve.mutate(|writable| {
                let ips = writable.services.entry(pkg_id).or_default();
                let weak = ips.entry(ip).or_default();
                let rc = if let Some(rc) = Weak::upgrade(&*weak) {
                    rc
                } else {
                    let new = Arc::new(());
                    *weak = Arc::downgrade(&new);
                    new
                };
                Ok(rc)
            })
        } else {
            Err(Error::new(
                eyre!("DNS Server Thread has exited"),
                crate::ErrorKind::Network,
            ))
        }
    }

    pub fn gc_service(&self, pkg_id: Option<PackageId>, ip: Ipv4Addr) -> Result<(), Error> {
        if let Some(resolve) = Weak::upgrade(&self.resolve) {
            resolve.mutate(|writable| {
                let mut ips = writable.services.remove(&pkg_id).unwrap_or_default();
                if let Some(rc) = Weak::upgrade(&ips.remove(&ip).unwrap_or_default()) {
                    ips.insert(ip, Arc::downgrade(&rc));
                }
                if !ips.is_empty() {
                    writable.services.insert(pkg_id, ips);
                }
                Ok(())
            })
        } else {
            Err(Error::new(
                eyre!("DNS Server Thread has exited"),
                crate::ErrorKind::Network,
            ))
        }
    }

    pub fn add_private_domain(&self, fqdn: InternedString) -> Result<Arc<()>, Error> {
        if let Some(resolve) = Weak::upgrade(&self.resolve) {
            resolve.mutate(|writable| {
                let weak = writable.private_domains.entry(fqdn).or_default();
                let rc = if let Some(rc) = Weak::upgrade(&*weak) {
                    rc
                } else {
                    let new = Arc::new(());
                    *weak = Arc::downgrade(&new);
                    new
                };
                Ok(rc)
            })
        } else {
            Err(Error::new(
                eyre!("DNS Server Thread has exited"),
                crate::ErrorKind::Network,
            ))
        }
    }

    pub fn gc_private_domains<'a, BK: Ord + 'a>(
        &self,
        domains: impl IntoIterator<Item = &'a BK> + 'a,
    ) -> Result<(), Error>
    where
        InternedString: Borrow<BK>,
    {
        if let Some(resolve) = Weak::upgrade(&self.resolve) {
            resolve.mutate(|writable| {
                for domain in domains {
                    if let Some((k, v)) = writable.private_domains.remove_entry(domain) {
                        if v.strong_count() > 0 {
                            writable.private_domains.insert(k, v);
                        }
                    }
                }
                Ok(())
            })
        } else {
            Err(Error::new(
                eyre!("DNS Server Thread has exited"),
                crate::ErrorKind::Network,
            ))
        }
    }
}
