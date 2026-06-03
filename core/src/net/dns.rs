use std::borrow::Borrow;
use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr, SocketAddrV6};
use std::str::FromStr;
use std::sync::{Arc, Weak};
use std::time::Duration;

use clap::Parser;
use color_eyre::eyre::eyre;
use futures::{FutureExt, StreamExt, TryStreamExt};
use hickory_server::authority::{AuthorityObject, Catalog, MessageResponseBuilder};
use hickory_server::proto::op::{Header, ResponseCode};
use hickory_server::proto::rr::{Name, Record, RecordType};
use hickory_server::proto::xfer::Protocol;
use hickory_server::resolver::config::{NameServerConfig, ResolverConfig, ResolverOpts};
use hickory_server::server::{Request, RequestHandler, ResponseHandler, ResponseInfo};
use hickory_server::store::forwarder::{ForwardAuthority, ForwardConfig};
use hickory_server::{ServerFuture, resolver as hickory_resolver};
use imbl::OrdMap;
use imbl_value::InternedString;
use itertools::Itertools;
use rpc_toolkit::{
    Context, HandlerArgs, HandlerExt, ParentHandler, from_fn_async, from_fn_blocking,
};
use serde::{Deserialize, Serialize};
use tokio::net::UdpSocket;
use tokio::sync::RwLock;
use tracing::instrument;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::db::model::Database;
use crate::db::model::public::NetworkInterfaceInfo;
use crate::net::gateway::NetworkInterfaceWatcher;
use crate::net::utils::{bind_tokio_listener, ipv6_is_link_local};
use crate::prelude::*;
use crate::util::future::NonDetachingJoinHandle;
use crate::util::io::file_string_stream;
use crate::util::serde::{HandlerExtSerde, display_serializable};
use crate::util::sync::{SyncRwLock, Watch};
use crate::{GatewayId, OptionExt, PackageId};

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
                .with_about("about.test-dns-configuration-for-domain"),
        )
        .subcommand(
            "set-static",
            from_fn_async(set_static_dns)
                .no_display()
                .with_about("about.set-static-dns-servers")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "dump-table",
            from_fn_async(dump_table)
                .with_display_serializable()
                .with_custom_display_fn(|HandlerArgs { params, .. }, res| {
                    use prettytable::*;

                    if let Some(format) = params.format {
                        return display_serializable(format, res);
                    }

                    let mut table = Table::new();
                    table.add_row(row![bc => "FQDN", "DESTINATION"]);
                    for (hostname, destination) in res {
                        if let Some(ip) = destination {
                            table.add_row(row![hostname, ip]);
                        } else {
                            table.add_row(row![hostname, "SELF"]);
                        }
                    }

                    table.print_tty(false)?;

                    Ok(())
                })
                .with_about("about.dump-address-resolution-table")
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[ts(export)]
pub struct QueryDnsParams {
    #[arg(help = "help.arg.fqdn")]
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

#[derive(Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[ts(export)]
pub struct SetStaticDnsParams {
    #[arg(help = "help.arg.dns-servers")]
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

pub async fn dump_table(
    ctx: RpcContext,
) -> Result<BTreeMap<InternedString, Option<IpAddr>>, Error> {
    Ok(ctx
        .net_controller
        .dns
        .resolve
        .upgrade()
        .or_not_found("DnsController")?
        .peek(|map| {
            map.private_domains
                .iter()
                .map(|(d, _)| (d.clone(), None))
                .chain(map.services.iter().filter_map(|(svc, ip)| {
                    ip.iter()
                        .find(|(_, rc)| rc.strong_count() > 0)
                        .map(|(ip, _)| {
                            (
                                svc.as_ref().map_or(
                                    InternedString::from_static("startos"),
                                    |svc| {
                                        InternedString::from_display(&lazy_format!("{svc}.startos"))
                                    },
                                ),
                                Some(IpAddr::V4(*ip)),
                            )
                        })
                }))
                .collect()
        }))
}

#[derive(Default)]
struct ResolveMap {
    private_domains: BTreeMap<InternedString, (BTreeSet<GatewayId>, Weak<()>)>,
    services: BTreeMap<Option<PackageId>, BTreeMap<Ipv4Addr, Weak<()>>>,
    challenges: BTreeMap<InternedString, (InternedString, Weak<()>)>,
}

pub struct DnsController {
    resolve: Weak<SyncRwLock<ResolveMap>>,
    #[allow(dead_code)]
    dns_server: NonDetachingJoinHandle<()>,
}

lazy_static::lazy_static! {
    static ref LOCALHOST: Name = Name::from_ascii("localhost").unwrap();
    static ref STARTOS: Name = Name::from_ascii("startos").unwrap();
    static ref EMBASSY: Name = Name::from_ascii("embassy").unwrap();
}

struct Resolver {
    catalog: Arc<RwLock<Catalog>>,
    resolve: Arc<SyncRwLock<ResolveMap>>,
    net_iface: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
    /// The gateway whose socket this resolver serves; `None` for the wildcard
    /// catch-all. Private domains are only answered when their gateway set
    /// contains this gateway, so they never leak across interfaces.
    my_gateway: Option<GatewayId>,
}

/// Keep the forwarder authority in `catalog` in sync with resolv.conf and the
/// user's static upstream servers.
fn spawn_forwarder(
    db: TypedPatchDb<Database>,
    catalog: Arc<RwLock<Catalog>>,
) -> NonDetachingJoinHandle<()> {
    tokio::spawn(async move {
                let mut prev = crate::util::serde::hash_serializable::<sha2::Sha256, _>(&(
                    ResolverConfig::new(),
                    ResolverOpts::default(),
                    Option::<std::collections::VecDeque<SocketAddr>>::None,
                ))
                .unwrap_or_default();
                loop {
                    let res: Result<(), Error> = async {
                        let mut file_stream =
                            file_string_stream("/run/systemd/resolve/resolv.conf")
                                .filter_map(|a| futures::future::ready(a.transpose()))
                                .boxed();
                        let mut static_sub = db
                            .subscribe(
                                "/public/serverInfo/network/dns/staticServers"
                                    .parse()
                                    .unwrap(),
                            )
                            .await;
                        let mut last_config: Option<(ResolverConfig, ResolverOpts)> = None;
                        loop {
                            let got_file = tokio::select! {
                                res = file_stream.try_next() => {
                                    let conf = res?
                                        .ok_or_else(|| Error::new(
                                            eyre!("resolv.conf stream ended"),
                                            ErrorKind::Network,
                                        ))?;
                                    let (config, mut opts) =
                                        hickory_resolver::system_conf::parse_resolv_conf(conf)
                                            .with_kind(ErrorKind::ParseSysInfo)?;
                                    opts.timeout = Duration::from_secs(30);
                                    last_config = Some((config, opts));
                                    true
                                }
                                _ = static_sub.recv() => false,
                            };
                            let Some((ref config, ref opts)) = last_config else {
                                continue;
                            };
                            let static_servers: Option<std::collections::VecDeque<SocketAddr>> = db
                                .peek()
                                .await
                                .as_public()
                                .as_server_info()
                                .as_network()
                                .as_dns()
                                .as_static_servers()
                                .de()?;
                            let hash = crate::util::serde::hash_serializable::<sha2::Sha256, _>(
                                &(config, opts, &static_servers),
                            )?;
                            if hash == prev {
                                prev = hash;
                                continue;
                            }
                            if got_file {
                                db.mutate(|db| {
                                    db.as_public_mut()
                                        .as_server_info_mut()
                                        .as_network_mut()
                                        .as_dns_mut()
                                        .as_dhcp_servers_mut()
                                        .ser(
                                            &config
                                                .name_servers()
                                                .into_iter()
                                                .map(|n| n.socket_addr)
                                                .dedup()
                                                .skip(2)
                                                .collect(),
                                        )
                                })
                                .await
                                .result?;
                            }
                            let forward_servers = if let Some(servers) = &static_servers {
                                servers
                                    .iter()
                                    .flat_map(|addr| {
                                        [
                                            NameServerConfig::new(*addr, Protocol::Udp),
                                            NameServerConfig::new(*addr, Protocol::Tcp),
                                        ]
                                    })
                                    .map(|n| to_value(&n))
                                    .collect::<Result<_, Error>>()?
                            } else {
                                config
                                    .name_servers()
                                    .into_iter()
                                    .skip(4)
                                    .map(to_value)
                                    .collect::<Result<_, Error>>()?
                            };
                            let auth: Vec<Arc<dyn AuthorityObject>> = vec![Arc::new(
                                ForwardAuthority::builder_tokio(ForwardConfig {
                                    name_servers: from_value(Value::Array(forward_servers))?,
                                    options: Some(opts.clone()),
                                })
                                .build()
                                .map_err(|e| Error::new(eyre!("{e}"), ErrorKind::Network))?,
                            )];
                            {
                                let mut guard =
                                    tokio::time::timeout(Duration::from_secs(10), catalog.write())
                                        .await
                                        .map_err(|_| {
                                            Error::new(
                                                eyre!("{}", t!("net.dns.timeout-updating-catalog")),
                                                ErrorKind::Timeout,
                                            )
                                        })?;
                                guard.upsert(Name::root().into(), auth);
                                drop(guard);
                            }
                            prev = hash;
                        }
                    }
                    .await;
                    if let Err(e) = res {
                        tracing::error!("{e}");
                        tracing::debug!("{e:?}");
                        tokio::time::sleep(Duration::from_secs(1)).await;
                    }
                }
            })
        .into()
}
impl Resolver {
    fn resolve(&self, name: &Name, mut src: IpAddr) -> Option<Vec<IpAddr>> {
        if name.zone_of(&*LOCALHOST) {
            return Some(vec![Ipv4Addr::LOCALHOST.into(), Ipv6Addr::LOCALHOST.into()]);
        }
        src = match src {
            IpAddr::V6(v6) => {
                if let Some(v4) = v6.to_ipv4_mapped() {
                    IpAddr::V4(v4)
                } else {
                    IpAddr::V6(v6)
                }
            }
            a => a,
        };
        self.resolve.peek(|r| {
            if let Some(my_gateway) = self.my_gateway.as_ref().filter(|_| !src.is_loopback()) {
                let serves_here = r
                    .private_domains
                    .get(&*name.to_lowercase().to_utf8().trim_end_matches('.'))
                    .filter(|(_, rc)| rc.strong_count() > 0)
                    .map_or(false, |(gateways, _)| gateways.contains(my_gateway));
                if serves_here {
                    if let Some(res) = self.net_iface.peek(|i| {
                        i.get(my_gateway)
                            .and_then(|info| info.ip_info.as_ref())
                            .map(|ip_info| {
                                let mut res = ip_info.subnets.iter().collect::<Vec<_>>();
                                res.sort_by_cached_key(|a| !a.contains(&src));
                                res.into_iter().map(|s| s.addr()).collect()
                            })
                    }) {
                        return Some(res);
                    }
                }
            }
            if STARTOS.zone_of(name) || EMBASSY.zone_of(name) {
                let Ok(pkg) = name
                    .iter()
                    .rev()
                    .skip(1)
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
            let name = query.name();

            if STARTOS.zone_of(name) && query.query_type() == RecordType::TXT {
                let name_str =
                    InternedString::intern(name.to_lowercase().to_utf8().trim_end_matches('.'));
                if let Some(txt_value) = self.resolve.mutate(|r| {
                    r.challenges.retain(|_, (_, weak)| weak.strong_count() > 0);
                    r.challenges.remove(&name_str).map(|(val, _)| val)
                }) {
                    let mut header = Header::response_from_request(request.header());
                    header.set_recursion_available(true);
                    return response_handle
                        .send_response(
                            MessageResponseBuilder::from_message_request(&*request).build(
                                header,
                                &[Record::from_rdata(
                                    query.name().to_owned().into(),
                                    0,
                                    hickory_server::proto::rr::RData::TXT(
                                        hickory_server::proto::rr::rdata::TXT::new(vec![
                                            txt_value.to_string(),
                                        ]),
                                    ),
                                )],
                                [],
                                [],
                                [],
                            ),
                        )
                        .await
                        .map(Some);
                }
            }

            if let Some(ip) = self.resolve(name, req.src.ip()) {
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
                                            if let IpAddr::V4(a) = a { Some(a) } else { None }
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
                            .map(Some)
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
                                            if let IpAddr::V6(a) = a { Some(a) } else { None }
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
                            .map(Some)
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
                            .map(Some)
                    }
                }
            } else {
                Ok(None)
            }
        }
        .await
        {
            Ok(Some(a)) => return a,
            Ok(None) => (),
            Err(e) => {
                tracing::error!(
                    "{}",
                    t!("net.dns.error-resolving-internal", error = e.to_string())
                );
                tracing::debug!("{e:?}");
                let mut header = Header::response_from_request(request.header());
                header.set_recursion_available(true);
                header.set_response_code(ResponseCode::ServFail);
                return response_handle
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
                    .unwrap_or_else(|_| header.into());
            }
        }
        self.catalog
            .read()
            .await
            .handle_request(request, response_handle)
            .await
    }
}

fn bind_reuse_udp(addr: SocketAddr, dual_stack: bool) -> Result<UdpSocket, Error> {
    let domain = match addr {
        SocketAddr::V4(_) => socket2::Domain::IPV4,
        SocketAddr::V6(_) => socket2::Domain::IPV6,
    };
    let socket = socket2::Socket::new(domain, socket2::Type::DGRAM, Some(socket2::Protocol::UDP))
        .with_kind(ErrorKind::Network)?;
    socket
        .set_reuse_address(true)
        .with_kind(ErrorKind::Network)?;
    if matches!(addr, SocketAddr::V6(_)) {
        socket
            .set_only_v6(!dual_stack)
            .with_kind(ErrorKind::Network)?;
    }
    socket.set_nonblocking(true).with_kind(ErrorKind::Network)?;
    socket.bind(&addr.into()).with_kind(ErrorKind::Network)?;
    UdpSocket::from_std(socket.into()).with_kind(ErrorKind::Network)
}

fn dns_server_on(resolver: Resolver, addr: SocketAddr) -> Result<ServerFuture<Resolver>, Error> {
    let dual_stack = matches!(addr, SocketAddr::V6(v6) if v6.ip().is_unspecified());
    let mut server = ServerFuture::new(resolver);
    server.register_listener(
        bind_tokio_listener(addr).with_kind(ErrorKind::Network)?,
        Duration::from_secs(30),
    );
    server.register_socket(bind_reuse_udp(addr, dual_stack)?);
    Ok(server)
}

/// Serve DNS on a wildcard catch-all socket plus a socket bound to each gateway
/// interface address. The wildcard handles loopback, the container bridge, and
/// `*.startos`/forwarding; the per-gateway sockets additionally answer private
/// domains, and because each binds a specific address the kernel routes a query
/// to the socket for the interface it ingressed on — so a private domain is only
/// served on the gateways it's configured for, regardless of source IP.
async fn run_dns_servers(
    db: TypedPatchDb<Database>,
    catalog: Arc<RwLock<Catalog>>,
    resolve: Arc<SyncRwLock<ResolveMap>>,
    mut net_iface: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
) -> Result<(), Error> {
    let _forwarder = spawn_forwarder(db, catalog.clone());
    let resolver_iface = net_iface.clone();
    let resolver = |my_gateway: Option<GatewayId>| Resolver {
        catalog: catalog.clone(),
        resolve: resolve.clone(),
        net_iface: resolver_iface.clone(),
        my_gateway,
    };

    let _wildcard = dns_server_on(
        resolver(None),
        SocketAddr::new(IpAddr::V6(Ipv6Addr::UNSPECIFIED), 53),
    )?;

    let mut per_gateway: BTreeMap<SocketAddr, ServerFuture<Resolver>> = BTreeMap::new();
    loop {
        let desired: BTreeMap<SocketAddr, GatewayId> = net_iface.peek(|ifaces| {
            ifaces
                .iter()
                .filter_map(|(gw, info)| info.ip_info.as_ref().map(|ip_info| (gw, ip_info)))
                .flat_map(|(gw, ip_info)| {
                    ip_info.subnets.iter().map(move |subnet| {
                        let addr = match subnet.addr() {
                            IpAddr::V6(v6) if ipv6_is_link_local(v6) => {
                                SocketAddr::V6(SocketAddrV6::new(v6, 53, 0, ip_info.scope_id))
                            }
                            ip => SocketAddr::new(ip, 53),
                        };
                        (addr, gw.clone())
                    })
                })
                .collect()
        });

        per_gateway.retain(|addr, _| desired.contains_key(addr));
        for (addr, gw) in desired {
            if !per_gateway.contains_key(&addr) {
                match dns_server_on(resolver(Some(gw)), addr) {
                    Ok(server) => {
                        per_gateway.insert(addr, server);
                    }
                    Err(e) => {
                        tracing::error!("failed to bind DNS on {addr}: {e}");
                        tracing::debug!("{e:?}");
                    }
                }
            }
        }

        net_iface.changed().await;
    }
}

impl DnsController {
    #[instrument(skip_all)]
    pub async fn init(
        db: TypedPatchDb<Database>,
        watcher: &NetworkInterfaceWatcher,
    ) -> Result<Self, Error> {
        let resolve = Arc::new(SyncRwLock::new(ResolveMap::default()));
        let catalog = Arc::new(RwLock::new(Catalog::new()));
        let weak = Arc::downgrade(&resolve);
        let net_iface = watcher.subscribe();

        let dns_server = tokio::spawn(run_dns_servers(db, catalog, resolve, net_iface).map(|r| {
            r.log_err();
        }))
        .into();

        Ok(Self {
            resolve: weak,
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
                eyre!("{}", t!("net.dns.server-thread-exited")),
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
                eyre!("{}", t!("net.dns.server-thread-exited")),
                crate::ErrorKind::Network,
            ))
        }
    }

    pub fn add_private_domain(
        &self,
        fqdn: InternedString,
        gateways: BTreeSet<GatewayId>,
    ) -> Result<Arc<()>, Error> {
        if let Some(resolve) = Weak::upgrade(&self.resolve) {
            resolve.mutate(|writable| {
                let entry = writable.private_domains.entry(fqdn).or_default();
                entry.0 = gateways;
                let rc = if let Some(rc) = Weak::upgrade(&entry.1) {
                    rc
                } else {
                    let new = Arc::new(());
                    entry.1 = Arc::downgrade(&new);
                    new
                };
                Ok(rc)
            })
        } else {
            Err(Error::new(
                eyre!("{}", t!("net.dns.server-thread-exited")),
                crate::ErrorKind::Network,
            ))
        }
    }

    pub fn add_challenge(
        &self,
        domain: InternedString,
        value: InternedString,
    ) -> Result<Arc<()>, Error> {
        if let Some(resolve) = Weak::upgrade(&self.resolve) {
            resolve.mutate(|writable| {
                let entry = writable
                    .challenges
                    .entry(domain)
                    .or_insert_with(|| (value.clone(), Weak::new()));
                let rc = if let Some(rc) = Weak::upgrade(&entry.1) {
                    rc
                } else {
                    let new = Arc::new(());
                    *entry = (value, Arc::downgrade(&new));
                    new
                };
                Ok(rc)
            })
        } else {
            Err(Error::new(
                eyre!("{}", t!("net.dns.server-thread-exited")),
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
                        if v.1.strong_count() > 0 {
                            writable.private_domains.insert(k, v);
                        }
                    }
                }
                Ok(())
            })
        } else {
            Err(Error::new(
                eyre!("{}", t!("net.dns.server-thread-exited")),
                crate::ErrorKind::Network,
            ))
        }
    }
}
