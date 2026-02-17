use std::borrow::Borrow;
use std::collections::BTreeMap;
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr};
use std::str::FromStr;
use std::sync::{Arc, Weak};
use std::time::Duration;

use clap::Parser;
use color_eyre::eyre::eyre;
use futures::{FutureExt, StreamExt, TryStreamExt};
use hickory_server::authority::{AuthorityObject, Catalog, MessageResponseBuilder};
use hickory_server::proto::op::{Header, ResponseCode};
use hickory_server::proto::rr::{Name, Record, RecordType};
use hickory_server::resolver::config::{ResolverConfig, ResolverOpts};
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
use tokio::net::{TcpListener, UdpSocket};
use tokio::sync::RwLock;
use tracing::instrument;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::db::model::Database;
use crate::db::model::public::NetworkInterfaceInfo;
use crate::net::gateway::NetworkInterfaceWatcher;
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
    private_domains: BTreeMap<InternedString, Weak<()>>,
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
    _thread: NonDetachingJoinHandle<()>,
}
impl Resolver {
    fn new(
        db: TypedPatchDb<Database>,
        net_iface: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
    ) -> Self {
        let catalog = Arc::new(RwLock::new(Catalog::new()));
        Self {
            catalog: catalog.clone(),
            resolve: Arc::new(SyncRwLock::new(ResolveMap::default())),
            net_iface,
            _thread: tokio::spawn(async move {
                let mut prev = crate::util::serde::hash_serializable::<sha2::Sha256, _>(&(
                    ResolverConfig::new(),
                    ResolverOpts::default(),
                ))
                .unwrap_or_default();
                loop {
                    if let Err(e) = async {
                        let mut stream = file_string_stream("/run/systemd/resolve/resolv.conf")
                            .filter_map(|a| futures::future::ready(a.transpose()))
                            .boxed();
                        while let Some(conf) = stream.try_next().await? {
                            let (config, mut opts) =
                                hickory_resolver::system_conf::parse_resolv_conf(conf)
                                    .with_kind(ErrorKind::ParseSysInfo)?;
                            opts.timeout = Duration::from_secs(30);
                            let hash = crate::util::serde::hash_serializable::<sha2::Sha256, _>(
                                &(&config, &opts),
                            )?;
                            if hash != prev {
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
                                let auth: Vec<Arc<dyn AuthorityObject>> = vec![Arc::new(
                                    ForwardAuthority::builder_tokio(ForwardConfig {
                                        name_servers: from_value(Value::Array(
                                            config
                                                .name_servers()
                                                .into_iter()
                                                .skip(4)
                                                .map(to_value)
                                                .collect::<Result<_, Error>>()?,
                                        ))?,
                                        options: Some(opts),
                                    })
                                    .build()
                                    .map_err(|e| Error::new(eyre!("{e}"), ErrorKind::Network))?,
                                )];
                                {
                                    let mut guard = tokio::time::timeout(
                                        Duration::from_secs(10),
                                        catalog.write(),
                                    )
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
                            }
                            prev = hash;
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
            })
            .into(),
        }
    }
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
            if !src.is_loopback()
                && r.private_domains
                    .get(&*name.to_lowercase().to_utf8().trim_end_matches('.'))
                    .map_or(false, |d| d.strong_count() > 0)
            {
                if let Some(res) = self.net_iface.peek(|i| {
                    i.values()
                        .filter_map(|i| i.ip_info.as_ref())
                        .find(|i| i.subnets.iter().any(|s| s.contains(&src)))
                        .map(|ip_info| {
                            let mut res = ip_info.subnets.iter().collect::<Vec<_>>();
                            res.sort_by_cached_key(|a| !a.contains(&src));
                            res.into_iter().map(|s| s.addr()).collect()
                        })
                }) {
                    return Some(res);
                } else {
                    tracing::warn!(
                        "{}",
                        t!(
                            "net.dns.could-not-determine-source-interface",
                            src = src.to_string()
                        )
                    );
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

impl DnsController {
    #[instrument(skip_all)]
    pub async fn init(
        db: TypedPatchDb<Database>,
        watcher: &NetworkInterfaceWatcher,
    ) -> Result<Self, Error> {
        let resolver = Resolver::new(db, watcher.subscribe());
        let resolve = Arc::downgrade(&resolver.resolve);
        let mut server = ServerFuture::new(resolver);

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
            resolve,
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
                        if v.strong_count() > 0 {
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
