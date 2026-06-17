use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, Ipv4Addr, SocketAddr, SocketAddrV4};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock};

use clap::Parser;
use cookie::{Cookie, Expiration, SameSite};
use http::HeaderMap;
use imbl::OrdMap;
use imbl_value::InternedString;
use include_dir::Dir;
use patch_db::PatchDb;
use patch_db::json_ptr::ROOT;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{CallRemote, Context, Empty, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::sync::broadcast::Sender;
use tracing::instrument;
use url::Url;

use crate::GatewayId;
use crate::auth::Sessions;
use crate::context::config::ContextConfig;
use crate::context::{CliContext, RpcContext};
use crate::db::model::public::{NetworkInterfaceInfo, NetworkInterfaceType};
use crate::middleware::auth::Auth;
use crate::middleware::auth::local::LocalAuthContext;
use crate::middleware::cors::Cors;
use crate::net::forward::{PortForwardController, nft_rule};
use crate::net::static_server::{EMPTY_DIR, UiContext};
use crate::prelude::*;
use crate::rpc_continuations::{OpenAuthedContinuations, RpcContinuations};
use crate::tunnel::TUNNEL_DEFAULT_LISTEN;
use crate::tunnel::api::tunnel_api;
use crate::net::rfc2136::{DnsInjector, InjectedRecord};
use crate::tunnel::db::{DnsRecordEntry, DnsRecords, PortForward, TunnelDatabase};
use crate::tunnel::dns::DnsProxyController;
use crate::tunnel::migrations::run_migrations;
use crate::tunnel::wg::{WIREGUARD_INTERFACE_NAME, WgServer};
use crate::util::collections::OrdMapIterMut;
use crate::util::io::read_file_to_string;
use crate::util::sync::{SyncMutex, Watch};

#[derive(Debug, Clone, Default, Deserialize, Serialize, Parser)]
#[group(skip)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct TunnelConfig {
    #[arg(short = 'c', long = "config", help = "help.arg.config-file-path")]
    pub config: Option<PathBuf>,
    #[arg(short = 'l', long = "listen", help = "help.arg.tunnel-listen-address")]
    pub tunnel_listen: Option<SocketAddr>,
    #[arg(short = 'd', long = "datadir", help = "help.arg.data-directory")]
    pub datadir: Option<PathBuf>,
}
impl ContextConfig for TunnelConfig {
    fn next(&mut self) -> Option<PathBuf> {
        self.config.take()
    }
    fn merge_with(&mut self, other: Self) {
        self.tunnel_listen = self.tunnel_listen.take().or(other.tunnel_listen);
        self.datadir = self.datadir.take().or(other.datadir);
    }
}

impl TunnelConfig {
    pub fn load(mut self) -> Result<Self, Error> {
        let path = self.next();
        self.load_path_rec(path)?;
        self.load_path_rec(Some("/etc/start-tunneld"))?;
        Ok(self)
    }
}

/// WireGuard client IPs whose `allow_dns_injection` toggle is on.
fn allowed_injectors(server: &WgServer) -> BTreeSet<IpAddr> {
    let mut out = BTreeSet::new();
    for (_, subnet) in &server.subnets.0 {
        for (ip, client) in &subnet.clients.0 {
            if client.allow_dns_injection {
                out.insert(IpAddr::V4(*ip));
            }
        }
    }
    out
}

/// Seed the injector from the persisted records (dropping any that no longer
/// parse).
fn seed_records(records: &DnsRecords) -> Vec<InjectedRecord> {
    records
        .0
        .iter()
        .filter_map(|e| {
            let src = e
                .source
                .as_deref()
                .and_then(|s| s.parse().ok())
                .unwrap_or(IpAddr::V4(Ipv4Addr::UNSPECIFIED));
            InjectedRecord::from_parts(&e.name, &e.rtype, &e.value, e.ttl, src).ok()
        })
        .collect()
}

fn dns_entry(r: &InjectedRecord) -> DnsRecordEntry {
    let (name, rtype, value, ttl, source) = r.to_parts();
    DnsRecordEntry {
        name,
        rtype,
        value,
        ttl,
        source: source.map(|s| s.to_string()),
    }
}

pub struct TunnelContextSeed {
    pub listen: SocketAddr,
    pub db: TypedPatchDb<TunnelDatabase>,
    pub datadir: PathBuf,
    pub rpc_continuations: RpcContinuations,
    pub open_authed_continuations: OpenAuthedContinuations<Option<InternedString>>,
    pub ephemeral_sessions: SyncMutex<Sessions>,
    pub net_iface: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
    pub forward: PortForwardController,
    pub dns_proxy: DnsProxyController,
    pub sni: Arc<crate::tunnel::sni::SniDemux>,
    pub dns_injector: Arc<DnsInjector>,
    /// WireGuard client IPs whose `allow_dns_injection` toggle is on; the DNS
    /// injector's authorizer reads this so a toggle change takes effect without
    /// rebuilding the injector.
    pub dns_allowed: Arc<SyncMutex<BTreeSet<IpAddr>>>,
    pub active_forwards: SyncMutex<BTreeMap<SocketAddrV4, Arc<()>>>,
    pub shutdown: Sender<Option<bool>>,
}

#[derive(Clone)]
pub struct TunnelContext(Arc<TunnelContextSeed>);
impl TunnelContext {
    #[instrument(skip_all)]
    pub async fn init(config: &TunnelConfig) -> Result<Self, Error> {
        Self::init_auth_cookie().await?;
        let (shutdown, _) = tokio::sync::broadcast::channel(1);
        let datadir = config
            .datadir
            .as_deref()
            .unwrap_or_else(|| Path::new("/var/lib/start-tunnel"))
            .to_owned();
        if tokio::fs::metadata(&datadir).await.is_err() {
            tokio::fs::create_dir_all(&datadir).await?;
        }
        let db_path = datadir.join("tunnel.db");
        let db = TypedPatchDb::<TunnelDatabase>::load_unchecked(PatchDb::open(&db_path).await?);
        if db.dump(&ROOT).await.value.is_null() {
            db.put(&ROOT, &TunnelDatabase::init()).await?;
        }
        db.mutate(|db| run_migrations(db)).await.result?;
        let listen = config.tunnel_listen.unwrap_or(TUNNEL_DEFAULT_LISTEN);
        let ip_info = crate::net::utils::load_ip_info().await?;
        let net_iface = db
            .mutate(|db| {
                db.as_gateways_mut().mutate(|g| {
                    for (_, v) in OrdMapIterMut::from(&mut *g) {
                        v.ip_info = None;
                    }
                    for (id, info) in ip_info {
                        if id.as_str() != WIREGUARD_INTERFACE_NAME {
                            g.entry(id).or_default().ip_info = Some(Arc::new(info));
                        }
                    }
                    Ok(g.clone())
                })
            })
            .await
            .result?;
        let net_iface = Watch::new(net_iface);
        let forward = PortForwardController::new();
        nft_rule(
            "forward",
            "wg-forward",
            false,
            false,
            &format!("iifname \"{WIREGUARD_INTERFACE_NAME}\" ct state new accept"),
        )
        .await?;
        // Clamp TCP MSS on forwarded SYNs to the WireGuard path MTU so large
        // TLS ClientHellos (desktop Firefox/Chromium with X25519MLKEM768
        // post-quantum key shares) don't get silently dropped after
        // encapsulation. See start-os#3261.
        nft_rule(
            "mangle_forward",
            "wg-mss-clamp",
            false,
            false,
            "tcp flags syn / syn,rst tcp option maxseg size set rt mtu",
        )
        .await?;

        let dns_proxy = DnsProxyController::new();
        let peek = db.peek().await;
        let wg = peek.as_wg().de()?;
        let dns_allowed = Arc::new(SyncMutex::new(allowed_injectors(&wg)));
        let dns_injector = {
            let seed = seed_records(&peek.as_dns_records().de()?);
            let allowed = dns_allowed.clone();
            let persist_db = db.clone();
            DnsInjector::new(
                seed,
                move |src| allowed.peek(|s| s.contains(&src)),
                move |records| {
                    let db = persist_db.clone();
                    let entries: Vec<_> = records.iter().map(dns_entry).collect();
                    tokio::spawn(async move {
                        db.mutate(|d| d.as_dns_records_mut().ser(&DnsRecords(entries)))
                            .await
                            .result
                            .log_err();
                    });
                },
            )
        };
        wg.sync().await?;
        dns_proxy.sync(&wg, dns_injector.clone()).await?;

        for iface in net_iface.peek(|i| {
            i.iter()
                .filter(|(_, info)| {
                    info.ip_info.as_ref().map_or(false, |i| {
                        i.device_type != Some(NetworkInterfaceType::Loopback)
                    })
                })
                .map(|(name, _)| name)
                .filter(|id| id.as_str() != WIREGUARD_INTERFACE_NAME)
                .cloned()
                .collect::<Vec<_>>()
        }) {
            for subnet in peek.as_wg().as_subnets().keys()? {
                let net = subnet.trunc();
                nft_rule(
                    "postrouting",
                    &format!("tunnel-masq-{net}-{iface}"),
                    false,
                    false,
                    &format!("ip saddr {net} oifname \"{iface}\" masquerade"),
                )
                .await?;
            }
        }

        let sni = crate::tunnel::sni::SniDemux::new();
        let mut active_forwards = BTreeMap::new();
        for (from, entry) in peek.as_port_forwards().de()?.0 {
            match entry {
                PortForward::Dnat {
                    target,
                    enabled,
                    count,
                    ..
                } => {
                    if !enabled {
                        continue;
                    }
                    let to = target;
                    let prefix = net_iface
                        .peek(|i| {
                            i.iter()
                                .find_map(|(_, i)| {
                                    i.ip_info.as_ref().and_then(|i| {
                                        i.subnets
                                            .iter()
                                            .find(|s| s.contains(&IpAddr::from(*to.ip())))
                                    })
                                })
                                .cloned()
                        })
                        .map(|s| s.prefix_len())
                        .unwrap_or(32);
                    active_forwards.insert(
                        from,
                        forward.add_forward_range(from, to, count, prefix, None).await?,
                    );
                }
                PortForward::Sni { routes } => {
                    for (hostname, route) in routes {
                        if !route.enabled {
                            continue;
                        }
                        if let Err(code) = sni.register(
                            *from.ip(),
                            from.port(),
                            &[hostname.clone()],
                            route.target,
                            route.nonce,
                            None,
                        ) {
                            tracing::warn!(
                                "failed to restore SNI route {hostname} on {from}: code {code}"
                            );
                        }
                    }
                }
            }
        }

        let ctx = Self(Arc::new(TunnelContextSeed {
            listen,
            db,
            datadir,
            rpc_continuations: RpcContinuations::new(),
            open_authed_continuations: OpenAuthedContinuations::new(),
            ephemeral_sessions: SyncMutex::new(Sessions::new()),
            net_iface,
            forward,
            dns_proxy,
            sni,
            dns_injector,
            dns_allowed,
            active_forwards: SyncMutex::new(active_forwards),
            shutdown,
        }));

        // Serve PCP (preferred) and a UPnP IGD (fallback) to connected peers
        // over the WireGuard interface so StartOS clients can open their public
        // ports automatically.
        tokio::spawn(crate::tunnel::pcp::run(ctx.clone()));
        tokio::spawn(crate::tunnel::igd::run(ctx.clone()));

        Ok(ctx)
    }

    pub async fn gc_forwards(
        &self,
        keep: &BTreeSet<SocketAddrV4>,
        dropped_sni: &[(SocketAddrV4, String, [u8; 12])],
    ) -> Result<(), Error> {
        for (source, hostname, nonce) in dropped_sni {
            self.sni.unregister(
                *source.ip(),
                source.port(),
                std::slice::from_ref(hostname),
                *nonce,
            );
        }
        self.active_forwards
            .mutate(|pf| pf.retain(|k, _| keep.contains(k)));
        self.forward.gc().await
    }

    /// Apply a WireGuard config change to the live tunnel: re-render and reload the
    /// wg interface, then rebind the per-subnet DNS proxies (which bind to the
    /// interface addresses `wg-quick up` just (re)created — so this must run after
    /// `server.sync()`).
    pub async fn sync_network(&self, server: &WgServer) -> Result<(), Error> {
        server.sync().await?;
        self.dns_allowed.mutate(|s| *s = allowed_injectors(server));
        self.dns_proxy.sync(server, self.dns_injector.clone()).await
    }
}
impl AsRef<RpcContinuations> for TunnelContext {
    fn as_ref(&self) -> &RpcContinuations {
        &self.rpc_continuations
    }
}

impl AsRef<OpenAuthedContinuations<Option<InternedString>>> for TunnelContext {
    fn as_ref(&self) -> &OpenAuthedContinuations<Option<InternedString>> {
        &self.open_authed_continuations
    }
}

impl Context for TunnelContext {}
impl Deref for TunnelContext {
    type Target = TunnelContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

#[derive(Debug, Deserialize, Serialize, Parser)]
#[group(skip)]
pub struct TunnelAddrParams {
    #[arg(help = "help.arg.tunnel-ip-address")]
    pub tunnel: IpAddr,
}

impl CallRemote<TunnelContext> for CliContext {
    async fn call_remote(
        &self,
        mut method: &str,

        _: OrdMap<&'static str, Value>,
        params: Value,
        _: Empty,
    ) -> Result<Value, RpcError> {
        let (tunnel_addr, addr_from_config) = if let Some(addr) = self.tunnel_addr {
            (addr, true)
        } else if let Some(addr) = self.tunnel_listen {
            (addr, true)
        } else {
            (TUNNEL_DEFAULT_LISTEN, false)
        };

        let local =
            if let Ok(local) = read_file_to_string(TunnelContext::LOCAL_AUTH_COOKIE_PATH).await {
                self.cookie_store
                    .lock()
                    .unwrap()
                    .insert_raw(
                        &Cookie::build(("local", local))
                            .domain(&tunnel_addr.ip().to_string())
                            .expires(Expiration::Session)
                            .same_site(SameSite::Strict)
                            .build(),
                        &format!("http://{tunnel_addr}").parse()?,
                    )
                    .with_kind(crate::ErrorKind::Network)?;
                true
            } else {
                false
            };

        let (url, sig_ctx) = if local && tunnel_addr.ip().is_loopback() {
            (format!("http://{tunnel_addr}/rpc/v0").parse()?, None)
        } else if addr_from_config {
            (
                format!("https://{tunnel_addr}/rpc/v0").parse()?,
                Some(InternedString::from_display(&tunnel_addr.ip())),
            )
        } else {
            return Err(Error::new(eyre!("`--tunnel` required"), ErrorKind::InvalidRequest).into());
        };

        method = method.strip_prefix("tunnel.").unwrap_or(method);

        crate::middleware::auth::signature::call_remote(
            self,
            url,
            HeaderMap::new(),
            sig_ctx.as_deref(),
            method,
            params,
        )
        .await
    }
}

#[derive(Debug, Deserialize, Serialize, Parser)]
#[group(skip)]
pub struct TunnelUrlParams {
    #[arg(help = "help.arg.tunnel-url")]
    pub tunnel: Url,
}

impl CallRemote<TunnelContext, TunnelUrlParams> for RpcContext {
    async fn call_remote(
        &self,
        mut method: &str,
        _: OrdMap<&'static str, Value>,
        params: Value,
        TunnelUrlParams { tunnel }: TunnelUrlParams,
    ) -> Result<Value, RpcError> {
        let url = tunnel.join("rpc/v0")?;
        method = method.strip_prefix("tunnel.").unwrap_or(method);

        let sig_ctx = url.host_str().map(InternedString::from_display);

        crate::middleware::auth::signature::call_remote(
            self,
            url,
            HeaderMap::new(),
            sig_ctx.as_deref(),
            method,
            params,
        )
        .await
    }
}

pub static TUNNEL_UI_CELL: OnceLock<Dir<'static>> = OnceLock::new();

impl UiContext for TunnelContext {
    fn ui_dir() -> &'static Dir<'static> {
        TUNNEL_UI_CELL.get().unwrap_or(&EMPTY_DIR)
    }
    fn api() -> ParentHandler<Self> {
        tracing::info!("loading tunnel api...");
        tunnel_api()
    }
    fn middleware(server: rpc_toolkit::Server<Self>) -> rpc_toolkit::HttpServer<Self> {
        server.middleware(Cors::new()).middleware(
            Auth::new()
                .with_local_auth()
                .with_signature_auth()
                .with_session_auth(),
        )
    }
}
