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
use crate::net::forward::{PortForwardController, nft_comments_with_prefix, nft_rule};
use crate::net::static_server::{EMPTY_DIR, UiContext};
use crate::prelude::*;
use crate::rpc_continuations::{OpenAuthedContinuations, RpcContinuations};
use crate::tunnel::TUNNEL_DEFAULT_LISTEN;
use crate::tunnel::api::tunnel_api;
use crate::net::dns_update::rfc2136::{DnsInjector, InjectedRecord};
use crate::tunnel::db::{DnsRecordEntry, DnsRecords, PortForward, PortForwards, TunnelDatabase};
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
    pub sni: Arc<crate::tunnel::forward::sni::SniDemux>,
    pub dns_injector: Arc<DnsInjector>,
    /// WireGuard client IPs whose `allow_dns_injection` toggle is on; the DNS
    /// injector's authorizer reads this so a toggle change takes effect without
    /// rebuilding the injector.
    pub dns_allowed: Arc<SyncMutex<BTreeSet<IpAddr>>>,
    pub active_forwards: SyncMutex<BTreeMap<SocketAddrV4, Arc<()>>>,
    /// Serializes `resync_egress` so a concurrent reconcile can't prune a
    /// device SNAT rule another call just installed (read-DB → install → prune
    /// is not otherwise atomic).
    pub egress_lock: tokio::sync::Mutex<()>,
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

        let sni = crate::tunnel::forward::sni::SniDemux::new();
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
            egress_lock: tokio::sync::Mutex::new(()),
            shutdown,
        }));

        ctx.resync_egress().await?;

        // Serve PCP (preferred) and a UPnP IGD (fallback) to connected peers
        // over the WireGuard interface so StartOS clients can open their public
        // ports automatically.
        tokio::spawn(crate::tunnel::forward::pcp::run(ctx.clone()));
        tokio::spawn(crate::tunnel::forward::igd::run(ctx.clone()));

        Ok(ctx)
    }

    pub async fn gc_forwards(
        &self,
        keep: &BTreeSet<SocketAddrV4>,
        dropped_sni: &[(SocketAddrV4, String, SocketAddrV4)],
    ) -> Result<(), Error> {
        for (source, hostname, target) in dropped_sni {
            self.sni.unregister(
                *source.ip(),
                source.port(),
                std::slice::from_ref(hostname),
                *target,
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
        self.dns_proxy.sync(server, self.dns_injector.clone()).await?;
        self.resync_egress().await
    }

    /// Reconcile the per-subnet (and per-device override) egress NAT rules in
    /// `postrouting`. A subnet with no assigned `wan_ip` keeps plain
    /// `masquerade`; one with a `wan_ip` is SNATed to that address. A device with
    /// its own `wan_ip` gets a higher-priority `/32` rule that wins over its
    /// subnet's rule. Comment tags are stable per (subnet/device, iface) so each
    /// re-run replaces in place — a subnet reverting `wan_ip` to `None` swaps its
    /// snat rule back to masquerade rather than leaving a stale duplicate.
    pub async fn resync_egress(&self) -> Result<(), Error> {
        let _guard = self.egress_lock.lock().await;
        let ifaces: Vec<GatewayId> = self.net_iface.peek(|i| {
            i.iter()
                .filter(|(_, info)| {
                    info.ip_info.as_ref().map_or(false, |i| {
                        i.device_type != Some(NetworkInterfaceType::Loopback)
                    })
                })
                .map(|(name, _)| name)
                .filter(|id| id.as_str() != WIREGUARD_INTERFACE_NAME)
                .cloned()
                .collect()
        });
        let subnets = self.db.peek().await.as_wg().as_subnets().de()?;
        let mut want_dev: BTreeSet<String> = BTreeSet::new();
        for iface in &ifaces {
            for (subnet, cfg) in &subnets.0 {
                let net = subnet.trunc();
                let subnet_rule = match cfg.wan_ip {
                    Some(wan) => format!("ip saddr {net} oifname \"{iface}\" snat to {wan}"),
                    None => format!("ip saddr {net} oifname \"{iface}\" masquerade"),
                };
                nft_rule(
                    "postrouting",
                    &format!("tunnel-masq-{net}-{iface}"),
                    false,
                    false,
                    &subnet_rule,
                )
                .await?;

                for (client_ip, client) in &cfg.clients.0 {
                    if let Some(wan) = client.wan_ip {
                        let comment = format!("tunnel-snat-dev-{client_ip}-{iface}");
                        nft_rule(
                            "postrouting",
                            &comment,
                            false,
                            true,
                            &format!("ip saddr {client_ip}/32 oifname \"{iface}\" snat to {wan}"),
                        )
                        .await?;
                        want_dev.insert(comment);
                    }
                }
            }
        }
        // Prune device-override rules whose device was removed or had its
        // `wan_ip` cleared since the last reconcile.
        for comment in nft_comments_with_prefix("postrouting", "tunnel-snat-dev-").await {
            if !want_dev.contains(&comment) {
                nft_rule("postrouting", &comment, true, false, "").await?;
            }
        }
        Ok(())
    }

    /// Move existing forwards to follow their target device's WAN. A forward's
    /// external IP equals its target's WAN, so when a device or subnet's
    /// assigned WAN changes the forward must be re-keyed from the old external
    /// IP to the new one. Idempotent: a key that didn't change matches in the
    /// per-key diffs below and is left untouched.
    pub async fn resync_forward_keys(&self) -> Result<(), Error> {
        let old = self.db.peek().await.as_port_forwards().de()?.0;

        let mut want: BTreeMap<SocketAddrV4, PortForward> = BTreeMap::new();
        for (src, entry) in &old {
            match entry {
                PortForward::Dnat {
                    target,
                    label,
                    enabled,
                    count,
                } => {
                    let ip = crate::tunnel::forward::igd::external_ipv4(self, *target.ip())
                        .await
                        .unwrap_or(*src.ip());
                    want.insert(
                        SocketAddrV4::new(ip, src.port()),
                        PortForward::Dnat {
                            target: *target,
                            label: label.clone(),
                            enabled: *enabled,
                            count: *count,
                        },
                    );
                }
                PortForward::Sni { routes } => {
                    for (host, route) in routes {
                        let ip = crate::tunnel::forward::igd::external_ipv4(self, *route.target.ip())
                            .await
                            .unwrap_or(*src.ip());
                        let key = SocketAddrV4::new(ip, src.port());
                        match want
                            .entry(key)
                            .or_insert_with(|| PortForward::Sni {
                                routes: BTreeMap::new(),
                            }) {
                            PortForward::Sni { routes } => {
                                routes.insert(host.clone(), route.clone());
                            }
                            PortForward::Dnat { .. } => {
                                tracing::warn!(
                                    "dropping SNI route {host} on {key}: external IP now collides with a DNAT forward"
                                );
                            }
                        }
                    }
                }
            }
        }

        let sni_routes = |map: &BTreeMap<SocketAddrV4, PortForward>| {
            let mut out: BTreeMap<(SocketAddrV4, String), SocketAddrV4> = BTreeMap::new();
            for (src, entry) in map {
                if let PortForward::Sni { routes } = entry {
                    for (host, route) in routes {
                        out.insert((*src, host.clone()), route.target);
                    }
                }
            }
            out
        };
        let old_sni = sni_routes(&old);
        let new_sni = sni_routes(&want);
        for ((src, host), target) in &old_sni {
            if new_sni.get(&(*src, host.clone())) != Some(target) {
                self.sni
                    .unregister(*src.ip(), src.port(), std::slice::from_ref(host), *target);
            }
        }
        for ((src, host), target) in &new_sni {
            if old_sni.get(&(*src, host.clone())) != Some(target) {
                if let Err(code) =
                    self.sni
                        .register(*src.ip(), src.port(), std::slice::from_ref(host), *target, None)
                {
                    tracing::warn!("failed to register SNI route {host} on {src}: code {code}");
                }
            }
        }

        let old_dnat: BTreeSet<SocketAddrV4> = old
            .iter()
            .filter_map(|(src, e)| matches!(e, PortForward::Dnat { .. }).then_some(*src))
            .collect();
        let new_dnat: BTreeMap<SocketAddrV4, (SocketAddrV4, u16, bool)> = want
            .iter()
            .filter_map(|(src, e)| match e {
                PortForward::Dnat {
                    target,
                    enabled,
                    count,
                    ..
                } => Some((*src, (*target, *count, *enabled))),
                _ => None,
            })
            .collect();
        for src in &old_dnat {
            if !new_dnat.contains_key(src) {
                if let Some(rc) = self.active_forwards.mutate(|m| m.remove(src)) {
                    drop(rc);
                }
            }
        }
        for (src, (target, count, enabled)) in &new_dnat {
            if !enabled {
                continue;
            }
            if self.active_forwards.peek(|m| m.contains_key(src)) {
                continue;
            }
            let prefix = crate::tunnel::forward::igd::prefix_for(self, target.ip()).await;
            let rc = self
                .forward
                .add_forward_range(*src, *target, *count, prefix, None)
                .await?;
            self.active_forwards.mutate(|m| {
                m.insert(*src, rc);
            });
        }
        self.forward.gc().await.log_err();

        self.db
            .mutate(|db| db.as_port_forwards_mut().ser(&PortForwards(want)))
            .await
            .result?;
        Ok(())
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
