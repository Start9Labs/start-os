use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, SocketAddr, SocketAddrV4};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock};

use clap::Parser;
use cookie::{Cookie, Expiration, SameSite};
use http::HeaderMap;
use imbl::OrdMap;
use imbl_value::InternedString;
use include_dir::Dir;
use ipnet::Ipv4Net;
use patch_db::PatchDb;
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
use crate::net::forward::{PortForwardController, add_iptables_rule};
use crate::net::static_server::{EMPTY_DIR, UiContext};
use crate::prelude::*;
use crate::rpc_continuations::{OpenAuthedContinuations, RpcContinuations};
use crate::tunnel::TUNNEL_DEFAULT_LISTEN;
use crate::tunnel::api::tunnel_api;
use crate::tunnel::db::TunnelDatabase;
use crate::tunnel::wg::{WIREGUARD_INTERFACE_NAME, WgSubnetConfig};
use crate::util::collections::OrdMapIterMut;
use crate::util::io::read_file_to_string;
use crate::util::sync::{SyncMutex, Watch};

#[derive(Debug, Clone, Default, Deserialize, Serialize, Parser)]
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

pub struct TunnelContextSeed {
    pub listen: SocketAddr,
    pub db: TypedPatchDb<TunnelDatabase>,
    pub datadir: PathBuf,
    pub rpc_continuations: RpcContinuations,
    pub open_authed_continuations: OpenAuthedContinuations<Option<InternedString>>,
    pub ephemeral_sessions: SyncMutex<Sessions>,
    pub net_iface: Watch<OrdMap<GatewayId, NetworkInterfaceInfo>>,
    pub forward: PortForwardController,
    pub active_forwards: SyncMutex<BTreeMap<SocketAddrV4, Arc<()>>>,
    pub shutdown: Sender<()>,
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
        let db = TypedPatchDb::<TunnelDatabase>::load_or_init(
            PatchDb::open(&db_path).await?,
            || async {
                let mut db = TunnelDatabase::default();
                db.wg.subnets.0.insert(
                    Ipv4Net::new_assert([10, 59, rand::random(), 1].into(), 24),
                    WgSubnetConfig {
                        name: "Default Subnet".into(),
                        ..Default::default()
                    },
                );
                Ok(db)
            },
        )
        .await?;
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
        add_iptables_rule(
            false,
            false,
            &[
                "FORWARD",
                "-i",
                WIREGUARD_INTERFACE_NAME,
                "-m",
                "state",
                "--state",
                "NEW",
                "-j",
                "ACCEPT",
            ],
        )
        .await?;

        let peek = db.peek().await;
        peek.as_wg().de()?.sync().await?;

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
                add_iptables_rule(
                    true,
                    false,
                    &[
                        "POSTROUTING",
                        "-s",
                        &subnet.trunc().to_string(),
                        "-o",
                        iface.as_str(),
                        "-j",
                        "MASQUERADE",
                    ],
                )
                .await?;
            }
        }

        let mut active_forwards = BTreeMap::new();
        for (from, entry) in peek.as_port_forwards().de()?.0 {
            if !entry.enabled {
                continue;
            }
            let to = entry.target;
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
            active_forwards.insert(from, forward.add_forward(from, to, prefix, None).await?);
        }

        Ok(Self(Arc::new(TunnelContextSeed {
            listen,
            db,
            datadir,
            rpc_continuations: RpcContinuations::new(),
            open_authed_continuations: OpenAuthedContinuations::new(),
            ephemeral_sessions: SyncMutex::new(Sessions::new()),
            net_iface,
            forward,
            active_forwards: SyncMutex::new(active_forwards),
            shutdown,
        })))
    }

    pub async fn gc_forwards(&self, keep: &BTreeSet<SocketAddrV4>) -> Result<(), Error> {
        self.active_forwards
            .mutate(|pf| pf.retain(|k, _| keep.contains(k)));
        self.forward.gc().await
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
