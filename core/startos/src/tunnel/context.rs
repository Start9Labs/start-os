use std::collections::{BTreeMap, BTreeSet};
use std::net::{IpAddr, Ipv6Addr, SocketAddr};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::Parser;
use cookie::{Cookie, Expiration, SameSite};
use helpers::NonDetachingJoinHandle;
use http::HeaderMap;
use imbl::OrdMap;
use imbl_value::InternedString;
use models::GatewayId;
use patch_db::PatchDb;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{CallRemote, Context, Empty};
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use tokio::sync::broadcast::Sender;
use tracing::instrument;
use url::Url;

use crate::auth::Sessions;
use crate::context::config::ContextConfig;
use crate::context::{CliContext, RpcContext};
use crate::db::model::public::NetworkInterfaceType;
use crate::middleware::auth::AuthContext;
use crate::net::forward::PortForwardController;
use crate::net::gateway::{IdFilter, InterfaceFilter, NetworkInterfaceWatcher};
use crate::prelude::*;
use crate::rpc_continuations::{OpenAuthedContinuations, RpcContinuations};
use crate::tunnel::db::{GatewayPort, TunnelDatabase};
use crate::tunnel::TUNNEL_DEFAULT_PORT;
use crate::util::io::read_file_to_string;
use crate::util::sync::SyncMutex;
use crate::util::Invoke;

#[derive(Debug, Clone, Default, Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct TunnelConfig {
    #[arg(short = 'c', long = "config")]
    pub config: Option<PathBuf>,
    #[arg(short = 'l', long = "listen")]
    pub tunnel_listen: Option<SocketAddr>,
    #[arg(short = 'd', long = "datadir")]
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
    pub addrs: BTreeSet<IpAddr>,
    pub db: TypedPatchDb<TunnelDatabase>,
    pub datadir: PathBuf,
    pub rpc_continuations: RpcContinuations,
    pub open_authed_continuations: OpenAuthedContinuations<Option<InternedString>>,
    pub ephemeral_sessions: SyncMutex<Sessions>,
    pub net_iface: NetworkInterfaceWatcher,
    pub forward: PortForwardController,
    pub active_forwards: SyncMutex<BTreeMap<GatewayPort, Arc<()>>>,
    pub masquerade_thread: NonDetachingJoinHandle<()>,
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
            || async { Ok(Default::default()) },
        )
        .await?;
        let listen = config.tunnel_listen.unwrap_or(SocketAddr::new(
            Ipv6Addr::UNSPECIFIED.into(),
            TUNNEL_DEFAULT_PORT,
        ));
        let net_iface = NetworkInterfaceWatcher::new(async { OrdMap::new() }, []);
        let forward = PortForwardController::new(net_iface.subscribe());

        Command::new("sysctl")
            .arg("-w")
            .arg("net.ipv4.ip_forward=1")
            .invoke(ErrorKind::Network)
            .await?;

        let mut masquerade_net_iface = net_iface.subscribe();
        let masquerade_thread = tokio::spawn(async move {
            loop {
                for iface in masquerade_net_iface.peek(|i| {
                    i.iter()
                        .filter(|(_, info)| {
                            dbg!(info).ip_info.as_ref().map_or(false, |i| {
                                dbg!(i).device_type != Some(NetworkInterfaceType::Wireguard)
                            })
                        })
                        .map(|(name, _)| name)
                        .cloned()
                        .collect::<Vec<_>>()
                }) {
                    if Command::new("iptables")
                        .arg("-t")
                        .arg("nat")
                        .arg("-C")
                        .arg("POSTROUTING")
                        .arg("-o")
                        .arg(iface.as_str())
                        .arg("-j")
                        .arg("MASQUERADE")
                        .invoke(ErrorKind::Network)
                        .await
                        .is_err()
                    {
                        tracing::info!("Adding masquerade rule for interface {}", iface);
                        Command::new("iptables")
                            .arg("-t")
                            .arg("nat")
                            .arg("-A")
                            .arg("POSTROUTING")
                            .arg("-o")
                            .arg(iface.as_str())
                            .arg("-j")
                            .arg("MASQUERADE")
                            .invoke(ErrorKind::Network)
                            .await
                            .log_err();
                    }
                }

                masquerade_net_iface.changed().await;

                tracing::info!("Network interfaces changed, updating masquerade rules");
            }
        })
        .into();

        let peek = db.peek().await;
        peek.as_wg().de()?.sync().await?;
        let mut active_forwards = BTreeMap::new();
        for (from, to) in peek.as_port_forwards().de()?.0 {
            active_forwards.insert(
                from.clone(),
                forward
                    .add(from.1, IdFilter(from.0).into_dyn(), to.into())
                    .await?,
            );
        }

        Ok(Self(Arc::new(TunnelContextSeed {
            listen,
            addrs: crate::net::utils::all_socket_addrs_for(listen.port())
                .await?
                .into_iter()
                .map(|(_, a)| a.ip())
                .collect(),
            db,
            datadir,
            rpc_continuations: RpcContinuations::new(),
            open_authed_continuations: OpenAuthedContinuations::new(),
            ephemeral_sessions: SyncMutex::new(Sessions::new()),
            net_iface,
            forward,
            active_forwards: SyncMutex::new(active_forwards),
            masquerade_thread,
            shutdown,
        })))
    }
}
impl AsRef<RpcContinuations> for TunnelContext {
    fn as_ref(&self) -> &RpcContinuations {
        &self.rpc_continuations
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
    pub tunnel: IpAddr,
}

impl CallRemote<TunnelContext> for CliContext {
    async fn call_remote(
        &self,
        mut method: &str,
        params: Value,
        _: Empty,
    ) -> Result<Value, RpcError> {
        let local =
            if let Ok(local) = read_file_to_string(TunnelContext::LOCAL_AUTH_COOKIE_PATH).await {
                self.cookie_store
                    .lock()
                    .unwrap()
                    .insert_raw(
                        &Cookie::build(("local", local))
                            .domain("localhost")
                            .expires(Expiration::Session)
                            .same_site(SameSite::Strict)
                            .build(),
                        &"http://localhost".parse()?,
                    )
                    .with_kind(crate::ErrorKind::Network)?;
                true
            } else {
                false
            };

        let tunnel_addr = if let Some(addr) = self.tunnel_addr {
            Some(addr)
        } else if let Some(addr) = self.tunnel_listen {
            Some(addr)
        } else {
            None
        };
        let (url, sig_ctx) = if let Some(tunnel_addr) = tunnel_addr {
            (
                format!("https://{tunnel_addr}/rpc/v0").parse()?,
                Some(InternedString::from_display(
                    &self.tunnel_listen.unwrap_or(tunnel_addr).ip(),
                )),
            )
        } else if local {
            (
                format!("http://localhost:{TUNNEL_DEFAULT_PORT}/rpc/v0").parse()?,
                None,
            )
        } else {
            return Err(Error::new(eyre!("`--tunnel` required"), ErrorKind::InvalidRequest).into());
        };

        method = method.strip_prefix("tunnel.").unwrap_or(method);

        crate::middleware::signature::call_remote(
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
    pub tunnel: Url,
}

impl CallRemote<TunnelContext, TunnelUrlParams> for RpcContext {
    async fn call_remote(
        &self,
        mut method: &str,
        params: Value,
        TunnelUrlParams { tunnel }: TunnelUrlParams,
    ) -> Result<Value, RpcError> {
        let url = tunnel.join("rpc/v0")?;
        method = method.strip_prefix("tunnel.").unwrap_or(method);

        let sig_ctx = url.host_str().map(InternedString::from_display);

        crate::middleware::signature::call_remote(
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
