use std::collections::BTreeSet;
use std::net::{IpAddr, Ipv6Addr, SocketAddr};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::Parser;
use imbl::OrdMap;
use imbl_value::InternedString;
use patch_db::PatchDb;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{CallRemote, Context, Empty};
use serde::{Deserialize, Serialize};
use tokio::sync::broadcast::Sender;
use tracing::instrument;

use crate::auth::{Sessions, check_password};
use crate::context::CliContext;
use crate::context::config::ContextConfig;
use crate::middleware::auth::AuthContext;
use crate::middleware::signature::SignatureAuthContext;
use crate::net::forward::PortForwardController;
use crate::net::gateway::NetworkInterfaceWatcher;
use crate::prelude::*;
use crate::rpc_continuations::{OpenAuthedContinuations, RpcContinuations};
use crate::tunnel::TUNNEL_DEFAULT_PORT;
use crate::tunnel::db::TunnelDatabase;
use crate::util::sync::SyncMutex;

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
    pub shutdown: Sender<()>,
}

#[derive(Clone)]
pub struct TunnelContext(Arc<TunnelContextSeed>);
impl TunnelContext {
    #[instrument(skip_all)]
    pub async fn init(config: &TunnelConfig) -> Result<Self, Error> {
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

impl SignatureAuthContext for TunnelContext {
    type Database = TunnelDatabase;
    type AdditionalMetadata = ();
    type CheckPubkeyRes = ();
    fn db(&self) -> &TypedPatchDb<Self::Database> {
        &self.db
    }
    async fn sig_context(
        &self,
    ) -> impl IntoIterator<Item = Result<impl AsRef<str> + Send, Error>> + Send {
        self.addrs
            .iter()
            .filter(|a| !match a {
                IpAddr::V4(a) => a.is_loopback() || a.is_unspecified(),
                IpAddr::V6(a) => a.is_loopback() || a.is_unspecified(),
            })
            .map(|a| InternedString::from_display(&a))
            .map(Ok)
    }
    fn check_pubkey(
        db: &Model<Self::Database>,
        pubkey: Option<&crate::sign::AnyVerifyingKey>,
        _: Self::AdditionalMetadata,
    ) -> Result<Self::CheckPubkeyRes, Error> {
        if let Some(pubkey) = pubkey {
            if db.as_auth_pubkeys().de()?.contains(pubkey) {
                return Ok(());
            }
        }

        Err(Error::new(
            eyre!("Developer Key is not authorized"),
            ErrorKind::IncorrectPassword,
        ))
    }
    async fn post_auth_hook(
        &self,
        _: Self::CheckPubkeyRes,
        _: &rpc_toolkit::RpcRequest,
    ) -> Result<(), Error> {
        Ok(())
    }
}
impl AuthContext for TunnelContext {
    const LOCAL_AUTH_COOKIE_PATH: &str = "/run/start-tunnel/rpc.authcookie";
    const LOCAL_AUTH_COOKIE_OWNERSHIP: &str = "root:root";
    fn access_sessions(db: &mut Model<Self::Database>) -> &mut Model<crate::auth::Sessions> {
        db.as_sessions_mut()
    }
    fn ephemeral_sessions(&self) -> &SyncMutex<Sessions> {
        &self.ephemeral_sessions
    }
    fn open_authed_continuations(&self) -> &OpenAuthedContinuations<Option<InternedString>> {
        &self.open_authed_continuations
    }
    fn check_password(db: &Model<Self::Database>, password: &str) -> Result<(), Error> {
        check_password(&db.as_password().de()?, password)
    }
}

impl CallRemote<TunnelContext> for CliContext {
    async fn call_remote(
        &self,
        mut method: &str,
        params: Value,
        _: Empty,
    ) -> Result<Value, RpcError> {
        let tunnel_addr = if let Some(addr) = self.tunnel_addr {
            addr
        } else if let Some(addr) = self.tunnel_listen {
            addr
        } else {
            return Err(Error::new(eyre!("`--tunnel` required"), ErrorKind::InvalidRequest).into());
        };
        let sig_addr = self.tunnel_listen.unwrap_or(tunnel_addr);
        let url = format!("https://{tunnel_addr}").parse()?;

        method = method.strip_prefix("tunnel.").unwrap_or(method);

        crate::middleware::signature::call_remote(
            self,
            url,
            &InternedString::from_display(&sig_addr.ip()),
            method,
            params,
        )
        .await
    }
}
