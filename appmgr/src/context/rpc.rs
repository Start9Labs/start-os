use std::net::{IpAddr, SocketAddr};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use bollard::Docker;
use patch_db::PatchDb;
use rpc_toolkit::url::Host;
use rpc_toolkit::Context;
use serde::Deserialize;
use sqlx::SqlitePool;
use tokio::fs::File;

use crate::net::mdns::MdnsController;
use crate::net::tor::TorController;
use crate::util::{from_toml_async_reader, AsyncFileExt, Container};
use crate::{Error, ResultExt};

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct RpcContextConfig {
    pub bind_rpc: Option<SocketAddr>,
    pub bind_ws: Option<SocketAddr>,
    pub tor_control: Option<SocketAddr>,
    pub db: Option<PathBuf>,
    pub secret_store: Option<PathBuf>,
}

pub struct RpcContextSeed {
    pub bind_rpc: SocketAddr,
    pub bind_ws: SocketAddr,
    pub db: PatchDb,
    pub secret_store: SqlitePool,
    pub docker: Docker,
    pub tor_controller: TorController,
    pub mdns_controller: MdnsController,
}

#[derive(Clone)]
pub struct RpcContext(Arc<RpcContextSeed>);
impl RpcContext {
    pub async fn init<P: AsRef<Path>>(cfg_path: Option<P>) -> Result<Self, Error> {
        let cfg_path = cfg_path
            .as_ref()
            .map(|p| p.as_ref())
            .unwrap_or(Path::new(crate::CONFIG_PATH));
        let base = if let Some(f) = File::maybe_open(cfg_path)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, cfg_path.display().to_string()))?
        {
            from_toml_async_reader(f).await?
        } else {
            RpcContextConfig::default()
        };
        let db = PatchDb::open(
            base.db
                .unwrap_or_else(|| Path::new("/mnt/embassy-os/embassy.db").to_owned()),
        )
        .await?;
        let secret_store = SqlitePool::connect(&format!(
            "sqlite://{}",
            base.secret_store
                .unwrap_or_else(|| Path::new("/mnt/embassy-os/secrets.db").to_owned())
                .display()
        ))
        .await?;
        let mut db_handle = db.handle();
        let tor_controller = TorController::init(
            base.tor_control.unwrap_or(([127, 0, 0, 1], 9051).into()),
            &mut db_handle,
            &mut secret_store.acquire().await?,
        )
        .await?;
        let mdns_controller = MdnsController::init(&mut db_handle).await?;
        let seed = Arc::new(RpcContextSeed {
            bind_rpc: base.bind_rpc.unwrap_or(([127, 0, 0, 1], 5959).into()),
            bind_ws: base.bind_ws.unwrap_or(([127, 0, 0, 1], 5960).into()),
            db,
            secret_store,
            docker: Docker::connect_with_unix_defaults()?,
            tor_controller,
            mdns_controller,
        });
        Ok(Self(seed))
    }
}
impl Context for RpcContext {
    fn host(&self) -> Host<&str> {
        match self.0.bind_rpc.ip() {
            IpAddr::V4(a) => Host::Ipv4(a),
            IpAddr::V6(a) => Host::Ipv6(a),
        }
    }
    fn port(&self) -> u16 {
        self.0.bind_rpc.port()
    }
}
impl Deref for RpcContext {
    type Target = RpcContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
