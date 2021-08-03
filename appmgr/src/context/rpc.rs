use std::collections::VecDeque;
use std::net::{IpAddr, SocketAddr};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use bollard::Docker;
use patch_db::{PatchDb, Revision};
use reqwest::Url;
use rpc_toolkit::url::Host;
use rpc_toolkit::Context;
use serde::Deserialize;
use sqlx::SqlitePool;
use tokio::fs::File;
use tokio::sync::RwLock;

use crate::manager::ManagerMap;
use crate::net::NetController;
use crate::util::{from_toml_async_reader, AsyncFileExt};
use crate::{Error, ResultExt};

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct RpcContextConfig {
    pub bind_rpc: Option<SocketAddr>,
    pub bind_ws: Option<SocketAddr>,
    pub tor_control: Option<SocketAddr>,
    pub db: Option<PathBuf>,
    pub secret_store: Option<PathBuf>,
    pub revision_cache_size: Option<usize>,
}

pub struct RpcContextSeed {
    pub bind_rpc: SocketAddr,
    pub bind_ws: SocketAddr,
    pub db: PatchDb,
    pub secret_store: SqlitePool,
    pub docker: Docker,
    pub net_controller: Arc<NetController>,
    pub managers: ManagerMap,
    pub revision_cache_size: usize,
    pub revision_cache: RwLock<VecDeque<Arc<Revision>>>,
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
        let docker = Docker::connect_with_unix_defaults()?;
        let net_controller = Arc::new(
            NetController::init(
                base.tor_control
                    .unwrap_or(SocketAddr::from(([127, 0, 0, 1], 9051))),
            )
            .await?,
        );
        let managers = ManagerMap::init(
            &mut db.handle(),
            &mut secret_store.acquire().await?,
            docker.clone(),
            net_controller.clone(),
        )
        .await?;
        let seed = Arc::new(RpcContextSeed {
            bind_rpc: base.bind_rpc.unwrap_or(([127, 0, 0, 1], 5959).into()),
            bind_ws: base.bind_ws.unwrap_or(([127, 0, 0, 1], 5960).into()),
            db,
            secret_store,
            docker,
            net_controller,
            managers,
            revision_cache_size: base.revision_cache_size.unwrap_or(512),
            revision_cache: RwLock::new(VecDeque::new()),
        });
        Ok(Self(seed))
    }
    pub async fn package_registry_url(&self) -> Result<Url, Error> {
        Ok(crate::db::DatabaseModel::new()
            .server_info()
            .registry()
            .get(&mut self.db.handle(), false)
            .await?
            .to_owned())
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
