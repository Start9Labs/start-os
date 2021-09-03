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
use sqlx::migrate::MigrateDatabase;
use sqlx::{Sqlite, SqlitePool};
use tokio::fs::File;
use tokio::sync::broadcast::Sender;
use tokio::sync::RwLock;

use crate::manager::ManagerMap;
use crate::net::NetController;
use crate::shutdown::Shutdown;
use crate::util::{from_toml_async_reader, AsyncFileExt};
use crate::{Error, ResultExt};

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct RpcContextConfig {
    pub bind_rpc: Option<SocketAddr>,
    pub bind_ws: Option<SocketAddr>,
    pub tor_control: Option<SocketAddr>,
    pub revision_cache_size: Option<usize>,
    pub datadir: Option<PathBuf>,
}
impl RpcContextConfig {
    pub async fn load<P: AsRef<Path>>(path: Option<P>) -> Result<Self, Error> {
        let cfg_path = path
            .as_ref()
            .map(|p| p.as_ref())
            .unwrap_or(Path::new(crate::CONFIG_PATH));
        if let Some(f) = File::maybe_open(cfg_path)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, cfg_path.display().to_string()))?
        {
            from_toml_async_reader(f).await
        } else {
            Ok(RpcContextConfig::default())
        }
    }
    pub fn datadir(&self) -> &Path {
        self.datadir
            .as_ref()
            .map(|a| a.as_path())
            .unwrap_or_else(|| Path::new("/embassy-data"))
    }
    pub async fn db(&self) -> Result<PatchDb, Error> {
        PatchDb::open(self.datadir().join("main").join("embassy.db"))
            .await
            .map_err(Error::from)
    }
    pub async fn secret_store(&self) -> Result<SqlitePool, Error> {
        let secret_store_url = format!(
            "sqlite://{}",
            self.datadir().join("main").join("secrets.db").display()
        );
        if !Sqlite::database_exists(&secret_store_url).await? {
            Sqlite::create_database(&secret_store_url).await?;
        }
        let secret_store = SqlitePool::connect(&secret_store_url).await?;
        sqlx::migrate!()
            .run(&secret_store)
            .await
            .with_kind(crate::ErrorKind::Database)?;
        Ok(secret_store)
    }
}

pub struct RpcContextSeed {
    pub bind_rpc: SocketAddr,
    pub bind_ws: SocketAddr,
    pub datadir: PathBuf,
    pub db: PatchDb,
    pub secret_store: SqlitePool,
    pub docker: Docker,
    pub net_controller: NetController,
    pub managers: ManagerMap,
    pub revision_cache_size: usize,
    pub revision_cache: RwLock<VecDeque<Arc<Revision>>>,
    pub metrics_cache: RwLock<Option<crate::system::Metrics>>,
    pub shutdown: Sender<Option<Shutdown>>,
}

#[derive(Clone)]
pub struct RpcContext(Arc<RpcContextSeed>);
impl RpcContext {
    pub async fn init<P: AsRef<Path>>(
        cfg_path: Option<P>,
        shutdown: Sender<Option<Shutdown>>,
    ) -> Result<Self, Error> {
        let base = RpcContextConfig::load(cfg_path).await?;
        let db = base.db().await?;
        let secret_store = base.secret_store().await?;
        let docker = Docker::connect_with_unix_defaults()?;
        let net_controller = NetController::init(
            ([127, 0, 0, 1], 80).into(),
            crate::net::tor::os_key(&mut secret_store.acquire().await?).await?,
            base.tor_control
                .unwrap_or(SocketAddr::from(([127, 0, 0, 1], 9051))),
        )
        .await?;
        let managers = ManagerMap::default();
        let seed = Arc::new(RpcContextSeed {
            bind_rpc: base.bind_rpc.unwrap_or(([127, 0, 0, 1], 5959).into()),
            bind_ws: base.bind_ws.unwrap_or(([127, 0, 0, 1], 5960).into()),
            datadir: base.datadir().to_path_buf(),
            db,
            secret_store,
            docker,
            net_controller,
            managers,
            revision_cache_size: base.revision_cache_size.unwrap_or(512),
            revision_cache: RwLock::new(VecDeque::new()),
            metrics_cache: RwLock::new(None),
            shutdown,
        });
        let res = Self(seed);
        res.managers
            .init(
                &res,
                &mut res.db.handle(),
                &mut res.secret_store.acquire().await?,
            )
            .await?;
        // TODO: handle apps in bad / transient state
        Ok(res)
    }
    pub async fn package_registry_url(&self) -> Result<Url, Error> {
        Ok(
            if let Some(market) = crate::db::DatabaseModel::new()
                .server_info()
                .package_marketplace()
                .get(&mut self.db.handle(), false)
                .await?
                .to_owned()
            {
                market
            } else {
                crate::db::DatabaseModel::new()
                    .server_info()
                    .eos_marketplace()
                    .get(&mut self.db.handle(), false)
                    .await?
                    .to_owned()
            },
        )
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
