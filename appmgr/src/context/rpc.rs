use std::borrow::Cow;
use std::collections::{BTreeMap, VecDeque};
use std::net::{IpAddr, Ipv4Addr, SocketAddr, SocketAddrV4};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, AtomicUsize};
use std::sync::Arc;
use std::time::Duration;

use bollard::Docker;
use log::LevelFilter;
use patch_db::json_ptr::JsonPointer;
use patch_db::{PatchDb, Revision};
use reqwest::Url;
use rpc_toolkit::url::Host;
use rpc_toolkit::Context;
use serde::Deserialize;
use sqlx::sqlite::SqliteConnectOptions;
use sqlx::SqlitePool;
use tokio::fs::File;
use tokio::sync::broadcast::Sender;
use tokio::sync::RwLock;

use crate::db::model::Database;
use crate::hostname::{get_hostname, get_id};
use crate::manager::ManagerMap;
use crate::net::tor::os_key;
use crate::net::NetController;
use crate::notifications::NotificationManager;
use crate::shutdown::Shutdown;
use crate::system::launch_metrics_task;
use crate::util::io::from_toml_async_reader;
use crate::util::logger::EmbassyLogger;
use crate::util::AsyncFileExt;
use crate::{Error, ResultExt};

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct RpcContextConfig {
    pub bind_rpc: Option<SocketAddr>,
    pub bind_ws: Option<SocketAddr>,
    pub tor_control: Option<SocketAddr>,
    pub tor_socks: Option<SocketAddr>,
    pub revision_cache_size: Option<usize>,
    pub zfs_pool_name: Option<String>,
    pub datadir: Option<PathBuf>,
    pub log_server: Option<Url>,
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
            Ok(Self::default())
        }
    }
    pub fn zfs_pool_name(&self) -> &str {
        self.zfs_pool_name
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or("embassy-data")
    }
    pub fn datadir(&self) -> Cow<'_, Path> {
        self.datadir
            .as_ref()
            .map(|a| Cow::Borrowed(a.as_path()))
            .unwrap_or_else(|| Cow::Owned(Path::new("/").join(self.zfs_pool_name())))
    }
    pub async fn db(&self, secret_store: &SqlitePool) -> Result<PatchDb, Error> {
        let db_path = self.datadir().join("main").join("embassy.db");
        let db = PatchDb::open(&db_path)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, db_path.display().to_string()))?;
        if !db.exists(&<JsonPointer>::default()).await? {
            db.put(
                &<JsonPointer>::default(),
                &Database::init(
                    get_id().await?,
                    &get_hostname().await?,
                    &os_key(&mut secret_store.acquire().await?).await?,
                ),
                None,
            )
            .await?;
        }
        Ok(db)
    }
    pub async fn secret_store(&self) -> Result<SqlitePool, Error> {
        let secret_store = SqlitePool::connect_with(
            SqliteConnectOptions::new()
                .filename(self.datadir().join("main").join("secrets.db"))
                .create_if_missing(true)
                .busy_timeout(Duration::from_secs(30)),
        )
        .await?;
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
    pub zfs_pool_name: Arc<String>,
    pub db: PatchDb,
    pub secret_store: SqlitePool,
    pub docker: Docker,
    pub net_controller: NetController,
    pub managers: ManagerMap,
    pub revision_cache_size: usize,
    pub revision_cache: RwLock<VecDeque<Arc<Revision>>>,
    pub metrics_cache: RwLock<Option<crate::system::Metrics>>,
    pub shutdown: Sender<Option<Shutdown>>,
    pub websocket_count: AtomicUsize,
    pub logger: EmbassyLogger,
    pub log_epoch: Arc<AtomicU64>,
    pub tor_socks: SocketAddr,
    pub notification_manager: NotificationManager,
}

#[derive(Clone)]
pub struct RpcContext(Arc<RpcContextSeed>);
impl RpcContext {
    pub async fn init<P: AsRef<Path>>(
        cfg_path: Option<P>,
        log_level: LevelFilter,
        module_logging: BTreeMap<String, LevelFilter>,
    ) -> Result<Self, Error> {
        let base = RpcContextConfig::load(cfg_path).await?;
        let log_epoch = Arc::new(AtomicU64::new(rand::random()));
        let logger = EmbassyLogger::init(
            log_level,
            log_epoch.clone(),
            base.log_server.clone(),
            false,
            module_logging,
        );
        let (shutdown, _) = tokio::sync::broadcast::channel(1);
        let secret_store = base.secret_store().await?;
        let db = base.db(&secret_store).await?;
        let share = crate::db::DatabaseModel::new()
            .server_info()
            .share_stats()
            .get(&mut db.handle(), true)
            .await?;
        logger.set_sharing(*share);
        let docker = Docker::connect_with_unix_defaults()?;
        let net_controller = NetController::init(
            ([127, 0, 0, 1], 80).into(),
            crate::net::tor::os_key(&mut secret_store.acquire().await?).await?,
            base.tor_control
                .unwrap_or(SocketAddr::from(([127, 0, 0, 1], 9051))),
            secret_store.clone(),
        )
        .await?;
        let managers = ManagerMap::default();
        let metrics_cache = RwLock::new(None);
        let notification_manager = NotificationManager::new(secret_store.clone(), db.clone(), 3600);
        let seed = Arc::new(RpcContextSeed {
            bind_rpc: base.bind_rpc.unwrap_or(([127, 0, 0, 1], 5959).into()),
            bind_ws: base.bind_ws.unwrap_or(([127, 0, 0, 1], 5960).into()),
            datadir: base.datadir().to_path_buf(),
            zfs_pool_name: Arc::new(base.zfs_pool_name().to_owned()),
            db,
            secret_store,
            docker,
            net_controller,
            managers,
            revision_cache_size: base.revision_cache_size.unwrap_or(512),
            revision_cache: RwLock::new(VecDeque::new()),
            metrics_cache,
            shutdown,
            websocket_count: AtomicUsize::new(0),
            logger,
            log_epoch,
            tor_socks: base.tor_socks.unwrap_or(SocketAddr::V4(SocketAddrV4::new(
                Ipv4Addr::new(127, 0, 0, 1),
                9050,
            ))),
            notification_manager,
        });
        let metrics_seed = seed.clone();
        tokio::spawn(async move {
            launch_metrics_task(&metrics_seed.metrics_cache, || {
                metrics_seed.shutdown.subscribe()
            })
            .await
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
                .get(&mut self.db.handle(), true)
                .await?
                .to_owned()
            {
                market
            } else {
                self.eos_registry_url().await?
            },
        )
    }
    pub async fn eos_registry_url(&self) -> Result<Url, Error> {
        Ok(crate::db::DatabaseModel::new()
            .server_info()
            .eos_marketplace()
            .get(&mut self.db.handle(), true)
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
