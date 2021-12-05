use std::collections::{BTreeMap, VecDeque};
use std::net::{IpAddr, Ipv4Addr, SocketAddr, SocketAddrV4};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;

use bollard::Docker;
use patch_db::json_ptr::JsonPointer;
use patch_db::{PatchDb, Revision};
use reqwest::Url;
use rpc_toolkit::url::Host;
use rpc_toolkit::Context;
use serde::Deserialize;
use sqlx::sqlite::SqliteConnectOptions;
use sqlx::SqlitePool;
use tokio::fs::File;
use tokio::sync::{broadcast, oneshot, Mutex, RwLock};
use tracing::instrument;

use crate::core::rpc_continuations::{RequestGuid, RpcContinuation};
use crate::db::model::{Database, InstalledPackageDataEntry, PackageDataEntry};
use crate::hostname::{get_hostname, get_id};
use crate::install::cleanup::{cleanup_failed, uninstall};
use crate::manager::ManagerMap;
use crate::middleware::auth::HashSessionToken;
use crate::net::tor::os_key;
use crate::net::wifi::WpaCli;
use crate::net::NetController;
use crate::notifications::NotificationManager;
use crate::shutdown::Shutdown;
use crate::status::{MainStatus, Status};
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
    pub bind_static: Option<SocketAddr>,
    pub tor_control: Option<SocketAddr>,
    pub tor_socks: Option<SocketAddr>,
    pub revision_cache_size: Option<usize>,
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
    pub fn datadir(&self) -> &Path {
        self.datadir
            .as_deref()
            .unwrap_or_else(|| Path::new("/embassy-data"))
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
    #[instrument]
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
    is_closed: AtomicBool,
    pub bind_rpc: SocketAddr,
    pub bind_ws: SocketAddr,
    pub bind_static: SocketAddr,
    pub datadir: PathBuf,
    pub disk_guid: Arc<String>,
    pub db: PatchDb,
    pub secret_store: SqlitePool,
    pub docker: Docker,
    pub net_controller: NetController,
    pub managers: ManagerMap,
    pub revision_cache_size: usize,
    pub revision_cache: RwLock<VecDeque<Arc<Revision>>>,
    pub metrics_cache: RwLock<Option<crate::system::Metrics>>,
    pub shutdown: broadcast::Sender<Option<Shutdown>>,
    pub websocket_count: AtomicUsize,
    pub logger: EmbassyLogger,
    pub log_epoch: Arc<AtomicU64>,
    pub tor_socks: SocketAddr,
    pub notification_manager: NotificationManager,
    pub open_authed_websockets: Mutex<BTreeMap<HashSessionToken, Vec<oneshot::Sender<()>>>>,
    pub rpc_stream_continuations: Mutex<BTreeMap<RequestGuid, RpcContinuation>>,
    pub wifi_manager: Arc<RwLock<WpaCli>>,
}

#[derive(Clone)]
pub struct RpcContext(Arc<RpcContextSeed>);
impl RpcContext {
    #[instrument(skip(cfg_path))]
    pub async fn init<P: AsRef<Path>>(
        cfg_path: Option<P>,
        disk_guid: Arc<String>,
    ) -> Result<Self, Error> {
        let base = RpcContextConfig::load(cfg_path).await?;
        tracing::info!("Loaded Config");
        let logger = EmbassyLogger::init(base.log_server.clone(), false);
        tracing::info!("Set Logger");
        let (shutdown, _) = tokio::sync::broadcast::channel(1);
        let secret_store = base.secret_store().await?;
        tracing::info!("Opened Sqlite DB");
        let db = base.db(&secret_store).await?;
        tracing::info!("Opened PatchDB");
        let share = crate::db::DatabaseModel::new()
            .server_info()
            .share_stats()
            .get(&mut db.handle(), true)
            .await?;
        logger.set_sharing(*share);
        let docker = Docker::connect_with_unix_defaults()?;
        tracing::info!("Connected to Docker");
        let net_controller = NetController::init(
            ([127, 0, 0, 1], 80).into(),
            crate::net::tor::os_key(&mut secret_store.acquire().await?).await?,
            base.tor_control
                .unwrap_or(SocketAddr::from(([127, 0, 0, 1], 9051))),
            secret_store.clone(),
            None,
        )
        .await?;
        tracing::info!("Initialized Net Controller");
        let managers = ManagerMap::default();
        let metrics_cache = RwLock::new(None);
        let notification_manager = NotificationManager::new(secret_store.clone());
        tracing::info!("Initialized Notification Manager");
        let seed = Arc::new(RpcContextSeed {
            is_closed: AtomicBool::new(false),
            bind_rpc: base.bind_rpc.unwrap_or(([127, 0, 0, 1], 5959).into()),
            bind_ws: base.bind_ws.unwrap_or(([127, 0, 0, 1], 5960).into()),
            bind_static: base.bind_static.unwrap_or(([127, 0, 0, 1], 5961).into()),
            datadir: base.datadir().to_path_buf(),
            disk_guid,
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
            log_epoch: logger.epoch(),
            logger,
            tor_socks: base.tor_socks.unwrap_or(SocketAddr::V4(SocketAddrV4::new(
                Ipv4Addr::new(127, 0, 0, 1),
                9050,
            ))),
            notification_manager,
            open_authed_websockets: Mutex::new(BTreeMap::new()),
            rpc_stream_continuations: Mutex::new(BTreeMap::new()),
            wifi_manager: Arc::new(RwLock::new(WpaCli::init(
                "wlan0".to_string(),
                base.datadir().join("main"),
            ))),
        });
        let metrics_seed = seed.clone();
        tokio::spawn(async move {
            launch_metrics_task(&metrics_seed.metrics_cache, || {
                metrics_seed.shutdown.subscribe()
            })
            .await
        });
        let res = Self(seed);
        res.cleanup().await?;
        tracing::info!("Cleaned up transient states");
        res.managers
            .init(
                &res,
                &mut res.db.handle(),
                &mut res.secret_store.acquire().await?,
            )
            .await?;
        tracing::info!("Initialized Package Managers");
        Ok(res)
    }
    #[instrument(skip(self))]
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
                self.eos_registry_url().await?
            },
        )
    }
    #[instrument(skip(self))]
    pub async fn eos_registry_url(&self) -> Result<Url, Error> {
        Ok(crate::db::DatabaseModel::new()
            .server_info()
            .eos_marketplace()
            .get(&mut self.db.handle(), false)
            .await?
            .to_owned())
    }
    #[instrument(skip(self))]
    pub async fn shutdown(self) -> Result<(), Error> {
        self.managers.empty().await?;
        self.secret_store.close().await;
        self.is_closed.store(true, Ordering::SeqCst);
        if let Err(ctx) = Arc::try_unwrap(self.0) {
            tracing::warn!(
                "{} RPC Context(s) are still being held somewhere. This is likely a mistake.",
                Arc::strong_count(&ctx) - 1
            );
        }
        Ok(())
    }
    #[instrument(skip(self))]
    pub async fn cleanup(&self) -> Result<(), Error> {
        let mut db = self.db.handle();
        for package_id in crate::db::DatabaseModel::new()
            .package_data()
            .keys(&mut db, true)
            .await?
        {
            if let Err(e) = async {
                let mut pde = crate::db::DatabaseModel::new()
                    .package_data()
                    .idx_model(&package_id)
                    .expect(&mut db)
                    .await?
                    .get_mut(&mut db)
                    .await?;
                match &mut *pde {
                    PackageDataEntry::Installing { .. }
                    | PackageDataEntry::Restoring { .. }
                    | PackageDataEntry::Updating { .. } => {
                        cleanup_failed(self, &mut db, &package_id).await?;
                    }
                    PackageDataEntry::Removing { .. } => {
                        uninstall(self, &mut db, &package_id).await?;
                    }
                    PackageDataEntry::Installed {
                        installed:
                            InstalledPackageDataEntry {
                                status: Status { main, .. },
                                ..
                            },
                        ..
                    } => {
                        let new_main = match std::mem::replace(
                            main,
                            MainStatus::Stopped, /* placeholder */
                        ) {
                            MainStatus::BackingUp { started, health } => {
                                if let Some(started) = started {
                                    MainStatus::Running { started, health }
                                } else {
                                    MainStatus::Stopped
                                }
                            }
                            a => a,
                        };
                        *main = new_main;

                        pde.save(&mut db).await?;
                    }
                }
                Ok::<_, Error>(())
            }
            .await
            {
                tracing::error!("Failed to clean up package {}: {}", package_id, e);
                tracing::debug!("{:?}", e);
            }
        }
        Ok(())
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
        if self.0.is_closed.load(Ordering::SeqCst) {
            panic!(
                "RpcContext used after shutdown! {:?}",
                tracing_error::SpanTrace::capture()
            );
        }
        &*self.0
    }
}
