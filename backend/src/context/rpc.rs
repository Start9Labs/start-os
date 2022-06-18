use std::collections::{BTreeMap, VecDeque};
use std::net::{IpAddr, Ipv4Addr, SocketAddr, SocketAddrV4};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

use bollard::Docker;
use patch_db::json_ptr::JsonPointer;
use patch_db::{DbHandle, LockReceipt, LockType, PatchDb, Revision};
use reqwest::Url;
use rpc_toolkit::url::Host;
use rpc_toolkit::Context;
use serde::Deserialize;
use sqlx::sqlite::SqliteConnectOptions;
use sqlx::SqlitePool;
use tokio::fs::File;
use tokio::process::Command;
use tokio::sync::{broadcast, oneshot, Mutex, RwLock};
use tracing::instrument;

use crate::core::rpc_continuations::{RequestGuid, RpcContinuation};
use crate::db::model::{Database, InstalledPackageDataEntry, PackageDataEntry};
use crate::hostname::{derive_hostname, derive_id, get_product_key};
use crate::install::cleanup::{cleanup_failed, uninstall, CleanupFailedReceipts};
use crate::manager::ManagerMap;
use crate::middleware::auth::HashSessionToken;
use crate::net::tor::os_key;
use crate::net::wifi::WpaCli;
use crate::net::NetController;
use crate::notifications::NotificationManager;
use crate::setup::password_hash;
use crate::shutdown::Shutdown;
use crate::status::{MainStatus, Status};
use crate::util::io::from_yaml_async_reader;
use crate::util::{AsyncFileExt, Invoke};
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
            .unwrap_or(Path::new(crate::util::config::CONFIG_PATH));
        if let Some(f) = File::maybe_open(cfg_path)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, cfg_path.display().to_string()))?
        {
            from_yaml_async_reader(f).await
        } else {
            Ok(Self::default())
        }
    }
    pub fn datadir(&self) -> &Path {
        self.datadir
            .as_deref()
            .unwrap_or_else(|| Path::new("/embassy-data"))
    }
    pub async fn db(&self, secret_store: &SqlitePool, product_key: &str) -> Result<PatchDb, Error> {
        let sid = derive_id(product_key);
        let hostname = derive_hostname(&sid);
        let db_path = self.datadir().join("main").join("embassy.db");
        let db = PatchDb::open(&db_path)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, db_path.display().to_string()))?;
        if !db.exists(&<JsonPointer>::default()).await? {
            db.put(
                &<JsonPointer>::default(),
                &Database::init(
                    sid,
                    &hostname,
                    &os_key(&mut secret_store.acquire().await?).await?,
                    password_hash(&mut secret_store.acquire().await?).await?,
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
    pub tor_socks: SocketAddr,
    pub notification_manager: NotificationManager,
    pub open_authed_websockets: Mutex<BTreeMap<HashSessionToken, Vec<oneshot::Sender<()>>>>,
    pub rpc_stream_continuations: Mutex<BTreeMap<RequestGuid, RpcContinuation>>,
    pub wifi_manager: Arc<RwLock<WpaCli>>,
}

pub struct RpcCleanReceipts {
    cleanup_receipts: CleanupFailedReceipts,
    packages: LockReceipt<crate::db::model::AllPackageData, ()>,
    package: LockReceipt<crate::db::model::PackageDataEntry, String>,
}

impl RpcCleanReceipts {
    pub async fn new<'a>(db: &'a mut impl DbHandle) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks);
        Ok(setup(&db.lock_all(locks).await?)?)
    }

    pub fn setup(
        locks: &mut Vec<patch_db::LockTargetId>,
    ) -> impl FnOnce(&patch_db::Verifier) -> Result<Self, Error> {
        let cleanup_receipts = CleanupFailedReceipts::setup(locks);

        let packages = crate::db::DatabaseModel::new()
            .package_data()
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        let package = crate::db::DatabaseModel::new()
            .package_data()
            .star()
            .make_locker(LockType::Write)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                cleanup_receipts: cleanup_receipts(skeleton_key)?,
                packages: packages.verify(skeleton_key)?,
                package: package.verify(skeleton_key)?,
            })
        }
    }
}

pub struct RpcSetNginxReceipts {
    server_info: LockReceipt<crate::db::model::ServerInfo, ()>,
}

impl RpcSetNginxReceipts {
    pub async fn new(db: &'_ mut impl DbHandle) -> Result<Self, Error> {
        let mut locks = Vec::new();

        let setup = Self::setup(&mut locks);
        Ok(setup(&db.lock_all(locks).await?)?)
    }

    pub fn setup(
        locks: &mut Vec<patch_db::LockTargetId>,
    ) -> impl FnOnce(&patch_db::Verifier) -> Result<Self, Error> {
        let server_info = crate::db::DatabaseModel::new()
            .server_info()
            .make_locker(LockType::Read)
            .add_to_keys(locks);
        move |skeleton_key| {
            Ok(Self {
                server_info: server_info.verify(skeleton_key)?,
            })
        }
    }
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
        let tor_proxy = base.tor_socks.unwrap_or(SocketAddr::V4(SocketAddrV4::new(
            Ipv4Addr::new(127, 0, 0, 1),
            9050,
        )));
        let (shutdown, _) = tokio::sync::broadcast::channel(1);
        let secret_store = base.secret_store().await?;
        tracing::info!("Opened Sqlite DB");
        let db = base.db(&secret_store, &get_product_key().await?).await?;
        tracing::info!("Opened PatchDB");
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
            tor_socks: tor_proxy,
            notification_manager,
            open_authed_websockets: Mutex::new(BTreeMap::new()),
            rpc_stream_continuations: Mutex::new(BTreeMap::new()),
            wifi_manager: Arc::new(RwLock::new(WpaCli::init("wlan0".to_string()))),
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

    #[instrument(skip(self, db, receipts))]
    pub async fn set_nginx_conf<Db: DbHandle>(
        &self,
        db: &mut Db,
        receipts: RpcSetNginxReceipts,
    ) -> Result<(), Error> {
        tokio::fs::write("/etc/nginx/sites-available/default", {
            let info = receipts.server_info.get(db).await?;
            format!(
                include_str!("../nginx/main-ui.conf.template"),
                lan_hostname = info.lan_address.host_str().unwrap(),
                tor_hostname = info.tor_address.host_str().unwrap(),
            )
        })
        .await
        .with_ctx(|_| {
            (
                crate::ErrorKind::Filesystem,
                "/etc/nginx/sites-available/default",
            )
        })?;
        Command::new("systemctl")
            .arg("reload")
            .arg("nginx")
            .invoke(crate::ErrorKind::Nginx)
            .await?;
        Ok(())
    }
    #[instrument(skip(self))]
    pub async fn shutdown(self) -> Result<(), Error> {
        self.managers.empty().await?;
        self.secret_store.close().await;
        self.is_closed.store(true, Ordering::SeqCst);
        Ok(())
    }

    #[instrument(skip(self))]
    pub async fn cleanup(&self) -> Result<(), Error> {
        let mut db = self.db.handle();
        let receipts = RpcCleanReceipts::new(&mut db).await?;
        for (package_id, package) in receipts.packages.get(&mut db).await?.0 {
            if let Err(e) = async {
                match package {
                    PackageDataEntry::Installing { .. }
                    | PackageDataEntry::Restoring { .. }
                    | PackageDataEntry::Updating { .. } => {
                        cleanup_failed(self, &mut db, &package_id, &receipts.cleanup_receipts)
                            .await?;
                    }
                    PackageDataEntry::Removing { .. } => {
                        uninstall(
                            self,
                            &mut db,
                            &mut self.secret_store.acquire().await?,
                            &package_id,
                        )
                        .await?;
                    }
                    PackageDataEntry::Installed {
                        installed,
                        static_files,
                        manifest,
                    } => {
                        let status = installed.status;
                        let main = match status.main {
                            MainStatus::BackingUp { started, .. } => {
                                if let Some(_) = started {
                                    MainStatus::Starting { restarting: false }
                                } else {
                                    MainStatus::Stopped
                                }
                            }
                            MainStatus::Running { .. } => {
                                MainStatus::Starting { restarting: false }
                            }
                            a => a.clone(),
                        };
                        let new_package = PackageDataEntry::Installed {
                            installed: InstalledPackageDataEntry {
                                status: Status { main, ..status },
                                ..installed
                            },
                            static_files,
                            manifest,
                        };
                        receipts
                            .package
                            .set(&mut db, new_package, &package_id)
                            .await?;
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
        #[cfg(feature = "unstable")]
        if self.0.is_closed.load(Ordering::SeqCst) {
            panic!(
                "RpcContext used after shutdown! {}",
                tracing_error::SpanTrace::capture()
            );
        }
        &*self.0
    }
}
impl Drop for RpcContext {
    fn drop(&mut self) {
        #[cfg(feature = "unstable")]
        if self.0.is_closed.load(Ordering::SeqCst) {
            tracing::info!(
                "RpcContext dropped. {} left.",
                Arc::strong_count(&self.0) - 1
            );
        }
    }
}
