use std::collections::BTreeMap;
use std::net::{Ipv4Addr, SocketAddr, SocketAddrV4};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

use bollard::Docker;
use helpers::to_tmp_path;
use josekit::jwk::Jwk;
use patch_db::json_ptr::JsonPointer;
use patch_db::{DbHandle, LockReceipt, LockType, PatchDb};
use reqwest::Url;
use rpc_toolkit::Context;
use serde::Deserialize;
use sqlx::postgres::PgConnectOptions;
use sqlx::PgPool;
use tokio::sync::{broadcast, oneshot, Mutex, RwLock};
use tracing::instrument;

use super::setup::CURRENT_SECRET;
use crate::account::AccountInfo;
use crate::core::rpc_continuations::{RequestGuid, RestHandler, RpcContinuation};
use crate::db::model::{CurrentDependents, Database, InstalledPackageDataEntry, PackageDataEntry};
use crate::disk::OsPartitionInfo;
use crate::init::init_postgres;
use crate::install::cleanup::{cleanup_failed, uninstall, CleanupFailedReceipts};
use crate::manager::ManagerMap;
use crate::middleware::auth::HashSessionToken;
use crate::net::net_controller::NetController;
use crate::net::ssl::SslManager;
use crate::net::wifi::WpaCli;
use crate::notifications::NotificationManager;
use crate::shutdown::Shutdown;
use crate::status::{MainStatus, Status};
use crate::util::config::load_config_from_paths;
use crate::{Error, ErrorKind, ResultExt};

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct RpcContextConfig {
    pub wifi_interface: Option<String>,
    pub ethernet_interface: String,
    pub os_partitions: OsPartitionInfo,
    pub migration_batch_rows: Option<usize>,
    pub migration_prefetch_rows: Option<usize>,
    pub bind_rpc: Option<SocketAddr>,
    pub tor_control: Option<SocketAddr>,
    pub tor_socks: Option<SocketAddr>,
    pub dns_bind: Option<Vec<SocketAddr>>,
    pub revision_cache_size: Option<usize>,
    pub datadir: Option<PathBuf>,
    pub log_server: Option<Url>,
}
impl RpcContextConfig {
    pub async fn load<P: AsRef<Path> + Send + 'static>(path: Option<P>) -> Result<Self, Error> {
        tokio::task::spawn_blocking(move || {
            load_config_from_paths(
                path.as_ref()
                    .into_iter()
                    .map(|p| p.as_ref())
                    .chain(std::iter::once(Path::new(
                        crate::util::config::DEVICE_CONFIG_PATH,
                    )))
                    .chain(std::iter::once(Path::new(crate::util::config::CONFIG_PATH))),
            )
        })
        .await
        .unwrap()
    }
    pub fn datadir(&self) -> &Path {
        self.datadir
            .as_deref()
            .unwrap_or_else(|| Path::new("/embassy-data"))
    }
    pub async fn db(&self, account: &AccountInfo) -> Result<PatchDb, Error> {
        let db_path = self.datadir().join("main").join("embassy.db");
        let db = PatchDb::open(&db_path)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, db_path.display().to_string()))?;
        if !db.exists(&<JsonPointer>::default()).await {
            db.put(&<JsonPointer>::default(), &Database::init(account))
                .await?;
        }
        Ok(db)
    }
    #[instrument(skip_all)]
    pub async fn secret_store(&self) -> Result<PgPool, Error> {
        init_postgres(self.datadir()).await?;
        let secret_store =
            PgPool::connect_with(PgConnectOptions::new().database("secrets").username("root"))
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
    pub os_partitions: OsPartitionInfo,
    pub wifi_interface: Option<String>,
    pub ethernet_interface: String,
    pub datadir: PathBuf,
    pub disk_guid: Arc<String>,
    pub db: PatchDb,
    pub secret_store: PgPool,
    pub account: RwLock<AccountInfo>,
    pub docker: Docker,
    pub net_controller: Arc<NetController>,
    pub managers: ManagerMap,
    pub metrics_cache: RwLock<Option<crate::system::Metrics>>,
    pub shutdown: broadcast::Sender<Option<Shutdown>>,
    pub tor_socks: SocketAddr,
    pub notification_manager: NotificationManager,
    pub open_authed_websockets: Mutex<BTreeMap<HashSessionToken, Vec<oneshot::Sender<()>>>>,
    pub rpc_stream_continuations: Mutex<BTreeMap<RequestGuid, RpcContinuation>>,
    pub wifi_manager: Option<Arc<RwLock<WpaCli>>>,
    pub current_secret: Arc<Jwk>,
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

#[derive(Clone)]
pub struct RpcContext(Arc<RpcContextSeed>);
impl RpcContext {
    #[instrument(skip_all)]
    pub async fn init<P: AsRef<Path> + Send + 'static>(
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
        tracing::info!("Opened Pg DB");
        let account = AccountInfo::load(&secret_store).await?;
        let db = base.db(&account).await?;
        tracing::info!("Opened PatchDB");
        let mut docker = Docker::connect_with_unix_defaults()?;
        docker.set_timeout(Duration::from_secs(600));
        tracing::info!("Connected to Docker");
        let net_controller = Arc::new(
            NetController::init(
                base.tor_control
                    .unwrap_or(SocketAddr::from(([127, 0, 0, 1], 9051))),
                tor_proxy,
                base.dns_bind
                    .as_ref()
                    .map(|v| v.as_slice())
                    .unwrap_or(&[SocketAddr::from(([127, 0, 0, 1], 53))]),
                SslManager::new(&account)?,
                &account.hostname,
                &account.key,
            )
            .await?,
        );
        tracing::info!("Initialized Net Controller");
        let managers = ManagerMap::default();
        let metrics_cache = RwLock::new(None);
        let notification_manager = NotificationManager::new(secret_store.clone());
        tracing::info!("Initialized Notification Manager");
        let seed = Arc::new(RpcContextSeed {
            is_closed: AtomicBool::new(false),
            datadir: base.datadir().to_path_buf(),
            os_partitions: base.os_partitions,
            wifi_interface: base.wifi_interface.clone(),
            ethernet_interface: base.ethernet_interface,
            disk_guid,
            db,
            secret_store,
            account: RwLock::new(account),
            docker,
            net_controller,
            managers,
            metrics_cache,
            shutdown,
            tor_socks: tor_proxy,
            notification_manager,
            open_authed_websockets: Mutex::new(BTreeMap::new()),
            rpc_stream_continuations: Mutex::new(BTreeMap::new()),
            wifi_manager: base
                .wifi_interface
                .map(|i| Arc::new(RwLock::new(WpaCli::init(i)))),
            current_secret: Arc::new(
                Jwk::generate_ec_key(josekit::jwk::alg::ec::EcCurve::P256).map_err(|e| {
                    tracing::debug!("{:?}", e);
                    tracing::error!("Couldn't generate ec key");
                    Error::new(
                        color_eyre::eyre::eyre!("Couldn't generate ec key"),
                        crate::ErrorKind::Unknown,
                    )
                })?,
            ),
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

    #[instrument(skip_all)]
    pub async fn shutdown(self) -> Result<(), Error> {
        self.managers.empty().await?;
        self.secret_store.close().await;
        self.is_closed.store(true, Ordering::SeqCst);
        tracing::info!("RPC Context is shutdown");
        // TODO: shutdown http servers
        Ok(())
    }

    #[instrument(skip_all)]
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
                        for (volume_id, volume_info) in &*manifest.volumes {
                            let tmp_path = to_tmp_path(volume_info.path_for(
                                &self.datadir,
                                &package_id,
                                &manifest.version,
                                &volume_id,
                            ))
                            .with_kind(ErrorKind::Filesystem)?;
                            if tokio::fs::metadata(&tmp_path).await.is_ok() {
                                tokio::fs::remove_dir_all(&tmp_path).await?;
                            }
                        }
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
        let mut current_dependents = BTreeMap::new();
        for (package_id, package) in receipts.packages.get(&mut db).await?.0 {
            for (k, v) in package
                .into_installed()
                .into_iter()
                .flat_map(|i| i.current_dependencies.0)
            {
                let mut entry: BTreeMap<_, _> = current_dependents.remove(&k).unwrap_or_default();
                entry.insert(package_id.clone(), v);
                current_dependents.insert(k, entry);
            }
        }
        for (package_id, current_dependents) in current_dependents {
            if let Some(deps) = crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(&package_id)
                .and_then(|pde| pde.installed())
                .map::<_, CurrentDependents>(|i| i.current_dependents())
                .check(&mut db)
                .await?
            {
                deps.put(&mut db, &CurrentDependents(current_dependents))
                    .await?;
            }
        }
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn clean_continuations(&self) {
        let mut continuations = self.rpc_stream_continuations.lock().await;
        let mut to_remove = Vec::new();
        for (guid, cont) in &*continuations {
            if cont.is_timed_out() {
                to_remove.push(guid.clone());
            }
        }
        for guid in to_remove {
            continuations.remove(&guid);
        }
    }

    #[instrument(skip_all)]
    pub async fn add_continuation(&self, guid: RequestGuid, handler: RpcContinuation) {
        self.clean_continuations().await;
        self.rpc_stream_continuations
            .lock()
            .await
            .insert(guid, handler);
    }

    pub async fn get_continuation_handler(&self, guid: &RequestGuid) -> Option<RestHandler> {
        let mut continuations = self.rpc_stream_continuations.lock().await;
        if let Some(cont) = continuations.remove(guid) {
            cont.into_handler().await
        } else {
            None
        }
    }

    pub async fn get_ws_continuation_handler(&self, guid: &RequestGuid) -> Option<RestHandler> {
        let continuations = self.rpc_stream_continuations.lock().await;
        if matches!(continuations.get(guid), Some(RpcContinuation::WebSocket(_))) {
            drop(continuations);
            self.get_continuation_handler(guid).await
        } else {
            None
        }
    }

    pub async fn get_rest_continuation_handler(&self, guid: &RequestGuid) -> Option<RestHandler> {
        let continuations = self.rpc_stream_continuations.lock().await;
        if matches!(continuations.get(guid), Some(RpcContinuation::Rest(_))) {
            drop(continuations);
            self.get_continuation_handler(guid).await
        } else {
            None
        }
    }
}
impl AsRef<Jwk> for RpcContext {
    fn as_ref(&self) -> &Jwk {
        &*CURRENT_SECRET
    }
}
impl Context for RpcContext {}
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
