use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use josekit::jwk::Jwk;
use patch_db::json_ptr::JsonPointer;
use patch_db::PatchDb;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::Context;
use serde::{Deserialize, Serialize};
use sqlx::postgres::PgConnectOptions;
use sqlx::PgPool;
use tokio::sync::broadcast::Sender;
use tokio::sync::RwLock;
use tracing::instrument;

use crate::db::model::Database;
use crate::disk::OsPartitionInfo;
use crate::init::{init_postgres, pgloader};
use crate::net::tor::os_key;
use crate::setup::{password_hash, SetupStatus};
use crate::util::config::load_config_from_paths;
use crate::{Error, ResultExt};

lazy_static::lazy_static! {
    pub static ref CURRENT_SECRET: Jwk = Jwk::generate_ec_key(josekit::jwk::alg::ec::EcCurve::P256).unwrap_or_else(|e| {
        tracing::debug!("{:?}", e);
        tracing::error!("Couldn't generate ec key");
        panic!("Couldn't generate ec key")
    });
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct SetupResult {
    pub tor_address: String,
    pub lan_address: String,
    pub root_ca: String,
}

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct SetupContextConfig {
    pub os_partitions: OsPartitionInfo,
    pub migration_batch_rows: Option<usize>,
    pub migration_prefetch_rows: Option<usize>,
    pub datadir: Option<PathBuf>,
}
impl SetupContextConfig {
    #[instrument(skip(path))]
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
}

pub struct SetupContextSeed {
    pub os_partitions: OsPartitionInfo,
    pub config_path: Option<PathBuf>,
    pub migration_batch_rows: usize,
    pub migration_prefetch_rows: usize,
    pub shutdown: Sender<()>,
    pub datadir: PathBuf,
    pub selected_v2_drive: RwLock<Option<PathBuf>>,
    pub cached_product_key: RwLock<Option<Arc<String>>>,
    pub setup_status: RwLock<Option<Result<SetupStatus, RpcError>>>,
    pub setup_result: RwLock<Option<(Arc<String>, SetupResult)>>,
}

impl AsRef<Jwk> for SetupContextSeed {
    fn as_ref(&self) -> &Jwk {
        &*CURRENT_SECRET
    }
}

#[derive(Clone)]
pub struct SetupContext(Arc<SetupContextSeed>);
impl SetupContext {
    #[instrument(skip(path))]
    pub async fn init<P: AsRef<Path> + Send + 'static>(path: Option<P>) -> Result<Self, Error> {
        let cfg = SetupContextConfig::load(path.as_ref().map(|p| p.as_ref().to_owned())).await?;
        let (shutdown, _) = tokio::sync::broadcast::channel(1);
        let datadir = cfg.datadir().to_owned();
        Ok(Self(Arc::new(SetupContextSeed {
            os_partitions: cfg.os_partitions,
            config_path: path.as_ref().map(|p| p.as_ref().to_owned()),
            migration_batch_rows: cfg.migration_batch_rows.unwrap_or(25000),
            migration_prefetch_rows: cfg.migration_prefetch_rows.unwrap_or(100_000),
            shutdown,
            datadir,
            selected_v2_drive: RwLock::new(None),
            cached_product_key: RwLock::new(None),
            setup_status: RwLock::new(None),
            setup_result: RwLock::new(None),
        })))
    }
    #[instrument(skip(self))]
    pub async fn db(&self, secret_store: &PgPool) -> Result<PatchDb, Error> {
        let db_path = self.datadir.join("main").join("embassy.db");
        let db = PatchDb::open(&db_path)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, db_path.display().to_string()))?;
        if !db.exists(&<JsonPointer>::default()).await {
            db.put(
                &<JsonPointer>::default(),
                &Database::init(
                    &os_key(&mut secret_store.acquire().await?).await?,
                    password_hash(&mut secret_store.acquire().await?).await?,
                ),
            )
            .await?;
        }
        Ok(db)
    }
    #[instrument(skip(self))]
    pub async fn secret_store(&self) -> Result<PgPool, Error> {
        init_postgres(&self.datadir).await?;
        let secret_store =
            PgPool::connect_with(PgConnectOptions::new().database("secrets").username("root"))
                .await?;
        sqlx::migrate!()
            .run(&secret_store)
            .await
            .with_kind(crate::ErrorKind::Database)?;
        let old_db_path = self.datadir.join("main/secrets.db");
        if tokio::fs::metadata(&old_db_path).await.is_ok() {
            pgloader(
                &old_db_path,
                self.migration_batch_rows,
                self.migration_prefetch_rows,
            )
            .await?;
        }
        Ok(secret_store)
    }
}

impl Context for SetupContext {}
impl Deref for SetupContext {
    type Target = SetupContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
