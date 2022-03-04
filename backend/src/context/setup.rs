use std::net::{IpAddr, SocketAddr};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use patch_db::json_ptr::JsonPointer;
use patch_db::PatchDb;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::Context;
use serde::{Deserialize, Serialize};
use sqlx::sqlite::SqliteConnectOptions;
use sqlx::SqlitePool;
use tokio::fs::File;
use tokio::sync::broadcast::Sender;
use tokio::sync::RwLock;
use tracing::instrument;
use url::Host;

use crate::db::model::Database;
use crate::hostname::{derive_hostname, derive_id, get_product_key};
use crate::net::tor::os_key;
use crate::setup::{password_hash, RecoveryStatus};
use crate::util::io::from_toml_async_reader;
use crate::util::AsyncFileExt;
use crate::{Error, ResultExt};

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
    pub bind_rpc: Option<SocketAddr>,
    pub datadir: Option<PathBuf>,
}
impl SetupContextConfig {
    #[instrument(skip(path))]
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
}

pub struct SetupContextSeed {
    pub config_path: Option<PathBuf>,
    pub bind_rpc: SocketAddr,
    pub shutdown: Sender<()>,
    pub datadir: PathBuf,
    pub selected_v2_drive: RwLock<Option<PathBuf>>,
    pub cached_product_key: RwLock<Option<Arc<String>>>,
    pub recovery_status: RwLock<Option<Result<RecoveryStatus, RpcError>>>,
    pub setup_result: RwLock<Option<(Arc<String>, SetupResult)>>,
}

#[derive(Clone)]
pub struct SetupContext(Arc<SetupContextSeed>);
impl SetupContext {
    #[instrument(skip(path))]
    pub async fn init<P: AsRef<Path>>(path: Option<P>) -> Result<Self, Error> {
        let cfg = SetupContextConfig::load(path.as_ref()).await?;
        let (shutdown, _) = tokio::sync::broadcast::channel(1);
        let datadir = cfg.datadir().to_owned();
        Ok(Self(Arc::new(SetupContextSeed {
            config_path: path.as_ref().map(|p| p.as_ref().to_owned()),
            bind_rpc: cfg.bind_rpc.unwrap_or(([127, 0, 0, 1], 5959).into()),
            shutdown,
            datadir,
            selected_v2_drive: RwLock::new(None),
            cached_product_key: RwLock::new(None),
            recovery_status: RwLock::new(None),
            setup_result: RwLock::new(None),
        })))
    }
    #[instrument(skip(self))]
    pub async fn db(&self, secret_store: &SqlitePool) -> Result<PatchDb, Error> {
        let db_path = self.datadir.join("main").join("embassy.db");
        let db = PatchDb::open(&db_path)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, db_path.display().to_string()))?;
        if !db.exists(&<JsonPointer>::default()).await? {
            let pkey = self.product_key().await?;
            let sid = derive_id(&*pkey);
            let hostname = derive_hostname(&sid);
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
    #[instrument(skip(self))]
    pub async fn secret_store(&self) -> Result<SqlitePool, Error> {
        let secret_store = SqlitePool::connect_with(
            SqliteConnectOptions::new()
                .filename(self.datadir.join("main").join("secrets.db"))
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
    #[instrument(skip(self))]
    pub async fn product_key(&self) -> Result<Arc<String>, Error> {
        Ok(
            if let Some(k) = {
                let guard = self.cached_product_key.read().await;
                let res = guard.clone();
                drop(guard);
                res
            } {
                k
            } else {
                let k = Arc::new(get_product_key().await?);
                *self.cached_product_key.write().await = Some(k.clone());
                k
            },
        )
    }
}

impl Context for SetupContext {
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
impl Deref for SetupContext {
    type Target = SetupContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
