use std::ops::Deref;
use std::path::PathBuf;
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

use crate::account::AccountInfo;
use crate::context::config::ServerConfig;
use crate::db::model::Database;
use crate::disk::OsPartitionInfo;
use crate::init::init_postgres;
use crate::setup::SetupStatus;
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

pub struct SetupContextSeed {
    pub config: ServerConfig,
    pub os_partitions: OsPartitionInfo,
    pub disable_encryption: bool,
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
    #[instrument(skip_all)]
    pub fn init(config: &ServerConfig) -> Result<Self, Error> {
        let (shutdown, _) = tokio::sync::broadcast::channel(1);
        let datadir = config.datadir().to_owned();
        Ok(Self(Arc::new(SetupContextSeed {
            config: config.clone(),
            os_partitions: config.os_partitions,
            disable_encryption: config.disable_encryption,
            shutdown,
            datadir,
            selected_v2_drive: RwLock::new(None),
            cached_product_key: RwLock::new(None),
            setup_status: RwLock::new(None),
            setup_result: RwLock::new(None),
        })))
    }
    #[instrument(skip_all)]
    pub async fn db(&self, account: &AccountInfo) -> Result<PatchDb, Error> {
        let db_path = self.datadir.join("main").join("embassy.db");
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
        init_postgres(&self.datadir).await?;
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

impl Context for SetupContext {}
impl Deref for SetupContext {
    type Target = SetupContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
