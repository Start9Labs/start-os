use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use futures::{Future, StreamExt};
use helpers::NonDetachingJoinHandle;
use imbl_value::InternedString;
use josekit::jwk::Jwk;
use patch_db::PatchDb;
use rpc_toolkit::Context;
use serde::{Deserialize, Serialize};
use sqlx::postgres::PgConnectOptions;
use sqlx::PgPool;
use tokio::sync::broadcast::Sender;
use tokio::sync::OnceCell;
use tracing::instrument;
use ts_rs::TS;

use crate::account::AccountInfo;
use crate::context::config::ServerConfig;
use crate::context::RpcContext;
use crate::disk::OsPartitionInfo;
use crate::init::init_postgres;
use crate::prelude::*;
use crate::progress::FullProgressTracker;
use crate::rpc_continuations::{Guid, RpcContinuation, RpcContinuations};
use crate::setup::SetupProgress;
use crate::util::net::WebSocketExt;

lazy_static::lazy_static! {
    pub static ref CURRENT_SECRET: Jwk = Jwk::generate_ec_key(josekit::jwk::alg::ec::EcCurve::P256).unwrap_or_else(|e| {
        tracing::debug!("{:?}", e);
        tracing::error!("Couldn't generate ec key");
        panic!("Couldn't generate ec key")
    });
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetupResult {
    pub tor_address: String,
    #[ts(type = "string")]
    pub lan_address: InternedString,
    pub root_ca: String,
}
impl TryFrom<&AccountInfo> for SetupResult {
    type Error = Error;
    fn try_from(value: &AccountInfo) -> Result<Self, Self::Error> {
        Ok(Self {
            tor_address: format!("https://{}", value.tor_key.public().get_onion_address()),
            lan_address: value.hostname.lan_address(),
            root_ca: String::from_utf8(value.root_ca_cert.to_pem()?)?,
        })
    }
}

pub struct SetupContextSeed {
    pub config: ServerConfig,
    pub os_partitions: OsPartitionInfo,
    pub disable_encryption: bool,
    pub progress: FullProgressTracker,
    pub task: OnceCell<NonDetachingJoinHandle<()>>,
    pub result: OnceCell<Result<(SetupResult, RpcContext), Error>>,
    pub shutdown: Sender<()>,
    pub datadir: PathBuf,
    pub rpc_continuations: RpcContinuations,
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
            os_partitions: config.os_partitions.clone().ok_or_else(|| {
                Error::new(
                    eyre!("missing required configuration: `os-partitions`"),
                    ErrorKind::NotFound,
                )
            })?,
            disable_encryption: config.disable_encryption.unwrap_or(false),
            progress: FullProgressTracker::new(),
            task: OnceCell::new(),
            result: OnceCell::new(),
            shutdown,
            datadir,
            rpc_continuations: RpcContinuations::new(),
        })))
    }
    #[instrument(skip_all)]
    pub async fn db(&self) -> Result<PatchDb, Error> {
        let db_path = self.datadir.join("main").join("embassy.db");
        let db = PatchDb::open(&db_path)
            .await
            .with_ctx(|_| (crate::ErrorKind::Filesystem, db_path.display().to_string()))?;
        Ok(db)
    }

    pub fn run_setup<F, Fut>(&self, f: F) -> Result<(), Error>
    where
        F: FnOnce() -> Fut + Send + 'static,
        Fut: Future<Output = Result<(SetupResult, RpcContext), Error>> + Send,
    {
        let local_ctx = self.clone();
        self.task
            .set(
                tokio::spawn(async move {
                    local_ctx
                        .result
                        .get_or_init(|| async {
                            match f().await {
                                Ok(res) => {
                                    tracing::info!("Setup complete!");
                                    Ok(res)
                                }
                                Err(e) => {
                                    tracing::error!("Setup failed: {e}");
                                    tracing::debug!("{e:?}");
                                    Err(e)
                                }
                            }
                        })
                        .await;
                    local_ctx.progress.complete();
                })
                .into(),
            )
            .map_err(|_| {
                if self.result.initialized() {
                    Error::new(eyre!("Setup already complete"), ErrorKind::InvalidRequest)
                } else {
                    Error::new(
                        eyre!("Setup already in progress"),
                        ErrorKind::InvalidRequest,
                    )
                }
            })?;
        Ok(())
    }

    pub async fn progress(&self) -> SetupProgress {
        use axum::extract::ws;

        let guid = Guid::new();
        let progress_tracker = self.progress.clone();
        let progress = progress_tracker.snapshot();
        self.rpc_continuations
            .add(
                guid.clone(),
                RpcContinuation::ws(
                    |mut ws| async move {
                        if let Err(e) = async {
                            let mut stream =
                                progress_tracker.stream(Some(Duration::from_millis(100)));
                            while let Some(progress) = stream.next().await {
                                ws.send(ws::Message::Text(
                                    serde_json::to_string(&progress)
                                        .with_kind(ErrorKind::Serialization)?,
                                ))
                                .await
                                .with_kind(ErrorKind::Network)?;
                                if progress.overall.is_complete() {
                                    break;
                                }
                            }

                            ws.normal_close("complete").await?;

                            Ok::<_, Error>(())
                        }
                        .await
                        {
                            tracing::error!("Error in setup progress websocket: {e}");
                            tracing::debug!("{e:?}");
                        }
                    },
                    Duration::from_secs(30),
                ),
            )
            .await;

        SetupProgress { progress, guid }
    }
}

impl AsRef<Jwk> for SetupContext {
    fn as_ref(&self) -> &Jwk {
        &*CURRENT_SECRET
    }
}

impl AsRef<RpcContinuations> for SetupContext {
    fn as_ref(&self) -> &RpcContinuations {
        &self.rpc_continuations
    }
}

impl Context for SetupContext {}
impl Deref for SetupContext {
    type Target = SetupContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
