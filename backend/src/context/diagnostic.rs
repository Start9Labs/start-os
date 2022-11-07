use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::Context;
use serde::Deserialize;
use tokio::sync::broadcast::Sender;
use tracing::instrument;

use crate::shutdown::Shutdown;
use crate::util::config::load_config_from_paths;
use crate::Error;

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct DiagnosticContextConfig {
    pub ethernet_interface: String,
    pub datadir: Option<PathBuf>,
}
impl DiagnosticContextConfig {
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

pub struct DiagnosticContextSeed {
    pub ethernet_interface: String,
    pub datadir: PathBuf,
    pub shutdown: Sender<Option<Shutdown>>,
    pub error: Arc<RpcError>,
    pub disk_guid: Option<Arc<String>>,
}

#[derive(Clone)]
pub struct DiagnosticContext(Arc<DiagnosticContextSeed>);
impl DiagnosticContext {
    #[instrument(skip(path))]
    pub async fn init<P: AsRef<Path> + Send + 'static>(
        path: Option<P>,
        disk_guid: Option<Arc<String>>,
        error: Error,
    ) -> Result<Self, Error> {
        tracing::error!("Error: {}: Starting diagnostic UI", error);
        tracing::debug!("{:?}", error);

        let cfg = DiagnosticContextConfig::load(path).await?;

        let (shutdown, _) = tokio::sync::broadcast::channel(1);

        Ok(Self(Arc::new(DiagnosticContextSeed {
            ethernet_interface: cfg.ethernet_interface.clone(),
            datadir: cfg.datadir().to_owned(),
            shutdown,
            disk_guid,
            error: Arc::new(error.into()),
        })))
    }
}

impl Context for DiagnosticContext {}
impl Deref for DiagnosticContext {
    type Target = DiagnosticContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
