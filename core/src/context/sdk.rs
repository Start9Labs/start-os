use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::ArgMatches;
use color_eyre::eyre::eyre;
use rpc_toolkit::Context;
use serde::Deserialize;
use tracing::instrument;

use crate::prelude::*;

#[derive(Debug)]
pub struct SdkContextSeed {
    pub developer_key_path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct SdkContext(Arc<SdkContextSeed>);
impl SdkContext {
    /// BLOCKING
    #[instrument(skip_all)]
    pub fn init(config: ) -> Result<Self, crate::Error> {
        let local_config_path = local_config_path();
        let base: SdkContextConfig = load_config_from_paths(
            matches
                .values_of("config")
                .into_iter()
                .flatten()
                .map(|p| Path::new(p))
                .chain(local_config_path.as_deref().into_iter())
                .chain(std::iter::once(Path::new(crate::util::config::CONFIG_PATH))),
        )?;
        Ok(SdkContext(Arc::new(SdkContextSeed {
            developer_key_path: base.developer_key_path.unwrap_or_else(|| {
                local_config_path
                    .as_deref()
                    .unwrap_or_else(|| Path::new(crate::util::config::CONFIG_PATH))
                    .parent()
                    .unwrap_or(Path::new("/"))
                    .join("developer.key.pem")
            }),
        })))
    }
    
}
impl std::ops::Deref for SdkContext {
    type Target = SdkContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl Context for SdkContext {}
