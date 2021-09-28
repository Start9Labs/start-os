use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::anyhow;
use clap::ArgMatches;
use rpc_toolkit::Context;
use serde::Deserialize;

use crate::{Error, ResultExt};

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct SdkContextConfig {
    pub developer_key_path: Option<PathBuf>,
}

#[derive(Debug)]
pub struct SdkContextSeed {
    pub developer_key_path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct SdkContext(Arc<SdkContextSeed>);
impl SdkContext {
    /// BLOCKING
    pub fn init(matches: &ArgMatches) -> Result<Self, crate::Error> {
        let cfg_path = Path::new(matches.value_of("config").unwrap_or(crate::CONFIG_PATH));
        let base = if cfg_path.exists() {
            serde_yaml::from_reader(
                File::open(cfg_path)
                    .with_ctx(|_| (crate::ErrorKind::Filesystem, cfg_path.display().to_string()))?,
            )
            .with_kind(crate::ErrorKind::Deserialization)?
        } else {
            SdkContextConfig::default()
        };
        Ok(SdkContext(Arc::new(SdkContextSeed {
            developer_key_path: base.developer_key_path.unwrap_or_else(|| {
                cfg_path
                    .parent()
                    .unwrap_or(Path::new("/"))
                    .join(".developer_key")
            }),
        })))
    }
    /// BLOCKING
    pub fn developer_key(&self) -> Result<ed25519_dalek::Keypair, Error> {
        if !self.developer_key_path.exists() {
            return Err(Error::new(anyhow!("Developer Key does not exist! Please run `embassy-sdk init` before running this command."), crate::ErrorKind::Uninitialized));
        }
        let mut keypair_buf = [0; ed25519_dalek::KEYPAIR_LENGTH];
        File::open(&self.developer_key_path)?.read_exact(&mut keypair_buf)?;
        Ok(ed25519_dalek::Keypair::from_bytes(&keypair_buf)?)
    }
}
impl std::ops::Deref for SdkContext {
    type Target = SdkContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl Context for SdkContext {}
