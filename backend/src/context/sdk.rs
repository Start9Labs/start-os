use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::ArgMatches;
use color_eyre::eyre::eyre;
use rpc_toolkit::Context;
use serde::Deserialize;
use tracing::instrument;

use crate::util::config::{load_config_from_paths, local_config_path};
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
    #[instrument(skip(matches))]
    pub fn init(matches: &ArgMatches) -> Result<Self, crate::Error> {
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
    /// BLOCKING
    #[instrument]
    pub fn developer_key(&self) -> Result<ed25519_dalek::Keypair, Error> {
        if !self.developer_key_path.exists() {
            return Err(Error::new(eyre!("Developer Key does not exist! Please run `embassy-sdk init` before running this command."), crate::ErrorKind::Uninitialized));
        }
        let pair = <ed25519::KeypairBytes as ed25519::pkcs8::DecodePrivateKey>::from_pkcs8_pem(
            &std::fs::read_to_string(&self.developer_key_path)?,
        )
        .with_kind(crate::ErrorKind::Pem)?;
        let secret = ed25519_dalek::SecretKey::from_bytes(&pair.secret_key[..])?;
        let public = if let Some(public) = pair.public_key {
            ed25519_dalek::PublicKey::from_bytes(&public[..])?
        } else {
            (&secret).into()
        };
        Ok(ed25519_dalek::Keypair { secret, public })
    }
}
impl std::ops::Deref for SdkContext {
    type Target = SdkContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl Context for SdkContext {}
