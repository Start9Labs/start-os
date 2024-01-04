use std::fs::File;
use std::io::BufReader;
use std::net::Ipv4Addr;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::Parser;
use color_eyre::eyre::eyre;
use cookie_store::{CookieStore, RawCookie};
use josekit::jwk::Jwk;
use reqwest::Proxy;
use reqwest_cookie_store::CookieStoreMutex;
use rpc_toolkit::reqwest::{Client, Url};
use rpc_toolkit::url::Host;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{call_remote_http, CallRemote, Context};
use tokio::runtime::Runtime;
use tokio::sync::OnceCell;
use tracing::instrument;

use super::setup::CURRENT_SECRET;
use crate::context::config::{local_config_path, ClientConfig};
use crate::middleware::auth::LOCAL_AUTH_COOKIE_PATH;
use crate::prelude::*;

#[derive(Debug)]
pub struct CliContextSeed {
    pub runtime: OnceCell<Runtime>,
    pub base_url: Url,
    pub rpc_url: Url,
    pub client: Client,
    pub cookie_store: Arc<CookieStoreMutex>,
    pub cookie_path: PathBuf,
    pub developer_key_path: PathBuf,
}
impl Drop for CliContextSeed {
    fn drop(&mut self) {
        let tmp = format!("{}.tmp", self.cookie_path.display());
        let parent_dir = self.cookie_path.parent().unwrap_or(Path::new("/"));
        if !parent_dir.exists() {
            std::fs::create_dir_all(&parent_dir).unwrap();
        }
        let mut writer = fd_lock_rs::FdLock::lock(
            File::create(&tmp).unwrap(),
            fd_lock_rs::LockType::Exclusive,
            true,
        )
        .unwrap();
        let mut store = self.cookie_store.lock().unwrap();
        store.remove("localhost", "", "local");
        store.save_json(&mut *writer).unwrap();
        writer.sync_all().unwrap();
        std::fs::rename(tmp, &self.cookie_path).unwrap();
    }
}

const DEFAULT_HOST: Host<&'static str> = Host::Ipv4(Ipv4Addr::new(127, 0, 0, 1));
const DEFAULT_PORT: u16 = 5959;

#[derive(Debug, Clone)]
pub struct CliContext(Arc<CliContextSeed>);
impl CliContext {
    /// BLOCKING
    #[instrument(skip_all)]
    pub fn init(config: ClientConfig) -> Result<Self, Error> {
        let mut url = if let Some(host) = config.host {
            host.parse()?
        } else {
            "http://localhost".parse()?
        };

        let cookie_path = config.cookie_path.unwrap_or_else(|| {
            local_config_path()
                .as_deref()
                .unwrap_or_else(|| Path::new(super::config::CONFIG_PATH))
                .parent()
                .unwrap_or(Path::new("/"))
                .join(".cookies.json")
        });
        let cookie_store = Arc::new(CookieStoreMutex::new({
            let mut store = if cookie_path.exists() {
                CookieStore::load_json(BufReader::new(File::open(&cookie_path)?))
                    .map_err(|e| eyre!("{}", e))
                    .with_kind(crate::ErrorKind::Deserialization)?
            } else {
                CookieStore::default()
            };
            if let Ok(local) = std::fs::read_to_string(LOCAL_AUTH_COOKIE_PATH) {
                store
                    .insert_raw(
                        &RawCookie::new("local", local),
                        &"http://localhost".parse()?,
                    )
                    .with_kind(crate::ErrorKind::Network)?;
            }
            store
        }));

        Ok(CliContext(Arc::new(CliContextSeed {
            runtime: OnceCell::new(),
            base_url: url.clone(),
            rpc_url: {
                url.path_segments_mut()
                    .map_err(|_| eyre!("Url cannot be base"))
                    .with_kind(crate::ErrorKind::ParseUrl)?
                    .push("rpc")
                    .push("v1");
                url
            },
            client: {
                let mut builder = Client::builder().cookie_provider(cookie_store.clone());
                if let Some(proxy) = config.proxy {
                    builder =
                        builder.proxy(Proxy::all(proxy).with_kind(crate::ErrorKind::ParseUrl)?)
                }
                builder.build().expect("cannot fail")
            },
            cookie_store,
            cookie_path,
            developer_key_path: config.developer_key_path.unwrap_or_else(|| {
                local_config_path()
                    .as_deref()
                    .unwrap_or_else(|| Path::new(super::config::CONFIG_PATH))
                    .parent()
                    .unwrap_or(Path::new("/"))
                    .join("developer.key.pem")
            }),
        })))
    }

    /// BLOCKING
    #[instrument(skip_all)]
    pub fn developer_key(&self) -> Result<ed25519_dalek::SigningKey, Error> {
        if !self.developer_key_path.exists() {
            return Err(Error::new(eyre!("Developer Key does not exist! Please run `start-sdk init` before running this command."), crate::ErrorKind::Uninitialized));
        }
        let pair = <ed25519::KeypairBytes as ed25519::pkcs8::DecodePrivateKey>::from_pkcs8_pem(
            &std::fs::read_to_string(&self.developer_key_path)?,
        )
        .with_kind(crate::ErrorKind::Pem)?;
        let secret = ed25519_dalek::SecretKey::try_from(&pair.secret_key[..]).map_err(|_| {
            Error::new(
                eyre!("pkcs8 key is of incorrect length"),
                ErrorKind::OpenSsl,
            )
        })?;
        Ok(secret.into())
    }
}
impl AsRef<Jwk> for CliContext {
    fn as_ref(&self) -> &Jwk {
        &*CURRENT_SECRET
    }
}
impl std::ops::Deref for CliContext {
    type Target = CliContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl Context for CliContext {
    fn runtime(&self) -> tokio::runtime::Handle {
        if let Some(rt) = self.runtime.get() {
            rt
        } else {
            self.runtime
                .set(
                    tokio::runtime::Builder::new_current_thread()
                        .enable_all()
                        .build()
                        .unwrap(),
                )
                .unwrap();
            self.runtime.get().unwrap()
        }
    }
}
#[async_trait::async_trait]
impl CallRemote for CliContext {
    async fn call_remote(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        call_remote_http(&self.client, &self.url, method, params).await
    }
}
