use std::fs::File;
use std::io::BufReader;
use std::net::{Ipv4Addr, SocketAddr};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::ArgMatches;
use color_eyre::eyre::eyre;
use cookie_store::CookieStore;
use reqwest::Proxy;
use reqwest_cookie_store::CookieStoreMutex;
use rpc_toolkit::reqwest::{Client, Url};
use rpc_toolkit::url::Host;
use rpc_toolkit::Context;
use serde::Deserialize;
use tracing::instrument;

use crate::util::config::{load_config_from_paths, local_config_path};
use crate::ResultExt;

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct CliContextConfig {
    pub bind_rpc: Option<SocketAddr>,
    pub host: Option<Url>,
    #[serde(deserialize_with = "crate::util::serde::deserialize_from_str_opt")]
    pub proxy: Option<Url>,
    pub cookie_path: Option<PathBuf>,
}

#[derive(Debug)]
pub struct CliContextSeed {
    pub base_url: Url,
    pub rpc_url: Url,
    pub client: Client,
    pub cookie_store: Arc<CookieStoreMutex>,
    pub cookie_path: PathBuf,
}
impl Drop for CliContextSeed {
    fn drop(&mut self) {
        let tmp = format!("{}.tmp", self.cookie_path.display());
        let mut writer = fd_lock_rs::FdLock::lock(
            File::create(&tmp).unwrap(),
            fd_lock_rs::LockType::Exclusive,
            true,
        )
        .unwrap();
        let store = self.cookie_store.lock().unwrap();
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
    #[instrument(skip(matches))]
    pub fn init(matches: &ArgMatches) -> Result<Self, crate::Error> {
        let local_config_path = local_config_path();
        let base: CliContextConfig = load_config_from_paths(
            matches
                .values_of("config")
                .into_iter()
                .flatten()
                .map(|p| Path::new(p))
                .chain(local_config_path.as_deref().into_iter())
                .chain(std::iter::once(Path::new(crate::util::config::CONFIG_PATH))),
        )?;
        let mut url = if let Some(host) = matches.value_of("host") {
            host.parse()?
        } else if let Some(host) = base.host {
            host
        } else {
            format!(
                "http://{}",
                base.bind_rpc.unwrap_or(([127, 0, 0, 1], 80).into())
            )
            .parse()?
        };
        let proxy = if let Some(proxy) = matches.value_of("proxy") {
            Some(proxy.parse()?)
        } else {
            base.proxy
        };

        let cookie_path = base.cookie_path.unwrap_or_else(|| {
            local_config_path
                .as_deref()
                .unwrap_or_else(|| Path::new(crate::util::config::CONFIG_PATH))
                .parent()
                .unwrap_or(Path::new("/"))
                .join(".cookies.json")
        });
        let cookie_store = Arc::new(CookieStoreMutex::new(if cookie_path.exists() {
            CookieStore::load_json(BufReader::new(File::open(&cookie_path)?))
                .map_err(|e| eyre!("{}", e))
                .with_kind(crate::ErrorKind::Deserialization)?
        } else {
            CookieStore::default()
        }));
        Ok(CliContext(Arc::new(CliContextSeed {
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
                if let Some(proxy) = proxy {
                    builder =
                        builder.proxy(Proxy::all(proxy).with_kind(crate::ErrorKind::ParseUrl)?)
                }
                builder.build().expect("cannot fail")
            },
            cookie_store,
            cookie_path,
        })))
    }
}
impl std::ops::Deref for CliContext {
    type Target = CliContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl Context for CliContext {
    fn protocol(&self) -> &str {
        self.0.base_url.scheme()
    }
    fn host(&self) -> Host<&str> {
        self.0.base_url.host().unwrap_or(DEFAULT_HOST)
    }
    fn port(&self) -> u16 {
        self.0.base_url.port().unwrap_or(DEFAULT_PORT)
    }
    fn path(&self) -> &str {
        self.0.rpc_url.path()
    }
    fn url(&self) -> Url {
        self.0.rpc_url.clone()
    }
    fn client(&self) -> &Client {
        &self.0.client
    }
}
