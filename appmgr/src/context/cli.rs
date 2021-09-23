use std::fs::File;
use std::io::{BufReader, Read};
use std::net::{Ipv4Addr, SocketAddr};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::anyhow;
use clap::ArgMatches;
use cookie_store::CookieStore;
use reqwest::Proxy;
use reqwest_cookie_store::CookieStoreMutex;
use rpc_toolkit::reqwest::{Client, Url};
use rpc_toolkit::url::Host;
use rpc_toolkit::Context;
use serde::Deserialize;

use super::rpc::RpcContextConfig;
use crate::{Error, ResultExt};

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct CliContextConfig {
    pub bind_rpc: Option<SocketAddr>,
    pub host: Option<Url>,
    #[serde(deserialize_with = "crate::util::deserialize_from_str_opt")]
    pub proxy: Option<Url>,
    pub developer_key_path: Option<PathBuf>,
    pub cookie_path: Option<PathBuf>,
    #[serde(flatten)]
    pub server_config: RpcContextConfig,
}

#[derive(Debug)]
pub struct CliContextSeed {
    pub url: Url,
    pub client: Client,
    pub cookie_store: Arc<CookieStoreMutex>,
    pub cookie_path: PathBuf,
    pub developer_key_path: PathBuf,
}
impl Drop for CliContextSeed {
    fn drop(&mut self) {
        let tmp = format!("{}.tmp", self.cookie_path.display());
        let mut writer = fd_lock_rs::FdLock::lock(
            File::create(&tmp).unwrap(),
            fd_lock_rs::LockType::Exclusive,
            true,
        );
        let store = self.cookie_store.lock().unwrap();
        store.save_json(&mut writer).unwrap();
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
    pub fn init(matches: &ArgMatches) -> Result<Self, crate::Error> {
        let cfg_path = Path::new(matches.value_of("config").unwrap_or(crate::CONFIG_PATH));
        let mut base = if cfg_path.exists() {
            serde_yaml::from_reader(
                File::open(cfg_path)
                    .with_ctx(|_| (crate::ErrorKind::Filesystem, cfg_path.display().to_string()))?,
            )
            .with_kind(crate::ErrorKind::Deserialization)?
        } else {
            CliContextConfig::default()
        };
        let url = if let Some(host) = matches.value_of("host") {
            host.parse()?
        } else if let Some(host) = base.host {
            host
        } else {
            format!(
                "http://{}",
                base.bind_rpc.unwrap_or(([127, 0, 0, 1], 5959).into())
            )
            .parse()?
        };
        let proxy = if let Some(proxy) = matches.value_of("proxy") {
            Some(proxy.parse()?)
        } else {
            base.proxy
        };

        let cookie_path = base.cookie_path.unwrap_or_else(|| {
            cfg_path
                .parent()
                .unwrap_or(Path::new("/"))
                .join(".cookies.json")
        });
        let cookie_store = Arc::new(CookieStoreMutex::new(if cookie_path.exists() {
            CookieStore::load_json(BufReader::new(File::open(&cookie_path)?))
                .map_err(|e| anyhow!("{}", e))
                .with_kind(crate::ErrorKind::Deserialization)?
        } else {
            CookieStore::default()
        }));
        Ok(CliContext(Arc::new(CliContextSeed {
            url,
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
impl std::ops::Deref for CliContext {
    type Target = CliContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl Context for CliContext {
    fn protocol(&self) -> &str {
        self.0.url.scheme()
    }
    fn host(&self) -> Host<&str> {
        self.0.url.host().unwrap_or(DEFAULT_HOST)
    }
    fn port(&self) -> u16 {
        self.0.url.port().unwrap_or(DEFAULT_PORT)
    }
    fn path(&self) -> &str {
        self.0.url.path()
    }
    fn url(&self) -> Url {
        self.0.url.clone()
    }
    fn client(&self) -> &Client {
        &self.0.client
    }
}

fn deserialize_host<'de, D: serde::de::Deserializer<'de>>(
    deserializer: D,
) -> Result<Option<Host>, D::Error> {
    struct Visitor;
    impl<'de> serde::de::Visitor<'de> for Visitor {
        type Value = Option<Host>;
        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(formatter, "a parsable string")
        }
        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Host::parse(v)
                .map(Some)
                .map_err(|e| serde::de::Error::custom(e))
        }
        fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
        where
            D: serde::de::Deserializer<'de>,
        {
            deserializer.deserialize_str(Visitor)
        }
        fn visit_none<E>(self) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(None)
        }
        fn visit_unit<E>(self) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(None)
        }
    }
    deserializer.deserialize_any(Visitor)
}
