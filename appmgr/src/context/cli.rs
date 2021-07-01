use std::fs::File;
use std::io::Read;
use std::net::IpAddr;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::anyhow;
use clap::ArgMatches;
use reqwest::Proxy;
use rpc_toolkit::reqwest::{Client, Url};
use rpc_toolkit::url::Host;
use rpc_toolkit::Context;
use serde::Deserialize;

use super::rpc::RpcContextConfig;
use crate::{Error, ResultExt};

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct CliContextConfig {
    #[serde(deserialize_with = "deserialize_host")]
    pub host: Option<Host>,
    pub port: Option<u16>,
    #[serde(deserialize_with = "crate::util::deserialize_from_str_opt")]
    pub proxy: Option<Url>,
    pub developer_key_path: Option<PathBuf>,
    #[serde(flatten)]
    pub server_config: RpcContextConfig,
}

#[derive(Debug)]
pub struct CliContextSeed {
    pub host: Host,
    pub port: u16,
    pub client: Client,
    pub developer_key_path: PathBuf,
}

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
        if let Some(bind) = base.server_config.bind_rpc {
            if base.host.is_none() {
                base.host = Some(match bind.ip() {
                    IpAddr::V4(a) => Host::Ipv4(a),
                    IpAddr::V6(a) => Host::Ipv6(a),
                });
            }
            if base.port.is_none() {
                base.port = Some(bind.port())
            }
        }
        if let Some(host) = matches.value_of("host") {
            base.host = Some(Host::parse(host).with_kind(crate::ErrorKind::ParseUrl)?);
        }
        if let Some(port) = matches.value_of("port") {
            base.port = Some(port.parse()?);
        }
        if let Some(proxy) = matches.value_of("proxy") {
            base.proxy = Some(proxy.parse()?);
        }
        Ok(CliContext(Arc::new(CliContextSeed {
            host: base.host.unwrap_or(Host::Ipv4([127, 0, 0, 1].into())),
            port: base.port.unwrap_or(5959),
            client: if let Some(proxy) = base.proxy {
                Client::builder()
                    .proxy(Proxy::all(proxy).with_kind(crate::ErrorKind::ParseUrl)?)
                    .build()
                    .expect("cannot fail")
            } else {
                Client::new()
            },
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
    fn host(&self) -> Host<&str> {
        match &self.0.host {
            Host::Domain(a) => Host::Domain(a.as_str()),
            Host::Ipv4(a) => Host::Ipv4(*a),
            Host::Ipv6(a) => Host::Ipv6(*a),
        }
    }
    fn port(&self) -> u16 {
        self.0.port
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
