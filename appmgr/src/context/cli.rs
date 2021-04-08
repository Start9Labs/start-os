use std::fs::File;
use std::net::IpAddr;
use std::path::Path;
use std::sync::Arc;

use clap::ArgMatches;
use reqwest::Proxy;
use rpc_toolkit::reqwest::{Client, Url};
use rpc_toolkit::url::Host;
use rpc_toolkit::Context;
use serde::Deserialize;

use super::rpc::RpcContextConfig;
use crate::ResultExt;

#[derive(Debug, Default, Deserialize)]
pub struct CliContextConfig {
    #[serde(deserialize_with = "deserialize_host")]
    pub host: Option<Host>,
    pub port: Option<u16>,
    #[serde(deserialize_with = "crate::util::deserialize_from_str_opt")]
    pub proxy: Option<Url>,
    #[serde(flatten)]
    pub server_config: RpcContextConfig,
}

#[derive(Debug)]
pub struct CliContextSeed {
    pub host: Host,
    pub port: u16,
    pub client: Client,
}

#[derive(Debug, Clone)]
pub struct CliContext(Arc<CliContextSeed>);
impl CliContext {
    pub fn init(matches: &ArgMatches) -> Result<Self, crate::Error> {
        let cfg_path = Path::new(crate::CONFIG_PATH);
        let mut base = if cfg_path.exists() {
            serde_yaml::from_reader(
                File::open(cfg_path)
                    .with_ctx(|_| (crate::ErrorKind::Filesystem, cfg_path.display().to_string()))?,
            )
            .with_kind(crate::ErrorKind::Deserialization)?
        } else {
            CliContextConfig::default()
        };
        if let Some(bind) = base.server_config.bind {
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
        })))
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
