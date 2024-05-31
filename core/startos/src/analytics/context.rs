use std::net::{Ipv4Addr, SocketAddr};
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;

use clap::Parser;
use reqwest::{Client, Proxy};
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{call_remote_http, CallRemote, Context, Empty};
use serde::{Deserialize, Serialize};
use sqlx::PgPool;
use tokio::sync::broadcast::Sender;
use tracing::instrument;
use url::Url;

use crate::context::config::{ContextConfig, CONFIG_PATH};
use crate::context::RpcContext;
use crate::prelude::*;
use crate::rpc_continuations::RpcContinuations;

#[derive(Debug, Clone, Default, Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct AnalyticsConfig {
    #[arg(short = 'c', long = "config")]
    pub config: Option<PathBuf>,
    #[arg(short = 'l', long = "listen")]
    pub listen: Option<SocketAddr>,
    #[arg(short = 'p', long = "proxy")]
    pub tor_proxy: Option<Url>,
    #[arg(short = 'd', long = "dbconnect")]
    pub dbconnect: Option<Url>,
}
impl ContextConfig for AnalyticsConfig {
    fn next(&mut self) -> Option<PathBuf> {
        self.config.take()
    }
    fn merge_with(&mut self, other: Self) {
        self.listen = self.listen.take().or(other.listen);
        self.tor_proxy = self.tor_proxy.take().or(other.tor_proxy);
        self.dbconnect = self.dbconnect.take().or(other.dbconnect);
    }
}

impl AnalyticsConfig {
    pub fn load(mut self) -> Result<Self, Error> {
        let path = self.next();
        self.load_path_rec(path)?;
        self.load_path_rec(Some(CONFIG_PATH))?;
        Ok(self)
    }
}

pub struct AnalyticsContextSeed {
    pub listen: SocketAddr,
    pub db: PgPool,
    pub rpc_continuations: RpcContinuations,
    pub client: Client,
    pub shutdown: Sender<()>,
}

#[derive(Clone)]
pub struct AnalyticsContext(Arc<AnalyticsContextSeed>);
impl AnalyticsContext {
    #[instrument(skip_all)]
    pub async fn init(config: &AnalyticsConfig) -> Result<Self, Error> {
        let (shutdown, _) = tokio::sync::broadcast::channel(1);
        let dbconnect = config
            .dbconnect
            .clone()
            .unwrap_or_else(|| "postgres://localhost/analytics".parse().unwrap())
            .to_owned();
        let db = PgPool::connect(dbconnect.as_str()).await?;
        let tor_proxy_url = config
            .tor_proxy
            .clone()
            .map(Ok)
            .unwrap_or_else(|| "socks5h://localhost:9050".parse())?;
        Ok(Self(Arc::new(AnalyticsContextSeed {
            listen: config
                .listen
                .unwrap_or(SocketAddr::new(Ipv4Addr::LOCALHOST.into(), 5959)),
            db,
            rpc_continuations: RpcContinuations::new(),
            client: Client::builder()
                .proxy(Proxy::custom(move |url| {
                    if url.host_str().map_or(false, |h| h.ends_with(".onion")) {
                        Some(tor_proxy_url.clone())
                    } else {
                        None
                    }
                }))
                .build()
                .with_kind(crate::ErrorKind::ParseUrl)?,
            shutdown,
        })))
    }
}
impl AsRef<RpcContinuations> for AnalyticsContext {
    fn as_ref(&self) -> &RpcContinuations {
        &self.rpc_continuations
    }
}

impl Context for AnalyticsContext {}
impl Deref for AnalyticsContext {
    type Target = AnalyticsContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl CallRemote<AnalyticsContext> for RpcContext {
    async fn call_remote(&self, method: &str, params: Value, _: Empty) -> Result<Value, RpcError> {
        if let Some(analytics_url) = self.analytics_url.clone() {
            call_remote_http(&self.client, analytics_url, method, params).await
        } else {
            Ok(Value::Null)
        }
    }
}
