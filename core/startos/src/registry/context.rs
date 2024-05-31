use std::net::{Ipv4Addr, SocketAddr};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::Parser;
use imbl_value::InternedString;
use patch_db::PatchDb;
use reqwest::{Client, Proxy};
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{CallRemote, Context, Empty};
use serde::{Deserialize, Serialize};
use tokio::sync::broadcast::Sender;
use tracing::instrument;
use url::Url;

use crate::context::config::{ContextConfig, CONFIG_PATH};
use crate::context::{CliContext, RpcContext};
use crate::prelude::*;
use crate::registry::auth::{SignatureHeader, AUTH_SIG_HEADER};
use crate::registry::device_info::{DeviceInfo, DEVICE_INFO_HEADER};
use crate::registry::signer::sign::AnySigningKey;
use crate::registry::RegistryDatabase;
use crate::rpc_continuations::RpcContinuations;

#[derive(Debug, Clone, Default, Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct RegistryConfig {
    #[arg(short = 'c', long = "config")]
    pub config: Option<PathBuf>,
    #[arg(short = 'l', long = "listen")]
    pub listen: Option<SocketAddr>,
    #[arg(short = 'h', long = "hostname")]
    pub hostname: Option<InternedString>,
    #[arg(short = 'p', long = "tor-proxy")]
    pub tor_proxy: Option<Url>,
    #[arg(short = 'd', long = "datadir")]
    pub datadir: Option<PathBuf>,
}
impl ContextConfig for RegistryConfig {
    fn next(&mut self) -> Option<PathBuf> {
        self.config.take()
    }
    fn merge_with(&mut self, other: Self) {
        self.listen = self.listen.take().or(other.listen);
        self.hostname = self.hostname.take().or(other.hostname);
        self.tor_proxy = self.tor_proxy.take().or(other.tor_proxy);
        self.datadir = self.datadir.take().or(other.datadir);
    }
}

impl RegistryConfig {
    pub fn load(mut self) -> Result<Self, Error> {
        let path = self.next();
        self.load_path_rec(path)?;
        self.load_path_rec(Some(CONFIG_PATH))?;
        Ok(self)
    }
}

pub struct RegistryContextSeed {
    pub hostname: InternedString,
    pub listen: SocketAddr,
    pub db: TypedPatchDb<RegistryDatabase>,
    pub datadir: PathBuf,
    pub rpc_continuations: RpcContinuations,
    pub client: Client,
    pub shutdown: Sender<()>,
}

#[derive(Clone)]
pub struct RegistryContext(Arc<RegistryContextSeed>);
impl RegistryContext {
    #[instrument(skip_all)]
    pub async fn init(config: &RegistryConfig) -> Result<Self, Error> {
        let (shutdown, _) = tokio::sync::broadcast::channel(1);
        let datadir = config
            .datadir
            .as_deref()
            .unwrap_or_else(|| Path::new("/var/lib/startos"))
            .to_owned();
        if tokio::fs::metadata(&datadir).await.is_err() {
            tokio::fs::create_dir_all(&datadir).await?;
        }
        let db_path = datadir.join("registry.db");
        let db = TypedPatchDb::<RegistryDatabase>::load_or_init(
            PatchDb::open(&db_path).await?,
            || async { Ok(Default::default()) },
        )
        .await?;
        let tor_proxy_url = config
            .tor_proxy
            .clone()
            .map(Ok)
            .unwrap_or_else(|| "socks5h://localhost:9050".parse())?;
        Ok(Self(Arc::new(RegistryContextSeed {
            hostname: config
                .hostname
                .as_ref()
                .ok_or_else(|| {
                    Error::new(
                        eyre!("missing required configuration: hostname"),
                        ErrorKind::NotFound,
                    )
                })?
                .clone(),
            listen: config
                .listen
                .unwrap_or(SocketAddr::new(Ipv4Addr::LOCALHOST.into(), 5959)),
            db,
            datadir,
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
impl AsRef<RpcContinuations> for RegistryContext {
    fn as_ref(&self) -> &RpcContinuations {
        &self.rpc_continuations
    }
}

impl Context for RegistryContext {}
impl Deref for RegistryContext {
    type Target = RegistryContextSeed;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

#[derive(Debug, Deserialize, Serialize, Parser)]
pub struct RegistryUrlParams {
    pub registry: Url,
}

impl CallRemote<RegistryContext> for CliContext {
    async fn call_remote(
        &self,
        mut method: &str,
        params: Value,
        _: Empty,
    ) -> Result<Value, RpcError> {
        use reqwest::header::{ACCEPT, CONTENT_LENGTH, CONTENT_TYPE};
        use reqwest::Method;
        use rpc_toolkit::yajrc::{GenericRpcMethod, Id, RpcRequest};
        use rpc_toolkit::RpcResponse;

        let url = self
            .registry_url
            .clone()
            .ok_or_else(|| Error::new(eyre!("`--registry` required"), ErrorKind::InvalidRequest))?;
        method = method.strip_prefix("registry.").unwrap_or(method);

        let rpc_req = RpcRequest {
            id: Some(Id::Number(0.into())),
            method: GenericRpcMethod::<_, _, Value>::new(method),
            params,
        };
        let body = serde_json::to_vec(&rpc_req)?;
        let host = url.host().or_not_found("registry hostname")?.to_string();
        let res = self
            .client
            .request(Method::POST, url)
            .header(CONTENT_TYPE, "application/json")
            .header(ACCEPT, "application/json")
            .header(CONTENT_LENGTH, body.len())
            .header(
                AUTH_SIG_HEADER,
                SignatureHeader::sign(
                    &AnySigningKey::Ed25519(self.developer_key()?.clone()),
                    &body,
                    &host,
                )?
                .to_header(),
            )
            .body(body)
            .send()
            .await?;

        match res
            .headers()
            .get(CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
        {
            Some("application/json") => {
                serde_json::from_slice::<RpcResponse>(&*res.bytes().await?)
                    .with_kind(ErrorKind::Deserialization)?
                    .result
            }
            _ => Err(Error::new(eyre!("missing content type"), ErrorKind::Network).into()),
        }
    }
}

impl CallRemote<RegistryContext, RegistryUrlParams> for RpcContext {
    async fn call_remote(
        &self,
        mut method: &str,
        params: Value,
        RegistryUrlParams { registry }: RegistryUrlParams,
    ) -> Result<Value, RpcError> {
        use reqwest::header::{ACCEPT, CONTENT_LENGTH, CONTENT_TYPE};
        use reqwest::Method;
        use rpc_toolkit::yajrc::{GenericRpcMethod, Id, RpcRequest};
        use rpc_toolkit::RpcResponse;

        let url = registry.join("rpc/v0")?;
        method = method.strip_prefix("registry.").unwrap_or(method);

        let rpc_req = RpcRequest {
            id: Some(Id::Number(0.into())),
            method: GenericRpcMethod::<_, _, Value>::new(method),
            params,
        };
        let body = serde_json::to_vec(&rpc_req)?;
        let res = self
            .client
            .request(Method::POST, url)
            .header(CONTENT_TYPE, "application/json")
            .header(ACCEPT, "application/json")
            .header(CONTENT_LENGTH, body.len())
            .header(DEVICE_INFO_HEADER, DeviceInfo::from(self).to_header_value())
            .body(body)
            .send()
            .await?;

        match res
            .headers()
            .get(CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
        {
            Some("application/json") => {
                serde_json::from_slice::<RpcResponse>(&*res.bytes().await?)
                    .with_kind(ErrorKind::Deserialization)?
                    .result
            }
            _ => Err(Error::new(eyre!("missing content type"), ErrorKind::Network).into()),
        }
    }
}
