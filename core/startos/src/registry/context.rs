use std::net::{Ipv4Addr, SocketAddr};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::Parser;
use emver::VersionRange;
use imbl_value::InternedString;
use patch_db::PatchDb;
use reqwest::Client;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{CallRemote, Context, EitherContext, Empty};
use serde::{Deserialize, Serialize};
use tokio::sync::broadcast::Sender;
use tracing::instrument;
use url::Url;

use crate::context::config::{ContextConfig, CONFIG_PATH};
use crate::context::{CliContext, RpcContext};
use crate::prelude::*;
use crate::registry::auth::{SignatureHeader, AUTH_SIG_HEADER};
use crate::registry::RegistryDatabase;
use crate::rpc_continuations::RpcContinuations;
use crate::version::VersionT;

#[derive(Debug, Clone, Default, Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct RegistryConfig {
    #[arg(short = 'c', long = "config")]
    pub config: Option<PathBuf>,
    #[arg(short = 'l', long = "listen")]
    pub listen: Option<SocketAddr>,
    #[arg(short = 'h', long = "hostname")]
    pub hostname: InternedString,
    #[arg(short = 'd', long = "datadir")]
    pub datadir: Option<PathBuf>,
}
impl ContextConfig for RegistryConfig {
    fn next(&mut self) -> Option<PathBuf> {
        self.config.take()
    }
    fn merge_with(&mut self, other: Self) {
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
        Ok(Self(Arc::new(RegistryContextSeed {
            hostname: config.hostname.clone(),
            listen: config
                .listen
                .unwrap_or(SocketAddr::new(Ipv4Addr::LOCALHOST.into(), 5959)),
            db,
            datadir,
            rpc_continuations: RpcContinuations::new(),
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
                serde_urlencoded::to_string(&SignatureHeader::sign_ed25519(
                    self.developer_key()?,
                    &body,
                    &host,
                )?)
                .with_kind(ErrorKind::Serialization)?,
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

fn hardware_header(ctx: &RpcContext) -> String {
    let mut url: Url = "http://localhost".parse().unwrap();
    url.query_pairs_mut()
        .append_pair(
            "os.version",
            &crate::version::Current::new().semver().to_string(),
        )
        .append_pair(
            "os.compat",
            &crate::version::Current::new().compat().to_string(),
        )
        .append_pair("os.arch", &*crate::PLATFORM)
        .append_pair("hardware.arch", &*crate::ARCH)
        .append_pair("hardware.ram", &ctx.hardware.ram.to_string());

    for hw in &ctx.hardware.devices {
        url.query_pairs_mut()
            .append_pair(&format!("hardware.device.{}", hw.class()), hw.product());
    }

    url.query().unwrap_or_default().to_string()
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

        let url = registry;
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
            .header(
                AUTH_SIG_HEADER,
                serde_urlencoded::to_string(&hardware_header(self))
                    .with_kind(ErrorKind::Serialization)?,
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
