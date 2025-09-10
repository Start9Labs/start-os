use std::net::{Ipv4Addr, SocketAddr};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use chrono::Utc;
use clap::Parser;
use imbl_value::InternedString;
use patch_db::PatchDb;
use reqwest::{Client, Proxy};
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{CallRemote, Context, Empty, RpcRequest};
use serde::{Deserialize, Serialize};
use sqlx::PgPool;
use tokio::sync::broadcast::Sender;
use tracing::instrument;
use ts_rs::TS;
use url::Url;

use crate::context::config::{CONFIG_PATH, ContextConfig};
use crate::context::{CliContext, RpcContext};
use crate::middleware::signature::SignatureAuthContext;
use crate::prelude::*;
use crate::registry::RegistryDatabase;
use crate::registry::device_info::{DEVICE_INFO_HEADER, DeviceInfo};
use crate::registry::signer::SignerInfo;
use crate::rpc_continuations::RpcContinuations;
use crate::sign::AnyVerifyingKey;
use crate::util::io::append_file;

const DEFAULT_REGISTRY_LISTEN: SocketAddr =
    SocketAddr::new(std::net::IpAddr::V4(Ipv4Addr::LOCALHOST), 5959);

#[derive(Debug, Clone, Default, Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct RegistryConfig {
    #[arg(short = 'c', long = "config")]
    pub config: Option<PathBuf>,
    #[arg(short = 'l', long = "listen")]
    pub registry_listen: Option<SocketAddr>,
    #[arg(short = 'H', long = "hostname")]
    pub registry_hostname: Vec<InternedString>,
    #[arg(short = 'p', long = "tor-proxy")]
    pub tor_proxy: Option<Url>,
    #[arg(short = 'd', long = "datadir")]
    pub datadir: Option<PathBuf>,
    #[arg(short = 'u', long = "pg-connection-url")]
    pub pg_connection_url: Option<String>,
}
impl ContextConfig for RegistryConfig {
    fn next(&mut self) -> Option<PathBuf> {
        self.config.take()
    }
    fn merge_with(&mut self, mut other: Self) {
        self.registry_listen = self.registry_listen.take().or(other.registry_listen);
        self.registry_hostname.append(&mut other.registry_hostname);
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
    pub hostnames: Vec<InternedString>,
    pub listen: SocketAddr,
    pub db: TypedPatchDb<RegistryDatabase>,
    pub datadir: PathBuf,
    pub rpc_continuations: RpcContinuations,
    pub client: Client,
    pub shutdown: Sender<()>,
    pub pool: Option<PgPool>,
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
        let pool: Option<PgPool> = match &config.pg_connection_url {
            Some(url) => match PgPool::connect(url.as_str()).await {
                Ok(pool) => Some(pool),
                Err(_) => None,
            },
            None => None,
        };
        if config.registry_hostname.is_empty() {
            return Err(Error::new(
                eyre!("missing required configuration: registry-hostname"),
                ErrorKind::NotFound,
            ));
        }
        Ok(Self(Arc::new(RegistryContextSeed {
            hostnames: config.registry_hostname.clone(),
            listen: config.registry_listen.unwrap_or(DEFAULT_REGISTRY_LISTEN),
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
            pool,
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
        let url = if let Some(url) = self.registry_url.clone() {
            url
        } else if self.registry_hostname.is_some() {
            format!(
                "http://{}",
                self.registry_listen.unwrap_or(DEFAULT_REGISTRY_LISTEN)
            )
            .parse()
            .map_err(Error::from)?
        } else {
            return Err(
                Error::new(eyre!("`--registry` required"), ErrorKind::InvalidRequest).into(),
            );
        };
        method = method.strip_prefix("registry.").unwrap_or(method);
        let sig_context = self
            .registry_hostname
            .clone()
            .or(url.host().as_ref().map(InternedString::from_display))
            .or_not_found("registry hostname")?;

        crate::middleware::signature::call_remote(self, url, &sig_context, method, params).await
    }
}

impl CallRemote<RegistryContext, RegistryUrlParams> for RpcContext {
    async fn call_remote(
        &self,
        mut method: &str,
        params: Value,
        RegistryUrlParams { registry }: RegistryUrlParams,
    ) -> Result<Value, RpcError> {
        use reqwest::Method;
        use reqwest::header::{ACCEPT, CONTENT_LENGTH, CONTENT_TYPE};
        use rpc_toolkit::RpcResponse;
        use rpc_toolkit::yajrc::{GenericRpcMethod, Id, RpcRequest};

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
            .header(
                DEVICE_INFO_HEADER,
                DeviceInfo::load(self).await?.to_header_value(),
            )
            .body(body)
            .send()
            .await?;

        if !res.status().is_success() {
            let status = res.status();
            let txt = res.text().await?;
            let mut res = Err(Error::new(
                eyre!("{}", status.canonical_reason().unwrap_or(status.as_str())),
                ErrorKind::Network,
            ));
            if !txt.is_empty() {
                res = res.with_ctx(|_| (ErrorKind::Network, txt));
            }
            return res.map_err(From::from);
        }

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
            _ => Err(Error::new(eyre!("unknown content type"), ErrorKind::Network).into()),
        }
    }
}

#[derive(Deserialize)]
pub struct RegistryAuthMetadata {
    #[serde(default)]
    admin: bool,
}

#[derive(Serialize, Deserialize, TS)]
pub struct AdminLogRecord {
    pub timestamp: String,
    pub name: String,
    #[ts(type = "{ id: string | number | null; method: string; params: any }")]
    pub request: RpcRequest,
    pub key: AnyVerifyingKey,
}

impl SignatureAuthContext for RegistryContext {
    type Database = RegistryDatabase;
    type AdditionalMetadata = RegistryAuthMetadata;
    type CheckPubkeyRes = Option<(AnyVerifyingKey, SignerInfo)>;
    fn db(&self) -> &TypedPatchDb<Self::Database> {
        &self.db
    }
    async fn sig_context(
        &self,
    ) -> impl IntoIterator<Item = Result<impl AsRef<str> + Send, Error>> + Send {
        self.hostnames.iter().map(Ok)
    }
    fn check_pubkey(
        db: &Model<Self::Database>,
        pubkey: Option<&AnyVerifyingKey>,
        metadata: Self::AdditionalMetadata,
    ) -> Result<Self::CheckPubkeyRes, Error> {
        if metadata.admin {
            if let Some(pubkey) = pubkey {
                let (guid, admin) = db.as_index().as_signers().get_signer_info(pubkey)?;
                if db.as_admins().de()?.contains(&guid) {
                    return Ok(Some((pubkey.clone(), admin)));
                }
            }
            Err(Error::new(eyre!("UNAUTHORIZED"), ErrorKind::Authorization))
        } else {
            Ok(None)
        }
    }
    async fn post_auth_hook(
        &self,
        check_pubkey_res: Self::CheckPubkeyRes,
        request: &RpcRequest,
    ) -> Result<(), Error> {
        use tokio::io::AsyncWriteExt;
        if let Some((pubkey, admin)) = check_pubkey_res {
            let mut log = append_file(self.datadir.join("admin.log")).await?;
            log.write_all(
                (serde_json::to_string(&AdminLogRecord {
                    timestamp: Utc::now().to_rfc3339(),
                    name: admin.name,
                    request: request.clone(),
                    key: pubkey,
                })
                .with_kind(ErrorKind::Serialization)?
                    + "\n")
                    .as_bytes(),
            )
            .await?;
        }
        Ok(())
    }
}
