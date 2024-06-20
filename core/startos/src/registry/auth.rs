use std::collections::BTreeMap;
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use axum::body::Body;
use axum::extract::Request;
use axum::response::Response;
use chrono::Utc;
use http::HeaderValue;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{Middleware, RpcRequest, RpcResponse};
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWriteExt;
use tokio::sync::Mutex;
use ts_rs::TS;
use url::Url;

use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::registry::signer::commitment::request::RequestCommitment;
use crate::registry::signer::commitment::Commitment;
use crate::registry::signer::sign::{
    AnySignature, AnySigningKey, AnyVerifyingKey, SignatureScheme,
};
use crate::util::serde::Base64;

pub const AUTH_SIG_HEADER: &str = "X-StartOS-Registry-Auth-Sig";

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Metadata {
    #[serde(default)]
    admin: bool,
    #[serde(default)]
    get_signer: bool,
}

#[derive(Clone)]
pub struct Auth {
    nonce_cache: Arc<Mutex<BTreeMap<Instant, u64>>>, // for replay protection
    signer: Option<Result<AnyVerifyingKey, RpcError>>,
}
impl Auth {
    pub fn new() -> Self {
        Self {
            nonce_cache: Arc::new(Mutex::new(BTreeMap::new())),
            signer: None,
        }
    }
    async fn handle_nonce(&mut self, nonce: u64) -> Result<(), Error> {
        let mut cache = self.nonce_cache.lock().await;
        if cache.values().any(|n| *n == nonce) {
            return Err(Error::new(
                eyre!("replay attack detected"),
                ErrorKind::Authorization,
            ));
        }
        while let Some(entry) = cache.first_entry() {
            if entry.key().elapsed() > Duration::from_secs(60) {
                entry.remove_entry();
            } else {
                break;
            }
        }
        Ok(())
    }
}

#[derive(Serialize, Deserialize, TS)]
pub struct RegistryAdminLogRecord {
    pub timestamp: String,
    pub name: String,
    #[ts(type = "{ id: string | number | null; method: string; params: any }")]
    pub request: RpcRequest,
    pub key: AnyVerifyingKey,
}

#[derive(Serialize, Deserialize)]
pub struct SignatureHeader {
    #[serde(flatten)]
    pub commitment: RequestCommitment,
    pub signer: AnyVerifyingKey,
    pub signature: AnySignature,
}
impl SignatureHeader {
    pub fn to_header(&self) -> HeaderValue {
        let mut url: Url = "http://localhost".parse().unwrap();
        self.commitment.append_query(&mut url);
        url.query_pairs_mut()
            .append_pair("signer", &self.signer.to_string());
        url.query_pairs_mut()
            .append_pair("signature", &self.signature.to_string());
        HeaderValue::from_str(url.query().unwrap_or_default()).unwrap()
    }
    pub fn from_header(header: &HeaderValue) -> Result<Self, Error> {
        let url: Url = format!(
            "http://localhost/?{}",
            header.to_str().with_kind(ErrorKind::Utf8)?
        )
        .parse()?;
        let query: BTreeMap<_, _> = url.query_pairs().collect();
        Ok(Self {
            commitment: RequestCommitment::from_query(&url)?,
            signer: query.get("signer").or_not_found("signer")?.parse()?,
            signature: query.get("signature").or_not_found("signature")?.parse()?,
        })
    }
    pub fn sign(signer: &AnySigningKey, body: &[u8], context: &str) -> Result<Self, Error> {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs() as i64)
            .unwrap_or_else(|e| e.duration().as_secs() as i64 * -1);
        let nonce = rand::random();
        let commitment = RequestCommitment {
            timestamp,
            nonce,
            size: body.len() as u64,
            blake3: Base64(*blake3::hash(body).as_bytes()),
        };
        let signature = signer
            .scheme()
            .sign_commitment(&signer, &commitment, context)?;
        Ok(Self {
            commitment,
            signer: signer.verifying_key(),
            signature,
        })
    }
}

impl Middleware<RegistryContext> for Auth {
    type Metadata = Metadata;
    async fn process_http_request(
        &mut self,
        ctx: &RegistryContext,
        request: &mut Request,
    ) -> Result<(), Response> {
        if request.headers().contains_key(AUTH_SIG_HEADER) {
            self.signer = Some(
                async {
                    let SignatureHeader {
                        commitment,
                        signer,
                        signature,
                    } = SignatureHeader::from_header(
                        request
                            .headers()
                            .get(AUTH_SIG_HEADER)
                            .or_not_found("missing X-StartOS-Registry-Auth-Sig")
                            .with_kind(ErrorKind::InvalidRequest)?,
                    )?;

                    signer.scheme().verify_commitment(
                        &signer,
                        &commitment,
                        &ctx.hostname,
                        &signature,
                    )?;

                    let now = SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .map(|d| d.as_secs() as i64)
                        .unwrap_or_else(|e| e.duration().as_secs() as i64 * -1);
                    if (now - commitment.timestamp).abs() > 30 {
                        return Err(Error::new(
                            eyre!("timestamp not within 30s of now"),
                            ErrorKind::InvalidSignature,
                        ));
                    }
                    self.handle_nonce(commitment.nonce).await?;

                    let mut body = Vec::with_capacity(commitment.size as usize);
                    commitment.copy_to(request, &mut body).await?;
                    *request.body_mut() = Body::from(body);

                    Ok(signer)
                }
                .await
                .map_err(RpcError::from),
            );
        }
        Ok(())
    }
    async fn process_rpc_request(
        &mut self,
        ctx: &RegistryContext,
        metadata: Self::Metadata,
        request: &mut RpcRequest,
    ) -> Result<(), RpcResponse> {
        async move {
            let signer = self.signer.take().transpose()?;
            if metadata.get_signer {
                if let Some(signer) = &signer {
                    request.params["__auth_signer"] = to_value(signer)?;
                }
            }
            if metadata.admin {
                let signer = signer
                    .ok_or_else(|| Error::new(eyre!("UNAUTHORIZED"), ErrorKind::Authorization))?;
                let db = ctx.db.peek().await;
                let (guid, admin) = db.as_index().as_signers().get_signer_info(&signer)?;
                if db.into_admins().de()?.contains(&guid) {
                    let mut log = tokio::fs::OpenOptions::new()
                        .create(true)
                        .append(true)
                        .open(ctx.datadir.join("admin.log"))
                        .await?;
                    log.write_all(
                        (serde_json::to_string(&RegistryAdminLogRecord {
                            timestamp: Utc::now().to_rfc3339(),
                            name: admin.name,
                            request: request.clone(),
                            key: signer,
                        })
                        .with_kind(ErrorKind::Serialization)?
                            + "\n")
                            .as_bytes(),
                    )
                    .await?;
                } else {
                    return Err(Error::new(eyre!("UNAUTHORIZED"), ErrorKind::Authorization));
                }
            }

            Ok(())
        }
        .await
        .map_err(|e| RpcResponse::from_result(Err(e)))
    }
}
