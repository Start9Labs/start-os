use std::collections::BTreeMap;
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use axum::body::Body;
use axum::extract::Request;
use axum::response::Response;
use chrono::Utc;
use http_body_util::BodyExt;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{Middleware, RpcRequest, RpcResponse};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha512};
use tokio::io::AsyncWriteExt;
use tokio::sync::Mutex;
use ts_rs::TS;

use crate::prelude::*;
use crate::registry::server::context::RegistryContext;
use crate::registry::signer::SignerKey;
use crate::util::serde::{Base64, Pem};

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
    signer: Option<Result<SignerKey, RpcError>>,
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
    pub key: SignerKey,
}

#[derive(Serialize, Deserialize)]
pub struct SignatureHeader {
    pub timestamp: i64,
    pub nonce: u64,
    #[serde(flatten)]
    pub signer: SignerKey,
    pub signature: Base64<[u8; 64]>,
}
impl SignatureHeader {
    pub fn sign_ed25519(
        key: &ed25519_dalek::SigningKey,
        body: &[u8],
        context: &str,
    ) -> Result<Self, Error> {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs() as i64)
            .unwrap_or_else(|e| e.duration().as_secs() as i64 * -1);
        let nonce = rand::random();
        let signer = SignerKey::Ed25519(Pem(key.verifying_key()));
        let mut hasher = Sha512::new();
        hasher.update(&i64::to_be_bytes(timestamp));
        hasher.update(&u64::to_be_bytes(nonce));
        hasher.update(body);
        let signature = Base64(
            key.sign_prehashed(hasher, Some(context.as_bytes()))?
                .to_bytes(),
        );
        Ok(Self {
            timestamp,
            nonce,
            signer,
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
                    let request = request;
                    let SignatureHeader {
                        timestamp,
                        nonce,
                        signer,
                        signature,
                    } = serde_urlencoded::from_str(
                        request
                            .headers()
                            .get(AUTH_SIG_HEADER)
                            .or_not_found("missing X-StartOS-Registry-Auth-Sig")
                            .with_kind(ErrorKind::InvalidRequest)?
                            .to_str()
                            .with_kind(ErrorKind::Utf8)?,
                    )
                    .with_kind(ErrorKind::Deserialization)?;
                    let now = SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .map(|d| d.as_secs() as i64)
                        .unwrap_or_else(|e| e.duration().as_secs() as i64 * -1);
                    if (now - timestamp).abs() > 30 {
                        return Err(Error::new(
                            eyre!("timestamp not within 30s of now"),
                            ErrorKind::InvalidSignature,
                        ));
                    }
                    self.handle_nonce(nonce).await?;
                    let body = std::mem::replace(request.body_mut(), Body::empty())
                        .collect()
                        .await
                        .with_kind(ErrorKind::Network)?
                        .to_bytes();
                    let mut verifier = signer.verifier();
                    verifier.update(&i64::to_be_bytes(timestamp));
                    verifier.update(&u64::to_be_bytes(nonce));
                    verifier.update(&body);
                    *request.body_mut() = Body::from(body);

                    verifier.verify(&*signature, &ctx.hostname)?;
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
