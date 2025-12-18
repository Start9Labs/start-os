use std::collections::BTreeMap;
use std::future::Future;
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use axum::body::Body;
use axum::extract::Request;
use http::{HeaderMap, HeaderValue};
use reqwest::Client;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{Middleware, RpcRequest, RpcResponse};
use serde::Deserialize;
use serde::de::DeserializeOwned;
use tokio::sync::Mutex;
use url::Url;

use crate::context::{CliContext, RpcContext};
use crate::middleware::auth::DbContext;
use crate::prelude::*;
use crate::sign::commitment::Commitment;
use crate::sign::commitment::request::RequestCommitment;
use crate::sign::{AnySignature, AnySigningKey, AnyVerifyingKey, SignatureScheme};
use crate::util::iter::TransposeResultIterExt;
use crate::util::serde::Base64;

pub const AUTH_SIG_HEADER: &str = "X-StartOS-Auth-Sig";

pub trait SignatureAuthContext: DbContext {
    type AdditionalMetadata: DeserializeOwned + Send;
    type CheckPubkeyRes: Send;
    fn sig_context(
        &self,
    ) -> impl Future<Output = impl IntoIterator<Item = Result<impl AsRef<str> + Send, Error>> + Send>
    + Send;
    fn check_pubkey(
        db: &Model<Self::Database>,
        pubkey: Option<&AnyVerifyingKey>,
        metadata: Self::AdditionalMetadata,
    ) -> Result<Self::CheckPubkeyRes, Error>;
    fn post_auth_hook(
        &self,
        check_pubkey_res: Self::CheckPubkeyRes,
        request: &RpcRequest,
    ) -> impl Future<Output = Result<(), Error>> + Send;
}

impl SignatureAuthContext for RpcContext {
    type AdditionalMetadata = ();
    type CheckPubkeyRes = ();
    async fn sig_context(
        &self,
    ) -> impl IntoIterator<Item = Result<impl AsRef<str> + Send, Error>> + Send {
        let peek = self.db.peek().await;
        self.account.peek(|a| {
            a.hostnames()
                .into_iter()
                .map(Ok)
                .chain(
                    peek.as_public()
                        .as_server_info()
                        .as_network()
                        .as_host()
                        .as_public_domains()
                        .keys()
                        .map(|k| k.into_iter())
                        .transpose(),
                )
                .chain(
                    peek.as_public()
                        .as_server_info()
                        .as_network()
                        .as_host()
                        .as_private_domains()
                        .de()
                        .map(|k| k.into_iter())
                        .transpose(),
                )
                .collect::<Vec<_>>()
        })
    }
    fn check_pubkey(
        db: &Model<Self::Database>,
        pubkey: Option<&AnyVerifyingKey>,
        _: Self::AdditionalMetadata,
    ) -> Result<Self::CheckPubkeyRes, Error> {
        if let Some(pubkey) = pubkey {
            if db.as_private().as_auth_pubkeys().de()?.contains(pubkey) {
                return Ok(());
            }
        }

        Err(Error::new(
            eyre!("Key is not authorized"),
            ErrorKind::IncorrectPassword,
        ))
    }
    async fn post_auth_hook(&self, _: Self::CheckPubkeyRes, _: &RpcRequest) -> Result<(), Error> {
        Ok(())
    }
}

pub trait SigningContext {
    fn signing_key(&self) -> Result<AnySigningKey, Error>;
}

impl SigningContext for CliContext {
    fn signing_key(&self) -> Result<AnySigningKey, Error> {
        Ok(AnySigningKey::Ed25519(self.developer_key()?.clone()))
    }
}

impl SigningContext for RpcContext {
    fn signing_key(&self) -> Result<AnySigningKey, Error> {
        Ok(AnySigningKey::Ed25519(
            self.account.peek(|a| a.developer_key.clone()),
        ))
    }
}

#[derive(Deserialize)]
pub struct Metadata<Additional> {
    #[serde(flatten)]
    additional: Additional,
    #[serde(default)]
    get_signer: bool,
}

#[derive(Clone)]
pub struct SignatureAuth {
    nonce_cache: Arc<Mutex<BTreeMap<Instant, u64>>>, // for replay protection
    signer: Option<Result<AnyVerifyingKey, RpcError>>,
}
impl SignatureAuth {
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

pub struct SignatureHeader {
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
        let query: BTreeMap<_, _> = form_urlencoded::parse(header.as_bytes()).collect();
        Ok(Self {
            commitment: RequestCommitment::from_query(&header)?,
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

impl<C: SignatureAuthContext> Middleware<C> for SignatureAuth {
    type Metadata = Metadata<C::AdditionalMetadata>;
    async fn process_http_request(
        &mut self,
        context: &C,
        request: &mut Request,
    ) -> Result<(), axum::response::Response> {
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
                            .or_not_found(AUTH_SIG_HEADER)
                            .with_kind(ErrorKind::InvalidRequest)?,
                    )?;

                    context.sig_context().await.into_iter().fold(
                        Err(Error::new(
                            eyre!("no valid signature context available to verify"),
                            ErrorKind::Authorization,
                        )),
                        |acc, x| {
                            if acc.is_ok() {
                                acc
                            } else {
                                signer.scheme().verify_commitment(
                                    &signer,
                                    &commitment,
                                    x?.as_ref(),
                                    &signature,
                                )
                            }
                        },
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
        context: &C,
        metadata: Self::Metadata,
        request: &mut RpcRequest,
    ) -> Result<(), RpcResponse> {
        async {
            let signer = self.signer.take().transpose()?;
            if metadata.get_signer {
                if let Some(signer) = &signer {
                    request.params["__Auth_signer"] = to_value(signer)?;
                }
            }
            let db = context.db().peek().await;
            let res = C::check_pubkey(&db, signer.as_ref(), metadata.additional)?;
            context.post_auth_hook(res, request).await?;
            Ok(())
        }
        .await
        .map_err(|e: Error| rpc_toolkit::RpcResponse::from_result(Err(e)))
    }
}

pub async fn call_remote<Ctx: SigningContext + AsRef<Client>>(
    ctx: &Ctx,
    url: Url,
    headers: HeaderMap,
    sig_context: Option<&str>,
    method: &str,
    params: Value,
) -> Result<Value, RpcError> {
    use reqwest::Method;
    use reqwest::header::{ACCEPT, CONTENT_LENGTH, CONTENT_TYPE};
    use rpc_toolkit::RpcResponse;
    use rpc_toolkit::yajrc::{GenericRpcMethod, Id, RpcRequest};

    let rpc_req = RpcRequest {
        id: Some(Id::Number(0.into())),
        method: GenericRpcMethod::<_, _, Value>::new(method),
        params,
    };
    let body = serde_json::to_vec(&rpc_req)?;
    let mut req = ctx
        .as_ref()
        .request(Method::POST, url)
        .header(CONTENT_TYPE, "application/json")
        .header(ACCEPT, "application/json")
        .header(CONTENT_LENGTH, body.len())
        .headers(headers);
    if let (Some(sig_ctx), Ok(key)) = (sig_context, ctx.signing_key()) {
        req = req.header(
            AUTH_SIG_HEADER,
            SignatureHeader::sign(&key, &body, sig_ctx)?.to_header(),
        );
    }
    let res = req.body(body).send().await?;

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
