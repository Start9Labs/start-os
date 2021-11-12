use std::borrow::Borrow;
use std::sync::Arc;
use std::time::{Duration, Instant};

use basic_cookies::Cookie;
use color_eyre::eyre::eyre;
use digest::Digest;
use futures::future::BoxFuture;
use futures::FutureExt;
use http::StatusCode;
use rpc_toolkit::command_helpers::prelude::RequestParts;
use rpc_toolkit::hyper::header::COOKIE;
use rpc_toolkit::hyper::http::Error as HttpError;
use rpc_toolkit::hyper::{Body, Request, Response};
use rpc_toolkit::rpc_server_helpers::{noop3, to_response, DynMiddleware, DynMiddlewareStage2};
use rpc_toolkit::yajrc::RpcMethod;
use rpc_toolkit::Metadata;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use tokio::sync::Mutex;

use crate::context::RpcContext;
use crate::{Error, ResultExt};
pub trait AsLogoutSessionId {
    fn as_logout_session_id(self) -> String;
}

/// Will need to know when we have logged out from a route
#[derive(Serialize, Deserialize)]
pub struct HasLoggedOutSessions(());

impl HasLoggedOutSessions {
    pub async fn new(
        logged_out_sessions: impl IntoIterator<Item = impl AsLogoutSessionId>,
        ctx: &RpcContext,
    ) -> Result<Self, Error> {
        let sessions = logged_out_sessions
            .into_iter()
            .by_ref()
            .map(|x| x.as_logout_session_id())
            .collect::<Vec<_>>();
        sqlx::query(&format!(
            "UPDATE session SET logged_out = CURRENT_TIMESTAMP WHERE id IN ('{}')",
            sessions.join("','")
        ))
        .execute(&mut ctx.secret_store.acquire().await?)
        .await?;
        for session in sessions {
            for socket in ctx
                .open_authed_websockets
                .lock()
                .await
                .remove(&session)
                .unwrap_or_default()
            {
                let _ = socket.send(());
            }
        }
        Ok(Self(()))
    }
}

/// Used when we need to know that we have logged in with a valid user
#[derive(Clone, Copy)]
pub struct HasValidSession(());

impl HasValidSession {
    pub async fn from_request_parts(
        request_parts: &RequestParts,
        ctx: &RpcContext,
    ) -> Result<Self, Error> {
        Self::from_session(&HashSessionToken::from_request_parts(request_parts)?, ctx).await
    }

    pub async fn from_session(session: &HashSessionToken, ctx: &RpcContext) -> Result<Self, Error> {
        let session_hash = session.hashed();
        let session = sqlx::query!("UPDATE session SET last_active = CURRENT_TIMESTAMP WHERE id = ? AND logged_out IS NULL OR logged_out > CURRENT_TIMESTAMP", session_hash)
            .execute(&mut ctx.secret_store.acquire().await?)
            .await?;
        if session.rows_affected() == 0 {
            return Err(Error::new(
                eyre!("UNAUTHORIZED"),
                crate::ErrorKind::Authorization,
            ));
        }
        Ok(Self(()))
    }
}

/// When we have a need to create a new session,
/// Or when we are using internal valid authenticated service.
#[derive(Debug, Clone)]
pub struct HashSessionToken {
    hashed: String,
    token: String,
}
impl HashSessionToken {
    pub fn new() -> Self {
        let token = base32::encode(
            base32::Alphabet::RFC4648 { padding: false },
            &rand::random::<[u8; 16]>(),
        )
        .to_lowercase();
        let hashed = Self::hash(&token);
        Self { hashed, token }
    }
    pub fn from_cookie(cookie: &Cookie) -> Self {
        let token = cookie.get_value().to_owned();
        let hashed = Self::hash(&token);
        Self { hashed, token }
    }

    pub fn from_request_parts(request_parts: &RequestParts) -> Result<Self, Error> {
        if let Some(cookie_header) = request_parts.headers.get(COOKIE) {
            let cookies = Cookie::parse(
                cookie_header
                    .to_str()
                    .with_kind(crate::ErrorKind::Authorization)?,
            )
            .with_kind(crate::ErrorKind::Authorization)?;
            if let Some(session) = cookies.iter().find(|c| c.get_name() == "session") {
                return Ok(Self::from_cookie(session));
            }
        }
        Err(Error::new(
            eyre!("UNAUTHORIZED"),
            crate::ErrorKind::Authorization,
        ))
    }

    pub fn header_value(&self) -> Result<http::HeaderValue, Error> {
        http::HeaderValue::from_str(&format!(
            "session={}; Path=/; SameSite=Lax; Expires=Fri, 31 Dec 9999 23:59:59 GMT;",
            self.token
        ))
        .with_kind(crate::ErrorKind::Unknown)
    }

    pub fn hashed(&self) -> &str {
        self.hashed.as_str()
    }

    pub fn as_hash(self) -> String {
        self.hashed
    }
    fn hash(token: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(token.as_bytes());
        base32::encode(
            base32::Alphabet::RFC4648 { padding: false },
            hasher.finalize().as_slice(),
        )
        .to_lowercase()
    }
}
impl AsLogoutSessionId for HashSessionToken {
    fn as_logout_session_id(self) -> String {
        self.hashed
    }
}
impl PartialEq for HashSessionToken {
    fn eq(&self, other: &Self) -> bool {
        self.hashed == other.hashed
    }
}
impl Eq for HashSessionToken {}
impl PartialOrd for HashSessionToken {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.hashed.partial_cmp(&other.hashed)
    }
}
impl Ord for HashSessionToken {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.hashed.cmp(&other.hashed)
    }
}
impl Borrow<String> for HashSessionToken {
    fn borrow(&self) -> &String {
        &self.hashed
    }
}

pub fn auth<M: Metadata>(ctx: RpcContext) -> DynMiddleware<M> {
    let rate_limiter = Arc::new(Mutex::new(Instant::now()));
    Box::new(
        move |req: &mut Request<Body>,
              metadata: M|
              -> BoxFuture<Result<Result<DynMiddlewareStage2, Response<Body>>, HttpError>> {
            let ctx = ctx.clone();
            let rate_limiter = rate_limiter.clone();
            async move {
                let mut header_stub = Request::new(Body::empty());
                *header_stub.headers_mut() = req.headers().clone();
                let m2: DynMiddlewareStage2 = Box::new(move |req, rpc_req| {
                    async move {
                        if let Err(e) = HasValidSession::from_request_parts(req, &ctx).await {
                            if metadata
                                .get(rpc_req.method.as_str(), "authenticated")
                                .unwrap_or(true)
                            {
                                let (res_parts, _) = Response::new(()).into_parts();
                                return Ok(Err(to_response(
                                    &req.headers,
                                    res_parts,
                                    Err(e.into()),
                                    |_| StatusCode::OK,
                                )?));
                            } else {
                                let mut guard = rate_limiter.lock().await;
                                if guard.elapsed() < Duration::from_secs(10) {
                                    let (res_parts, _) = Response::new(()).into_parts();
                                    return Ok(Err(to_response(
                                        &req.headers,
                                        res_parts,
                                        Err(Error::new(
                                            eyre!(
                                                "Please limit login attempts to 1 per 10 seconds."
                                            ),
                                            crate::ErrorKind::RateLimited,
                                        )
                                        .into()),
                                        |_| StatusCode::OK,
                                    )?));
                                }
                                *guard = Instant::now();
                            }
                        }
                        Ok(Ok(noop3()))
                    }
                    .boxed()
                });
                Ok(Ok(m2))
            }
            .boxed()
        },
    )
}
