use std::borrow::Borrow;
use std::sync::Arc;
use std::time::{Duration, Instant};

use axum::extract::Request;
use axum::response::Response;
use basic_cookies::Cookie;
use color_eyre::eyre::eyre;
use digest::Digest;
use helpers::const_true;
use http::header::COOKIE;
use http::HeaderValue;
use imbl_value::InternedString;
use rpc_toolkit::yajrc::INTERNAL_ERROR;
use rpc_toolkit::{Middleware, RpcRequest, RpcResponse};
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use tokio::sync::Mutex;

use crate::context::RpcContext;
use crate::prelude::*;

pub const LOCAL_AUTH_COOKIE_PATH: &str = "/run/embassy/rpc.authcookie";

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct LoginRes {
    pub session: InternedString,
}

pub trait AsLogoutSessionId {
    fn as_logout_session_id(self) -> InternedString;
}

/// Will need to know when we have logged out from a route
#[derive(Serialize, Deserialize)]
pub struct HasLoggedOutSessions(());

impl HasLoggedOutSessions {
    pub async fn new(
        logged_out_sessions: impl IntoIterator<Item = impl AsLogoutSessionId>,
        ctx: &RpcContext,
    ) -> Result<Self, Error> {
        let mut open_authed_websockets = ctx.open_authed_websockets.lock().await;
        let mut sqlx_conn = ctx.secret_store.acquire().await?;
        for session in logged_out_sessions {
            let session = session.as_logout_session_id();
            let session = &*session;
            sqlx::query!(
                "UPDATE session SET logged_out = CURRENT_TIMESTAMP WHERE id = $1",
                session
            )
            .execute(sqlx_conn.as_mut())
            .await?;
            for socket in open_authed_websockets.remove(session).unwrap_or_default() {
                let _ = socket.send(());
            }
        }
        Ok(HasLoggedOutSessions(()))
    }
}

/// Used when we need to know that we have logged in with a valid user
#[derive(Clone)]
pub struct HasValidSession(SessionType);

#[derive(Clone)]
enum SessionType {
    Local,
    Session(HashSessionToken),
}

impl HasValidSession {
    pub async fn from_header(
        header: Option<&HeaderValue>,
        ctx: &RpcContext,
    ) -> Result<Self, Error> {
        if let Some(cookie_header) = header {
            let cookies = Cookie::parse(
                cookie_header
                    .to_str()
                    .with_kind(crate::ErrorKind::Authorization)?,
            )
            .with_kind(crate::ErrorKind::Authorization)?;
            if let Some(cookie) = cookies.iter().find(|c| c.get_name() == "local") {
                if let Ok(s) = Self::from_local(cookie).await {
                    return Ok(s);
                }
            }
            if let Some(cookie) = cookies.iter().find(|c| c.get_name() == "session") {
                if let Ok(s) = Self::from_session(HashSessionToken::from_cookie(cookie), ctx).await
                {
                    return Ok(s);
                }
            }
        }
        Err(Error::new(
            eyre!("UNAUTHORIZED"),
            crate::ErrorKind::Authorization,
        ))
    }

    pub async fn from_session(
        session_token: HashSessionToken,
        ctx: &RpcContext,
    ) -> Result<Self, Error> {
        let session_hash = session_token.hashed();
        let session = sqlx::query!("UPDATE session SET last_active = CURRENT_TIMESTAMP WHERE id = $1 AND logged_out IS NULL OR logged_out > CURRENT_TIMESTAMP", session_hash)
            .execute(ctx.secret_store.acquire().await?.as_mut())
            .await?;
        if session.rows_affected() == 0 {
            return Err(Error::new(
                eyre!("UNAUTHORIZED"),
                crate::ErrorKind::Authorization,
            ));
        }
        Ok(Self(SessionType::Session(session_token)))
    }

    pub async fn from_local(local: &Cookie<'_>) -> Result<Self, Error> {
        let token = tokio::fs::read_to_string(LOCAL_AUTH_COOKIE_PATH).await?;
        if local.get_value() == &*token {
            Ok(Self(SessionType::Local))
        } else {
            Err(Error::new(
                eyre!("UNAUTHORIZED"),
                crate::ErrorKind::Authorization,
            ))
        }
    }
}

/// When we have a need to create a new session,
/// Or when we are using internal valid authenticated service.
#[derive(Debug, Clone)]
pub struct HashSessionToken {
    hashed: InternedString,
    token: InternedString,
}
impl HashSessionToken {
    pub fn new() -> Self {
        Self::from_token(InternedString::intern(
            base32::encode(
                base32::Alphabet::RFC4648 { padding: false },
                &rand::random::<[u8; 16]>(),
            )
            .to_lowercase(),
        ))
    }

    pub fn from_token(token: InternedString) -> Self {
        let hashed = Self::hash(&*token);
        Self { hashed, token }
    }

    pub fn from_cookie(cookie: &Cookie) -> Self {
        Self::from_token(InternedString::intern(cookie.get_value()))
    }

    pub fn from_header(header: Option<&HeaderValue>) -> Result<Self, Error> {
        if let Some(cookie_header) = header {
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

    pub fn to_login_res(&self) -> LoginRes {
        LoginRes {
            session: self.token.clone(),
        }
    }

    pub fn hashed(&self) -> &str {
        &*self.hashed
    }

    fn hash(token: &str) -> InternedString {
        let mut hasher = Sha256::new();
        hasher.update(token.as_bytes());
        InternedString::intern(
            base32::encode(
                base32::Alphabet::RFC4648 { padding: false },
                hasher.finalize().as_slice(),
            )
            .to_lowercase(),
        )
    }
}
impl AsLogoutSessionId for HashSessionToken {
    fn as_logout_session_id(self) -> InternedString {
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
impl Borrow<str> for HashSessionToken {
    fn borrow(&self) -> &str {
        &*self.hashed
    }
}

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Metadata {
    #[serde(default = "const_true")]
    authenticated: bool,
    #[serde(default)]
    login: bool,
    #[serde(default)]
    get_session: bool,
}

#[derive(Clone)]
pub struct Auth {
    rate_limiter: Arc<Mutex<(usize, Instant)>>,
    cookie: Option<HeaderValue>,
    is_login: bool,
    set_cookie: Option<HeaderValue>,
}
impl Auth {
    pub fn new() -> Self {
        Self {
            rate_limiter: Arc::new(Mutex::new((0, Instant::now()))),
            cookie: None,
            is_login: false,
            set_cookie: None,
        }
    }
}
#[async_trait::async_trait]
impl Middleware<RpcContext> for Auth {
    type Metadata = Metadata;
    async fn process_http_request(
        &mut self,
        context: &RpcContext,
        request: &mut Request,
    ) -> Result<(), Response> {
        self.cookie = request.headers_mut().get(COOKIE).cloned();
        Ok(())
    }
    async fn process_rpc_request(
        &mut self,
        context: &RpcContext,
        metadata: Self::Metadata,
        request: &mut RpcRequest,
    ) -> Result<(), RpcResponse> {
        if metadata.login {
            self.is_login = true;
            let guard = self.rate_limiter.lock().await;
            if guard.1.elapsed() < Duration::from_secs(20) && guard.0 >= 3 {
                return Err(RpcResponse {
                    id: request.id.take(),
                    result: Err(Error::new(
                        eyre!("Please limit login attempts to 3 per 20 seconds."),
                        crate::ErrorKind::RateLimited,
                    )
                    .into()),
                });
            }
        } else if metadata.authenticated {
            match HasValidSession::from_header(self.cookie.as_ref(), &context).await {
                Err(e) => {
                    return Err(RpcResponse {
                        id: request.id.take(),
                        result: Err(e.into()),
                    })
                }
                Ok(HasValidSession(SessionType::Session(s))) if metadata.get_session => {
                    request.params["session"] = Value::String(Arc::new(s.hashed().into()));
                    // TODO: will this panic?
                }
                _ => (),
            }
        }
        Ok(())
    }
    async fn process_rpc_response(&mut self, context: &RpcContext, response: &mut RpcResponse) {
        if self.is_login {
            let mut guard = self.rate_limiter.lock().await;
            if guard.1.elapsed() < Duration::from_secs(20) {
                if response.result.is_err() {
                    guard.0 += 1;
                }
            } else {
                guard.0 = 0;
            }
            guard.1 = Instant::now();
            if response.result.is_ok() {
                let res = std::mem::replace(&mut response.result, Err(INTERNAL_ERROR));
                response.result = async {
                    let res = res?;
                    let login_res = from_value::<LoginRes>(res.clone())?;
                    self.set_cookie = Some(
                        HeaderValue::from_str(&format!(
                        "session={}; Path=/; SameSite=Lax; Expires=Fri, 31 Dec 9999 23:59:59 GMT;",
                        login_res.session
                    ))
                        .with_kind(crate::ErrorKind::Network)?,
                    );

                    Ok(res)
                }
                .await;
            }
        }
    }
    async fn process_http_response(&mut self, context: &RpcContext, response: &mut Response) {
        if let Some(set_cookie) = self.set_cookie.take() {
            response.headers_mut().insert("set-cookie", set_cookie);
        }
    }
}
