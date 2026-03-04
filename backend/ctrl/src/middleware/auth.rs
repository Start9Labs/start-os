use std::borrow::Cow;
use std::sync::Arc;
use std::time::{Duration, Instant};

use axum::extract::{ConnectInfo, Request};
use axum::response::Response;
use basic_cookies::Cookie;
use http::header::{COOKIE, USER_AGENT};
use http::HeaderValue;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{Context, Middleware, RpcRequest, RpcResponse};
use serde::Deserialize;
use std::net::SocketAddr;

use crate::auth::{error_code, validate_session, HashSessionToken, LoginRes};
use crate::error::Error;

/// Simple synchronous mutex wrapper with mutate helper
pub struct SyncMutex<T>(std::sync::Mutex<T>);

impl<T> SyncMutex<T> {
    pub fn new(t: T) -> Self {
        Self(std::sync::Mutex::new(t))
    }

    pub fn mutate<F: FnOnce(&mut T) -> U, U>(&self, f: F) -> U {
        f(&mut *self.0.lock().unwrap())
    }
}

#[derive(Deserialize)]
pub struct Metadata {
    #[serde(default)]
    login: bool,
    #[serde(default)]
    get_session: bool,
    /// Bypass session validation without triggering the login rate limiter.
    /// Use for read-only status endpoints that need to be accessible without auth.
    #[serde(default)]
    no_auth: bool,
}

#[derive(Clone)]
pub struct SessionAuth {
    rate_limiter: Arc<SyncMutex<(usize, Instant)>>,
    is_login: bool,
    is_loopback: bool,
    cookie: Option<HeaderValue>,
    set_cookie: Option<HeaderValue>,
    user_agent: Option<HeaderValue>,
}

impl SessionAuth {
    pub fn new() -> Self {
        Self {
            rate_limiter: Arc::new(SyncMutex::new((0, Instant::now()))),
            is_login: false,
            is_loopback: false,
            cookie: None,
            set_cookie: None,
            user_agent: None,
        }
    }

    fn extract_session_from_cookie(&self) -> Option<HashSessionToken> {
        extract_session_token(self.cookie.as_ref()?)
    }
}

/// Extract the session token from a Cookie header value.
pub fn extract_session_token(cookie_header: &HeaderValue) -> Option<HashSessionToken> {
    let cookie_str = cookie_header.to_str().ok()?;
    let cookies = Cookie::parse(cookie_str).ok()?;
    let session_cookie = cookies.iter().find(|c| c.get_name() == "session")?;
    Some(HashSessionToken::from_token(
        session_cookie.get_value().to_string(),
    ))
}

impl<C: Context> Middleware<C> for SessionAuth {
    type Metadata = Metadata;

    async fn process_http_request(
        &mut self,
        _: &C,
        request: &mut Request,
    ) -> Result<(), Response> {
        self.cookie = request.headers().get(COOKIE).cloned();
        self.user_agent = request.headers().get(USER_AGENT).cloned();
        self.is_loopback = request
            .extensions()
            .get::<ConnectInfo<SocketAddr>>()
            .map_or(false, |ci| ci.0.ip().is_loopback());
        Ok(())
    }

    async fn process_rpc_request(
        &mut self,
        _context: &C,
        metadata: Self::Metadata,
        request: &mut RpcRequest,
    ) -> Result<(), RpcResponse> {
        let result: Result<(), Error> = async {
            if metadata.no_auth || self.is_loopback {
                // Bypass auth for read-only status endpoints and loopback
                // requests (CLI via SSH is already authenticated)
                return Ok(());
            } else if metadata.login {
                self.is_login = true;
                // Rate limit login attempts: 3 per 20 seconds
                self.rate_limiter.mutate(|(count, time)| {
                    if time.elapsed() >= Duration::from_secs(20) {
                        // Window expired, start fresh
                        *count = 1;
                        *time = Instant::now();
                        Ok(())
                    } else if *count >= 3 {
                        Err(Error::other("Login attempt limit exceeded."))
                    } else {
                        *count += 1;
                        Ok(())
                    }
                })?;
                // Inject user agent into request params for login
                if let Some(user_agent) = self.user_agent.as_ref().and_then(|h| h.to_str().ok()) {
                    request.params["userAgent"] =
                        imbl_value::Value::String(Arc::new(user_agent.to_owned()));
                }
            } else {
                // Validate session for non-login endpoints
                let session_token = self
                    .extract_session_from_cookie()
                    .ok_or_else(|| Error::other("UNAUTHORIZED"))?;

                validate_session(session_token.hashed()).await?;

                // Inject session hash into request params if requested
                if metadata.get_session {
                    request.params["sessionHash"] =
                        imbl_value::Value::String(Arc::new(session_token.hashed().to_owned()));
                }
            }
            Ok(())
        }
        .await;

        result.map_err(|e| {
            RpcResponse::from(RpcError {
                code: error_code::AUTHORIZATION,
                message: Cow::Owned(e.to_string()),
                data: None,
            })
        })
    }

    async fn process_rpc_response(&mut self, _: &C, response: &mut RpcResponse) {
        if self.is_login && response.result.is_ok() {
            // Extract session token from login response and prepare Set-Cookie header
            if let Ok(ref res) = response.result {
                if let Ok(login_res) = imbl_value::from_value::<LoginRes>(res.clone()) {
                    if let Ok(header_value) = HeaderValue::from_str(&format!(
                        "session={}; Path=/; SameSite=Strict; HttpOnly; Max-Age=86400",
                        login_res.session
                    )) {
                        self.set_cookie = Some(header_value);
                    }
                }
            }
        }
    }

    async fn process_http_response(&mut self, _: &C, response: &mut Response) {
        if let Some(set_cookie) = self.set_cookie.take() {
            response.headers_mut().insert("set-cookie", set_cookie);
        }
    }
}
