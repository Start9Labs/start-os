use std::collections::HashMap;
use std::fmt;
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use axum::body::Body;
use axum::extract::Path;
use axum::http::{Request, Response, StatusCode};
use axum::response::IntoResponse;
use serde::{Deserialize, Serialize};
use tokio::sync::{broadcast, oneshot};

use crate::prelude::*;

// ── Guid ──────────────────────────────────────────────────────────────

/// Unguessable random token, used as a one-time REST endpoint path segment.
/// Wraps `startos::util::new_guid()` (160-bit, base32-encoded, 32 chars).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Guid(String);

impl Guid {
    pub fn new() -> Self {
        Self(startos::util::new_guid().to_string())
    }
}

impl fmt::Display for Guid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl AsRef<str> for Guid {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

// ── TimedResource ─────────────────────────────────────────────────────

/// A resource that auto-drops after a timeout unless claimed first.
struct TimedResource<T: Send + 'static> {
    ready: oneshot::Sender<()>,
    handle: tokio::task::JoinHandle<Option<T>>,
}

impl<T: Send + 'static> TimedResource<T> {
    fn new(resource: T, timeout: Duration) -> Self {
        let (send, recv) = oneshot::channel();
        let handle = tokio::spawn(async move {
            tokio::select! {
                _ = tokio::time::sleep(timeout) => {
                    drop(resource);
                    None
                }
                _ = recv => Some(resource),
            }
        });
        Self {
            ready: send,
            handle,
        }
    }

    /// Claim the resource. Returns `None` if it already timed out.
    async fn get(self) -> Option<T> {
        let _ = self.ready.send(());
        self.handle.await.unwrap_or(None)
    }

    /// Check if the timeout has already fired (resource was dropped).
    fn is_timed_out(&self) -> bool {
        self.ready.is_closed()
    }
}

// ── RestHandler ───────────────────────────────────────────────────────

/// Future returned by a REST continuation handler, with optional session kill signal.
pub struct RestFuture {
    kill: Option<broadcast::Receiver<()>>,
    fut: Pin<Box<dyn Future<Output = Result<Response<Body>, Error>> + Send>>,
}

impl Future for RestFuture {
    type Output = Result<Response<Body>, Error>;
    fn poll(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        if self.kill.as_ref().map_or(false, |k| !k.is_empty()) {
            std::task::Poll::Ready(Err(Error::new(eyre!("session killed"), ErrorKind::Cancelled)))
        } else {
            self.fut.as_mut().poll(cx)
        }
    }
}

pub type RestHandler = Box<dyn FnOnce(Request<Body>) -> RestFuture + Send>;

// ── RpcContinuation ───────────────────────────────────────────────────

pub struct RpcContinuation(TimedResource<RestHandler>);

pub const DEFAULT_TTL: Duration = Duration::from_secs(60);

impl RpcContinuation {
    /// Create a REST continuation.
    pub fn rest<F, Fut>(handler: F, timeout: Duration) -> Self
    where
        F: FnOnce(Request<Body>) -> Fut + Send + 'static,
        Fut: Future<Output = Result<Response<Body>, Error>> + Send + 'static,
    {
        Self(TimedResource::new(
            Box::new(|req| RestFuture {
                kill: None,
                fut: Box::pin(handler(req)),
            }),
            timeout,
        ))
    }

    /// Create a REST continuation bound to a session lifecycle. Aborts if the session is killed.
    pub fn rest_authed<F, Fut>(
        open: &OpenAuthedContinuations,
        session: &str,
        handler: F,
        timeout: Duration,
    ) -> Self
    where
        F: FnOnce(Request<Body>) -> Fut + Send + 'static,
        Fut: Future<Output = Result<Response<Body>, Error>> + Send + 'static,
    {
        let kill = Some(open.subscribe(session));
        Self(TimedResource::new(
            Box::new(|req| RestFuture {
                kill,
                fut: Box::pin(handler(req)),
            }),
            timeout,
        ))
    }

    fn is_timed_out(&self) -> bool {
        self.0.is_timed_out()
    }
}

// ── RpcContinuations (registry) ───────────────────────────────────────

/// Registry of one-shot REST handlers keyed by Guid.
#[derive(Clone)]
pub struct RpcContinuations {
    inner: Arc<Mutex<HashMap<Guid, RpcContinuation>>>,
}

impl RpcContinuations {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Register a continuation. Cleans expired entries on every call.
    pub fn add(&self, guid: Guid, cont: RpcContinuation) {
        let mut map = self.inner.lock().unwrap();
        map.retain(|_, c| !c.is_timed_out());
        map.insert(guid, cont);
    }

    /// Remove and return a handler if it exists and hasn't timed out.
    pub async fn get_rest_handler(&self, guid: &Guid) -> Option<RestHandler> {
        let cont = self.inner.lock().unwrap().remove(guid)?;
        cont.0.get().await
    }
}

// ── OpenAuthedContinuations ───────────────────────────────────────────

/// Tracks broadcast channels per session so that killing a session
/// aborts all its in-flight continuations.
#[derive(Clone)]
pub struct OpenAuthedContinuations {
    inner: Arc<Mutex<HashMap<String, broadcast::Sender<()>>>>,
}

impl OpenAuthedContinuations {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Subscribe to the kill signal for a session.
    pub fn subscribe(&self, session: &str) -> broadcast::Receiver<()> {
        let mut map = self.inner.lock().unwrap();
        if let Some(sender) = map.get(session) {
            sender.subscribe()
        } else {
            let (send, recv) = broadcast::channel(1);
            map.insert(session.to_string(), send);
            recv
        }
    }

    /// Kill all continuations for a session.
    pub fn kill(&self, session: &str) {
        if let Some(sender) = self.inner.lock().unwrap().remove(session) {
            let _ = sender.send(());
        }
    }
}

// ── Axum handler ──────────────────────────────────────────────────────

fn server_error(e: Error) -> Response<Body> {
    Response::builder()
        .status(StatusCode::INTERNAL_SERVER_ERROR)
        .body(Body::from(e.to_string()))
        .unwrap()
}

/// Axum handler for `/rest/rpc/{guid}`.
pub async fn continuation_handler(
    Path(guid): Path<String>,
    axum::Extension(continuations): axum::Extension<RpcContinuations>,
    request: Request<Body>,
) -> Response<Body> {
    let guid = Guid(guid);
    match continuations.get_rest_handler(&guid).await {
        Some(handler) => handler(request).await.unwrap_or_else(server_error),
        None => StatusCode::NOT_FOUND.into_response(),
    }
}
