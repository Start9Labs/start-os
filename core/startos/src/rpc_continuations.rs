use std::collections::BTreeMap;
use std::str::FromStr;
use std::time::Duration;

use axum::extract::ws::WebSocket;
use axum::extract::Request;
use axum::response::Response;
use clap::builder::ValueParserFactory;
use futures::future::BoxFuture;
use helpers::TimedResource;
use imbl_value::InternedString;
use tokio::sync::Mutex;
use ts_rs::TS;

#[allow(unused_imports)]
use crate::prelude::*;
use crate::util::clap::FromStrParser;
use crate::util::new_guid;

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize, TS,
)]
#[ts(type = "string")]
pub struct Guid(InternedString);
impl Guid {
    pub fn new() -> Self {
        Self(new_guid())
    }

    pub fn from(r: &str) -> Option<Guid> {
        if r.len() != 32 {
            return None;
        }
        for c in r.chars() {
            if !(c >= 'A' && c <= 'Z' || c >= '2' && c <= '7') {
                return None;
            }
        }
        Some(Guid(InternedString::intern(r)))
    }
}
impl Default for Guid {
    fn default() -> Self {
        Self::new()
    }
}
impl AsRef<str> for Guid {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl FromStr for Guid {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from(s).ok_or_else(|| Error::new(eyre!("invalid guid"), ErrorKind::Deserialization))
    }
}
impl ValueParserFactory for Guid {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        Self::Parser::new()
    }
}

#[test]
fn parse_guid() {
    println!("{:?}", Guid::from(&format!("{}", Guid::new())))
}

impl std::fmt::Display for Guid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub type RestHandler =
    Box<dyn FnOnce(Request) -> BoxFuture<'static, Result<Response, crate::Error>> + Send>;

pub type WebSocketHandler = Box<dyn FnOnce(WebSocket) -> BoxFuture<'static, ()> + Send>;

pub enum RpcContinuation {
    Rest(TimedResource<RestHandler>),
    WebSocket(TimedResource<WebSocketHandler>),
}
impl RpcContinuation {
    pub fn rest(handler: RestHandler, timeout: Duration) -> Self {
        RpcContinuation::Rest(TimedResource::new(handler, timeout))
    }
    pub fn ws(handler: WebSocketHandler, timeout: Duration) -> Self {
        RpcContinuation::WebSocket(TimedResource::new(handler, timeout))
    }
    pub fn is_timed_out(&self) -> bool {
        match self {
            RpcContinuation::Rest(a) => a.is_timed_out(),
            RpcContinuation::WebSocket(a) => a.is_timed_out(),
        }
    }
}

pub struct RpcContinuations(Mutex<BTreeMap<Guid, RpcContinuation>>);
impl RpcContinuations {
    pub fn new() -> Self {
        RpcContinuations(Mutex::new(BTreeMap::new()))
    }

    #[instrument(skip_all)]
    pub async fn clean(&self) {
        let mut continuations = self.0.lock().await;
        let mut to_remove = Vec::new();
        for (guid, cont) in &*continuations {
            if cont.is_timed_out() {
                to_remove.push(guid.clone());
            }
        }
        for guid in to_remove {
            continuations.remove(&guid);
        }
    }

    #[instrument(skip_all)]
    pub async fn add(&self, guid: Guid, handler: RpcContinuation) {
        self.clean().await;
        self.0.lock().await.insert(guid, handler);
    }

    pub async fn get_ws_handler(&self, guid: &Guid) -> Option<WebSocketHandler> {
        let mut continuations = self.0.lock().await;
        if !matches!(continuations.get(guid), Some(RpcContinuation::WebSocket(_))) {
            return None;
        }
        let Some(RpcContinuation::WebSocket(x)) = continuations.remove(guid) else {
            return None;
        };
        x.get().await
    }

    pub async fn get_rest_handler(&self, guid: &Guid) -> Option<RestHandler> {
        let mut continuations: tokio::sync::MutexGuard<'_, BTreeMap<Guid, RpcContinuation>> =
            self.0.lock().await;
        if !matches!(continuations.get(guid), Some(RpcContinuation::Rest(_))) {
            return None;
        }
        let Some(RpcContinuation::Rest(x)) = continuations.remove(guid) else {
            return None;
        };
        x.get().await
    }
}
