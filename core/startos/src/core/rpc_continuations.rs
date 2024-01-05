use std::time::Duration;

use axum::{body::Body, extract::ws::WebSocket};
use futures::future::BoxFuture;
use futures::FutureExt;
use helpers::TimedResource;
use hyper::upgrade::Upgraded;
use hyper::{Error as HyperError, Request, Response};
use imbl_value::InternedString;
use tokio::task::JoinError;
use tokio_tungstenite::WebSocketStream;

use crate::util::new_guid;
use crate::{Error, ResultExt};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub struct RequestGuid(InternedString);
impl RequestGuid {
    pub fn new() -> Self {
        Self(new_guid())
    }

    pub fn from(r: &str) -> Option<RequestGuid> {
        if r.len() != 64 {
            return None;
        }
        for c in r.chars() {
            if !(c >= 'A' && c <= 'Z' || c >= '2' && c <= '7') {
                return None;
            }
        }
        Some(RequestGuid(InternedString::intern(r)))
    }
}
#[test]
fn parse_guid() {
    println!(
        "{:?}",
        RequestGuid::from(&format!("{}", RequestGuid::new()))
    )
}

impl std::fmt::Display for RequestGuid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub type RestHandler = Box<
    dyn FnOnce(Request<Body>) -> BoxFuture<'static, Result<Response<Body>, crate::Error>> + Send,
>;

pub type WebSocketHandler =
    Box<dyn FnOnce(WebSocket) -> BoxFuture<'static, Result<(), Error>> + Send>;

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
