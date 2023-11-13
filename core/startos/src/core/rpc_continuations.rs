use std::sync::Arc;
use std::time::Duration;

use futures::future::BoxFuture;
use futures::FutureExt;
use helpers::TimedResource;
use hyper::upgrade::Upgraded;
use hyper::{Body, Error as HyperError, Request, Response};
use rand::RngCore;
use tokio::task::JoinError;
use tokio_tungstenite::WebSocketStream;

use crate::{Error, ResultExt};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub struct RequestGuid<T: AsRef<str> = String>(Arc<T>);
impl RequestGuid {
    pub fn new() -> Self {
        let mut buf = [0; 40];
        rand::thread_rng().fill_bytes(&mut buf);
        RequestGuid(Arc::new(base32::encode(
            base32::Alphabet::RFC4648 { padding: false },
            &buf,
        )))
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
        Some(RequestGuid(Arc::new(r.to_owned())))
    }
}
#[test]
fn parse_guid() {
    println!(
        "{:?}",
        RequestGuid::from(&format!("{}", RequestGuid::new()))
    )
}

impl<T: AsRef<str>> std::fmt::Display for RequestGuid<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (&*self.0).as_ref().fmt(f)
    }
}

pub type RestHandler = Box<
    dyn FnOnce(Request<Body>) -> BoxFuture<'static, Result<Response<Body>, crate::Error>> + Send,
>;

pub type WebSocketHandler = Box<
    dyn FnOnce(
            BoxFuture<'static, Result<Result<WebSocketStream<Upgraded>, HyperError>, JoinError>>,
        ) -> BoxFuture<'static, Result<(), Error>>
        + Send,
>;

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
    pub async fn into_handler(self) -> Option<RestHandler> {
        match self {
            RpcContinuation::Rest(handler) => handler.get().await,
            RpcContinuation::WebSocket(handler) => {
                if let Some(handler) = handler.get().await {
                    Some(Box::new(
                        |req: Request<Body>| -> BoxFuture<'static, Result<Response<Body>, Error>> {
                            async move {
                                let (parts, body) = req.into_parts();
                                let req = Request::from_parts(parts, body);
                                let (res, ws_fut) = hyper_ws_listener::create_ws(req)
                                    .with_kind(crate::ErrorKind::Network)?;
                                if let Some(ws_fut) = ws_fut {
                                    tokio::task::spawn(async move {
                                        match handler(ws_fut.boxed()).await {
                                            Ok(()) => (),
                                            Err(e) => {
                                                tracing::error!("WebSocket Closed: {}", e);
                                                tracing::debug!("{:?}", e);
                                            }
                                        }
                                    });
                                }

                                Ok(res)
                            }
                            .boxed()
                        },
                    ))
                } else {
                    None
                }
            }
        }
    }
}
