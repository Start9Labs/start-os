pub mod model;
pub mod util;

use std::borrow::Cow;
use std::future::Future;
use std::sync::Arc;
use std::time::Duration;

use color_eyre::eyre::eyre;
use futures::{FutureExt, SinkExt, StreamExt};
use patch_db::json_ptr::JsonPointer;
use patch_db::{Dump, Revision};
use rpc_toolkit::command;
use rpc_toolkit::hyper::upgrade::Upgraded;
use rpc_toolkit::hyper::{Body, Error as HyperError, Request, Response};
use rpc_toolkit::yajrc::{GenericRpcMethod, RpcError, RpcResponse};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::{broadcast, oneshot};
use tokio::task::JoinError;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::WebSocketStream;
use tracing::instrument;

pub use self::model::DatabaseModel;
use self::util::WithRevision;
use crate::context::RpcContext;
use crate::middleware::auth::{HasValidSession, HashSessionToken};
use crate::util::{display_serializable, GeneralGuard, IoFormat};
use crate::{Error, ResultExt};

async fn ws_handler<
    WSFut: Future<Output = Result<Result<WebSocketStream<Upgraded>, HyperError>, JoinError>>,
>(
    ctx: RpcContext,
    ws_fut: WSFut,
) -> Result<(), Error> {
    let (dump, sub) = ctx.db.dump_and_sub().await;
    let mut stream = ws_fut
        .await
        .with_kind(crate::ErrorKind::Network)?
        .with_kind(crate::ErrorKind::Unknown)?;

    // add 1 to the session counter and issue an RAII guard to subtract 1 on drop
    ctx.websocket_count
        .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    let _decrementer = GeneralGuard::new(|| {
        let new_count = ctx
            .websocket_count
            .fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
        if new_count == 0 {
            ctx.log_epoch
                .store(rand::random(), std::sync::atomic::Ordering::SeqCst)
        }
        ()
    });

    let (has_valid_session, token) = loop {
        if let Some(Message::Text(cookie)) = stream
            .next()
            .await
            .transpose()
            .with_kind(crate::ErrorKind::Network)?
        {
            let cookie_str = serde_json::from_str::<Cow<str>>(&cookie)
                .with_kind(crate::ErrorKind::Deserialization)?;

            let id = basic_cookies::Cookie::parse(&cookie_str)
                .with_kind(crate::ErrorKind::Authorization)?
                .into_iter()
                .find(|c| c.get_name() == "session")
                .ok_or_else(|| {
                    Error::new(eyre!("UNAUTHORIZED"), crate::ErrorKind::Authorization)
                })?;
            let authenticated_session = HashSessionToken::from_cookie(&id);
            match HasValidSession::from_session(&authenticated_session, &ctx).await {
                Err(e) => {
                    stream
                        .send(Message::Text(
                            serde_json::to_string(
                                &RpcResponse::<GenericRpcMethod<String>>::from_result(Err::<
                                    _,
                                    RpcError,
                                >(
                                    e.into()
                                )),
                            )
                            .with_kind(crate::ErrorKind::Serialization)?,
                        ))
                        .await
                        .with_kind(crate::ErrorKind::Network)?;
                    return Ok(());
                }
                Ok(has_validation) => break (has_validation, authenticated_session),
            }
        }
    };
    let kill = subscribe_to_session_kill(&ctx, token).await;
    send_dump(has_valid_session, &mut stream, dump).await?;

    deal_with_messages(has_valid_session, kill, sub, stream).await?;
    Ok(())
}

async fn subscribe_to_session_kill(
    ctx: &RpcContext,
    token: HashSessionToken,
) -> oneshot::Receiver<()> {
    let (send, recv) = oneshot::channel();
    let mut guard = ctx.open_authed_websockets.lock().await;
    if !guard.contains_key(&token) {
        guard.insert(token, vec![send]);
    } else {
        guard.get_mut(&token).unwrap().push(send);
    }
    recv
}

async fn deal_with_messages(
    _has_valid_authentication: HasValidSession,
    mut kill: oneshot::Receiver<()>,
    mut sub: broadcast::Receiver<Arc<Revision>>,
    mut stream: WebSocketStream<Upgraded>,
) -> Result<(), Error> {
    loop {
        futures::select! {
            _ = (&mut kill).fuse() => {
                tracing::info!("Closing WebSocket: Reason: Session Terminated");
                return Ok(())
            }
            new_rev = sub.recv().fuse() => {
                let rev = new_rev.with_kind(crate::ErrorKind::Database)?;
                stream
                    .send(Message::Text(
                        serde_json::to_string(
                            &RpcResponse::<GenericRpcMethod<String>>::from_result(Ok::<_, RpcError>(
                                serde_json::to_value(&rev).with_kind(crate::ErrorKind::Serialization)?,
                            )),
                        )
                        .with_kind(crate::ErrorKind::Serialization)?,
                    ))
                    .await
                    .with_kind(crate::ErrorKind::Network)?;
            }
            message = stream.next().fuse() => {
                match message.transpose().with_kind(crate::ErrorKind::Network)? {
                    Some(Message::Ping(a)) => {
                        stream
                            .send(Message::Pong(a))
                            .await
                            .with_kind(crate::ErrorKind::Network)?;
                    }
                    Some(Message::Close(frame)) => {
                        if let Some(reason) = frame.as_ref() {
                            tracing::info!("Closing WebSocket: Reason: {} {}", reason.code, reason.reason);
                        } else {
                            tracing::info!("Closing WebSocket: Reason: Unknown");
                        }
                        stream
                            .send(Message::Close(frame))
                            .await
                            .with_kind(crate::ErrorKind::Network)?;
                        return Ok(())
                    }
                    _ => (),
                }
            }
            _ = tokio::time::sleep(Duration::from_secs(10)).fuse() => {
                stream
                    .send(Message::Ping(Vec::new()))
                    .await
                    .with_kind(crate::ErrorKind::Network)?;
            }
        }
    }
}

async fn send_dump(
    _has_valid_authentication: HasValidSession,
    stream: &mut WebSocketStream<Upgraded>,
    dump: Dump,
) -> Result<(), Error> {
    stream
        .send(Message::Text(
            serde_json::to_string(&RpcResponse::<GenericRpcMethod<String>>::from_result(Ok::<
                _,
                RpcError,
            >(
                serde_json::to_value(&dump).with_kind(crate::ErrorKind::Serialization)?,
            )))
            .with_kind(crate::ErrorKind::Serialization)?,
        ))
        .await
        .with_kind(crate::ErrorKind::Network)?;
    Ok(())
}

pub async fn subscribe(ctx: RpcContext, req: Request<Body>) -> Result<Response<Body>, Error> {
    let (parts, body) = req.into_parts();
    let req = Request::from_parts(parts, body);
    let (res, ws_fut) = hyper_ws_listener::create_ws(req).with_kind(crate::ErrorKind::Network)?;
    if let Some(ws_fut) = ws_fut {
        tokio::task::spawn(async move {
            match ws_handler(ctx, ws_fut).await {
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

#[command(subcommands(revisions, dump, put))]
pub fn db() -> Result<(), RpcError> {
    Ok(())
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum RevisionsRes {
    Revisions(Vec<Arc<Revision>>),
    Dump(Dump),
}

#[command(display(display_serializable))]
pub async fn revisions(
    #[context] ctx: RpcContext,
    #[arg] since: u64,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<RevisionsRes, RpcError> {
    let cache = ctx.revision_cache.read().await;
    if cache
        .front()
        .map(|rev| rev.id <= since + 1)
        .unwrap_or(false)
    {
        Ok(RevisionsRes::Revisions(
            cache
                .iter()
                .skip_while(|rev| rev.id < since + 1)
                .cloned()
                .collect(),
        ))
    } else {
        drop(cache);
        Ok(RevisionsRes::Dump(ctx.db.dump().await))
    }
}

#[command(display(display_serializable))]
pub async fn dump(
    #[context] ctx: RpcContext,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<Dump, RpcError> {
    Ok(ctx.db.dump().await)
}

#[command(subcommands(ui))]
pub fn put() -> Result<(), RpcError> {
    Ok(())
}

#[command(display(display_serializable))]
#[instrument(skip(ctx))]
pub async fn ui(
    #[context] ctx: RpcContext,
    #[arg] pointer: JsonPointer,
    #[arg] value: Value,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<WithRevision<()>, Error> {
    let ptr = "/ui"
        .parse::<JsonPointer>()
        .with_kind(crate::ErrorKind::Database)?
        + &pointer;
    Ok(WithRevision {
        response: (),
        revision: ctx.db.put(&ptr, &value, None).await?,
    })
}
