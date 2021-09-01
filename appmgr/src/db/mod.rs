pub mod model;
pub mod util;

use std::future::Future;
use std::sync::Arc;

use futures::{FutureExt, SinkExt, StreamExt};
use patch_db::json_ptr::JsonPointer;
use patch_db::{Dump, Revision};
use rpc_toolkit::command;
use rpc_toolkit::hyper::upgrade::Upgraded;
use rpc_toolkit::hyper::{Body, Error as HyperError, Request, Response};
use rpc_toolkit::yajrc::RpcError;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::task::JoinError;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::WebSocketStream;

pub use self::model::DatabaseModel;
use self::util::WithRevision;
use crate::context::RpcContext;
use crate::util::{display_serializable, IoFormat};
use crate::{Error, ResultExt};

async fn ws_handler<
    WSFut: Future<Output = Result<Result<WebSocketStream<Upgraded>, HyperError>, JoinError>>,
>(
    ctx: RpcContext,
    ws_fut: WSFut,
) -> Result<(), Error> {
    let (dump, mut sub) = ctx.db.dump_and_sub().await;
    let mut stream = ws_fut
        .await
        .with_kind(crate::ErrorKind::Network)?
        .with_kind(crate::ErrorKind::Unknown)?;
    loop {
        if let Some(Message::Text(_)) = stream
            .next()
            .await
            .transpose()
            .with_kind(crate::ErrorKind::Network)?
        {
            // TODO: check auth
            break;
        }
    }
    stream
        .send(Message::Text(
            rpc_toolkit::serde_json::to_string(&dump).with_kind(crate::ErrorKind::Serialization)?,
        ))
        .await
        .with_kind(crate::ErrorKind::Network)?;

    loop {
        futures::select! {
            new_rev = sub.recv().fuse() => {
                let rev = new_rev.with_kind(crate::ErrorKind::Database)?;
                stream
                    .send(Message::Text(
                        rpc_toolkit::serde_json::to_string(&rev)
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
                            log::info!("Closing WebSocket: Reason: {} {}", reason.code, reason.reason);
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
        }
    }
}

pub async fn subscribe(ctx: RpcContext, req: Request<Body>) -> Result<Response<Body>, Error> {
    let (parts, body) = req.into_parts();
    let req = Request::from_parts(parts, body);
    let (res, ws_fut) = hyper_ws_listener::create_ws(req).with_kind(crate::ErrorKind::Network)?;
    if let Some(ws_fut) = ws_fut {
        tokio::task::spawn(async move {
            match ws_handler(ctx, ws_fut).await {
                Ok(()) => (),
                Err(e) => log::error!("WebSocket Closed: {}", e),
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
pub async fn ui(
    #[context] ctx: RpcContext,
    #[arg] pointer: JsonPointer,
    #[arg] value: Value,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<WithRevision<()>, RpcError> {
    let ptr = "/ui".parse::<JsonPointer>()? + &pointer;
    Ok(WithRevision {
        response: (),
        revision: Some(ctx.db.put(&ptr, &value, None).await?),
    })
}
