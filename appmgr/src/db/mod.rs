pub mod model;
pub mod util;

use std::future::Future;
use std::sync::Arc;

use futures::{SinkExt, StreamExt};
use patch_db::json_ptr::JsonPointer;
use patch_db::{DiffPatch, Dump, Revision};
use rpc_toolkit::command;
use rpc_toolkit::hyper::upgrade::Upgraded;
use rpc_toolkit::hyper::{Body, Error as HyperError, Request, Response};
use rpc_toolkit::yajrc::RpcError;
use serde::Serialize;
use serde_json::Value;
use tokio::task::JoinError;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::WebSocketStream;

pub use self::model::DatabaseModel;
use self::util::WithRevision;
use crate::context::RpcContext;
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
    stream.next().await;
    stream
        .send(Message::Text(
            rpc_toolkit::serde_json::to_string(&dump).with_kind(crate::ErrorKind::Serialization)?,
        ))
        .await
        .with_kind(crate::ErrorKind::Network)?;

    loop {
        let rev = sub.recv().await.with_kind(crate::ErrorKind::Database)?;
        stream
            .send(Message::Text(
                rpc_toolkit::serde_json::to_string(&rev)
                    .with_kind(crate::ErrorKind::Serialization)?,
            ))
            .await
            .with_kind(crate::ErrorKind::Network)?;
    }
}

pub async fn subscribe(ctx: RpcContext, req: Request<Body>) -> Result<Response<Body>, Error> {
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

#[command(subcommands(revisions, dump, put, patch))]
pub fn db(#[context] ctx: RpcContext) -> Result<RpcContext, RpcError> {
    Ok(ctx)
}

#[derive(Serialize)]
#[serde(untagged)]
pub enum RevisionsRes {
    Revisions(Vec<Arc<Revision>>),
    Dump(Dump),
}

#[command(rpc_only)]
pub async fn revisions(
    #[context] ctx: RpcContext,
    #[arg] since: u64,
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

#[command(rpc_only)]
pub async fn dump(#[context] ctx: RpcContext) -> Result<Dump, RpcError> {
    Ok(ctx.db.dump().await)
}

#[command(subcommands(ui))]
pub fn put(#[context] ctx: RpcContext) -> Result<RpcContext, RpcError> {
    Ok(ctx)
}

#[command(rpc_only)]
pub async fn ui(
    #[context] ctx: RpcContext,
    #[arg] pointer: JsonPointer,
    #[arg] value: Value,
) -> Result<WithRevision<()>, RpcError> {
    let ptr = "/ui".parse::<JsonPointer>()? + &pointer;
    Ok(WithRevision {
        response: (),
        revision: ctx.db.put(&ptr, &value, None).await?,
    })
}

#[command(rpc_only)]
pub async fn patch(
    #[context] ctx: RpcContext,
    #[arg] patch: DiffPatch,
) -> Result<WithRevision<()>, RpcError> {
    Ok(WithRevision {
        response: (),
        revision: ctx.db.apply(patch, None, None).await?,
    })
}
