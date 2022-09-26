pub mod model;
pub mod package;

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
use tokio::sync::oneshot;
use tokio::task::JoinError;
use tokio_tungstenite::tungstenite::protocol::frame::coding::CloseCode;
use tokio_tungstenite::tungstenite::protocol::CloseFrame;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::WebSocketStream;
use tracing::instrument;

pub use self::model::DatabaseModel;
use crate::context::RpcContext;
use crate::middleware::auth::{HasValidSession, HashSessionToken};
use crate::util::serde::{display_serializable, IoFormat};
use crate::{Error, ResultExt};

#[instrument(skip(ctx, session, ws_fut))]
async fn ws_handler<
    WSFut: Future<Output = Result<Result<WebSocketStream<Upgraded>, HyperError>, JoinError>>,
>(
    ctx: RpcContext,
    session: Option<(HasValidSession, HashSessionToken)>,
    ws_fut: WSFut,
) -> Result<(), Error> {
    let (dump, sub) = ctx.db.dump_and_sub().await?;
    let mut stream = ws_fut
        .await
        .with_kind(crate::ErrorKind::Network)?
        .with_kind(crate::ErrorKind::Unknown)?;

    if let Some((session, token)) = session {
        let kill = subscribe_to_session_kill(&ctx, token).await;
        send_dump(session, &mut stream, dump).await?;

        deal_with_messages(session, kill, sub, stream).await?;
    } else {
        stream
            .close(Some(CloseFrame {
                code: CloseCode::Error,
                reason: "UNAUTHORIZED".into(),
            }))
            .await
            .with_kind(crate::ErrorKind::Network)?;
    }

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

#[instrument(skip(_has_valid_authentication, kill, sub, stream))]
async fn deal_with_messages(
    _has_valid_authentication: HasValidSession,
    mut kill: oneshot::Receiver<()>,
    mut sub: patch_db::Subscriber,
    mut stream: WebSocketStream<Upgraded>,
) -> Result<(), Error> {
    loop {
        futures::select! {
            _ = (&mut kill).fuse() => {
                tracing::info!("Closing WebSocket: Reason: Session Terminated");
                stream
                    .close(Some(CloseFrame {
                        code: CloseCode::Error,
                        reason: "UNAUTHORIZED".into(),
                    }))
                    .await
                    .with_kind(crate::ErrorKind::Network)?;
                return Ok(())
            }
            new_rev = sub.recv().fuse() => {
                let rev = new_rev.expect("UNREACHABLE: patch-db is dropped");
                stream
                    .send(Message::Text(serde_json::to_string(&rev).with_kind(crate::ErrorKind::Serialization)?))
                    .await
                    .with_kind(crate::ErrorKind::Network)?;
            }
            message = stream.next().fuse() => {
                let message = message.transpose().with_kind(crate::ErrorKind::Network)?;
                match message {
                    None => {
                        tracing::info!("Closing WebSocket: Stream Finished");
                        return Ok(())
                    }
                    _ => (),
                }
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
            serde_json::to_string(&dump).with_kind(crate::ErrorKind::Serialization)?,
        ))
        .await
        .with_kind(crate::ErrorKind::Network)?;
    Ok(())
}

pub async fn subscribe(ctx: RpcContext, req: Request<Body>) -> Result<Response<Body>, Error> {
    let (parts, body) = req.into_parts();
    let session = match async {
        let token = HashSessionToken::from_request_parts(&parts)?;
        let session = HasValidSession::from_session(&token, &ctx).await?;
        Ok::<_, Error>((session, token))
    }
    .await
    {
        Ok(a) => Some(a),
        Err(e) => {
            if e.kind != crate::ErrorKind::Authorization {
                tracing::error!("Error Authenticating Websocket: {}", e);
                tracing::debug!("{:?}", e);
            }
            None
        }
    };
    let req = Request::from_parts(parts, body);
    let (res, ws_fut) = hyper_ws_listener::create_ws(req).with_kind(crate::ErrorKind::Network)?;
    if let Some(ws_fut) = ws_fut {
        tokio::task::spawn(async move {
            match ws_handler(ctx, session, ws_fut).await {
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
) -> Result<RevisionsRes, Error> {
    Ok(match ctx.db.sync(since).await? {
        Ok(revs) => RevisionsRes::Revisions(revs),
        Err(dump) => RevisionsRes::Dump(dump),
    })
}

#[command(display(display_serializable))]
pub async fn dump(
    #[context] ctx: RpcContext,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<Dump, Error> {
    Ok(ctx.db.dump().await?)
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
) -> Result<(), Error> {
    let ptr = "/ui"
        .parse::<JsonPointer>()
        .with_kind(crate::ErrorKind::Database)?
        + &pointer;
    ctx.db.put(&ptr, &value).await?;
    Ok(())
}
