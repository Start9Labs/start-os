pub mod model;
pub mod package;
pub mod prelude;

use std::future::Future;
use std::path::PathBuf;
use std::sync::Arc;

use clap::Parser;
use futures::{FutureExt, SinkExt, StreamExt};
use patch_db::json_ptr::JsonPointer;
use patch_db::{Dump, Revision};
use rpc_toolkit::hyper::upgrade::Upgraded;
use rpc_toolkit::hyper::{Error as HyperError, Request, Response};
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{command, from_fn_async, ParentHandler};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::oneshot;
use tokio::task::JoinError;
use tokio_tungstenite::tungstenite::protocol::frame::coding::CloseCode;
use tokio_tungstenite::tungstenite::protocol::CloseFrame;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::WebSocketStream;
use tracing::instrument;

use crate::context::{CliContext, RpcContext};
use crate::middleware::auth::{HasValidSession, HashSessionToken};
use crate::prelude::*;
use crate::util::serde::IoFormat;

#[instrument(skip_all)]
async fn ws_handler<
    WSFut: Future<Output = Result<Result<WebSocketStream<Upgraded>, HyperError>, JoinError>>,
>(
    ctx: RpcContext,
    session: Option<(HasValidSession, HashSessionToken)>,
    ws_fut: WSFut,
) -> Result<(), Error> {
    let (dump, sub) = ctx.db.dump_and_sub().await;
    let mut stream = ws_fut
        .await
        .with_kind(ErrorKind::Network)?
        .with_kind(ErrorKind::Unknown)?;

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
            .with_kind(ErrorKind::Network)?;
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

#[instrument(skip_all)]
async fn deal_with_messages(
    _has_valid_authentication: HasValidSession,
    mut kill: oneshot::Receiver<()>,
    mut sub: patch_db::Subscriber,
    mut stream: WebSocketStream<Upgraded>,
) -> Result<(), Error> {
    let mut timer = tokio::time::interval(tokio::time::Duration::from_secs(5));

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
                    .with_kind(ErrorKind::Network)?;
                return Ok(())
            }
            new_rev = sub.recv().fuse() => {
                let rev = new_rev.expect("UNREACHABLE: patch-db is dropped");
                stream
                    .send(Message::Text(serde_json::to_string(&rev).with_kind(ErrorKind::Serialization)?))
                    .await
                    .with_kind(ErrorKind::Network)?;
            }
            message = stream.next().fuse() => {
                let message = message.transpose().with_kind(ErrorKind::Network)?;
                match message {
                    None => {
                        tracing::info!("Closing WebSocket: Stream Finished");
                        return Ok(())
                    }
                    _ => (),
                }
            }
            // This is trying to give a health checks to the home to keep the ui alive.
            _ = timer.tick().fuse() => {
                stream
                    .send(Message::Ping(vec![]))
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
            serde_json::to_string(&dump).with_kind(ErrorKind::Serialization)?,
        ))
        .await
        .with_kind(ErrorKind::Network)?;
    Ok(())
}

pub async fn subscribe(ctx: RpcContext, req: Request<Body>) -> Result<Response<Body>, Error> {
    let (parts, body) = req.into_parts();
    let session = match async {
        let token = HashSessionToken::from_request_parts(&parts)?;
        let session = HasValidSession::from_request_parts(&parts, &ctx).await?;
        Ok::<_, Error>((session, token))
    }
    .await
    {
        Ok(a) => Some(a),
        Err(e) => {
            if e.kind != ErrorKind::Authorization {
                tracing::error!("Error Authenticating Websocket: {}", e);
                tracing::debug!("{:?}", e);
            }
            None
        }
    };
    let req = Request::from_parts(parts, body);
    let (res, ws_fut) = hyper_ws_listener::create_ws(req).with_kind(ErrorKind::Network)?;
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

pub fn db() -> ParentHandler {
    ParentHandler::new()
        .subcommand("dump", from_fn_async(dump).with_remote_cli::<CliContext>())
        .subcommand("put", put())
        .subcommand(
            "apply",
            from_fn_async(apply)
                .no_display()
                .with_remote_cli::<CliContext>(),
        )
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum RevisionsRes {
    Revisions(Vec<Arc<Revision>>),
    Dump(Dump),
}

#[instrument(skip_all)]
async fn cli_dump(
    ctx: CliContext,
    _format: Option<IoFormat>,
    path: Option<PathBuf>,
) -> Result<Dump, RpcError> {
    let dump = if let Some(path) = path {
        PatchDb::open(path).await?.dump().await
    } else {
        rpc_toolkit::command_helpers::call_remote(
            ctx,
            "db.dump",
            serde_json::json!({}),
            std::marker::PhantomData::<Dump>,
        )
        .await?
        .result?
    };

    Ok(dump)
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct DumpParams {
    #[arg(long = "format")]
    format: Option<IoFormat>,
    path: Option<PathBuf>,
}

// #[command(
//     custom_cli(cli_dump(async, context(CliContext))),
//     display(display_serializable)
// )]
pub async fn dump(ctx: RpcContext, _: DumpParams) -> Result<Dump, Error> {
    Ok(ctx.db.dump().await)
}

fn apply_expr(input: jaq_core::Val, expr: &str) -> Result<jaq_core::Val, Error> {
    let (expr, errs) = jaq_core::parse::parse(expr, jaq_core::parse::main());

    let Some(expr) = expr else {
        return Err(Error::new(
            eyre!("Failed to parse expression: {:?}", errs),
            crate::ErrorKind::InvalidRequest,
        ));
    };

    let mut errs = Vec::new();

    let mut defs = jaq_core::Definitions::core();
    for def in jaq_std::std() {
        defs.insert(def, &mut errs);
    }

    let filter = defs.finish(expr, Vec::new(), &mut errs);

    if !errs.is_empty() {
        return Err(Error::new(
            eyre!("Failed to compile expression: {:?}", errs),
            crate::ErrorKind::InvalidRequest,
        ));
    };

    let inputs = jaq_core::RcIter::new(std::iter::empty());
    let mut res_iter = filter.run(jaq_core::Ctx::new([], &inputs), input);

    let Some(res) = res_iter
        .next()
        .transpose()
        .map_err(|e| eyre!("{e}"))
        .with_kind(crate::ErrorKind::Deserialization)?
    else {
        return Err(Error::new(
            eyre!("expr returned no results"),
            crate::ErrorKind::InvalidRequest,
        ));
    };

    if res_iter.next().is_some() {
        return Err(Error::new(
            eyre!("expr returned too many results"),
            crate::ErrorKind::InvalidRequest,
        ));
    }

    Ok(res)
}

#[instrument(skip_all)]
async fn cli_apply(ctx: CliContext, expr: String, path: Option<PathBuf>) -> Result<(), RpcError> {
    if let Some(path) = path {
        PatchDb::open(path)
            .await?
            .mutate(|db| {
                let res = apply_expr(
                    serde_json::to_value(patch_db::Value::from(db.clone()))
                        .with_kind(ErrorKind::Deserialization)?
                        .into(),
                    &expr,
                )?;

                db.ser(
                    &serde_json::from_value::<model::Database>(res.clone().into()).with_ctx(
                        |_| {
                            (
                                crate::ErrorKind::Deserialization,
                                "result does not match database model",
                            )
                        },
                    )?,
                )
            })
            .await?;
    } else {
        rpc_toolkit::command_helpers::call_remote(
            ctx,
            "db.apply",
            serde_json::json!({ "expr": expr }),
            std::marker::PhantomData::<()>,
        )
        .await?
        .result?;
    }

    Ok(())
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct ApplyParams {
    expr: String,
    path: Option<PathBuf>,
}

// #[command(
//     custom_cli(cli_apply(async, context(CliContext)))
// )]
pub async fn apply(ctx: RpcContext, ApplyParams { expr, path }: ApplyParams) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            let res = apply_expr(
                serde_json::to_value(patch_db::Value::from(db.clone()))
                    .with_kind(ErrorKind::Deserialization)?
                    .into(),
                &expr,
            )?;

            db.ser(
                &serde_json::from_value::<model::Database>(res.clone().into()).with_ctx(|_| {
                    (
                        crate::ErrorKind::Deserialization,
                        "result does not match database model",
                    )
                })?,
            )
        })
        .await
}

pub fn put() -> ParentHandler {
    ParentHandler::new().subcommand("ui", from_fn_async(ui).with_remote_cli::<CliContext>())
}
#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct UiParams {
    pointer: JsonPointer,
    value: Value,
    #[arg(long = "format")]
    format: Option<IoFormat>,
}

// #[command(display(display_serializable))]
#[instrument(skip_all)]
pub async fn ui(ctx: RpcContext, UiParams { pointer, value, .. }: UiParams) -> Result<(), Error> {
    let ptr = "/ui"
        .parse::<JsonPointer>()
        .with_kind(ErrorKind::Database)?
        + &pointer;
    ctx.db.put(&ptr, &value).await?;
    Ok(())
}
