pub mod model;
pub mod prelude;

use std::path::PathBuf;
use std::sync::Arc;

use axum::extract::ws::{self, WebSocket};
use axum::extract::WebSocketUpgrade;
use axum::response::Response;
use clap::Parser;
use futures::{FutureExt, StreamExt};
use http::header::COOKIE;
use http::HeaderMap;
use itertools::Itertools;
use patch_db::json_ptr::{JsonPointer, ROOT};
use patch_db::{Dump, Revision};
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{from_fn_async, CallRemote, Context, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::oneshot;
use tracing::instrument;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::middleware::auth::{HasValidSession, HashSessionToken};
use crate::prelude::*;
use crate::util::serde::{apply_expr, HandlerExtSerde};

lazy_static::lazy_static! {
    static ref PUBLIC: JsonPointer = "/public".parse().unwrap();
}

#[instrument(skip_all)]
async fn ws_handler(
    ctx: RpcContext,
    session: Option<(HasValidSession, HashSessionToken)>,
    mut stream: WebSocket,
) -> Result<(), Error> {
    let (dump, sub) = ctx.db.dump_and_sub(PUBLIC.clone()).await;

    if let Some((session, token)) = session {
        let kill = subscribe_to_session_kill(&ctx, token).await;
        send_dump(session.clone(), &mut stream, dump).await?;

        deal_with_messages(session, kill, sub, stream).await?;
    } else {
        stream
            .send(ws::Message::Close(Some(ws::CloseFrame {
                code: ws::close_code::ERROR,
                reason: "UNAUTHORIZED".into(),
            })))
            .await
            .with_kind(ErrorKind::Network)?;
        drop(stream);
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
    mut stream: WebSocket,
) -> Result<(), Error> {
    let mut timer = tokio::time::interval(tokio::time::Duration::from_secs(5));

    loop {
        futures::select! {
            _ = (&mut kill).fuse() => {
                tracing::info!("Closing WebSocket: Reason: Session Terminated");
                stream
                .send(ws::Message::Close(Some(ws::CloseFrame {
                    code: ws::close_code::ERROR,
                    reason: "UNAUTHORIZED".into(),
                }))).await
                .with_kind(ErrorKind::Network)?;
                drop(stream);
                return Ok(())
            }
            new_rev = sub.recv().fuse() => {
                let rev = new_rev.expect("UNREACHABLE: patch-db is dropped");
                stream
                    .send(ws::Message::Text(serde_json::to_string(&rev).with_kind(ErrorKind::Serialization)?))
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
                    .send(ws::Message::Ping(vec![]))
                    .await
                    .with_kind(crate::ErrorKind::Network)?;
            }
        }
    }
}

async fn send_dump(
    _has_valid_authentication: HasValidSession,
    stream: &mut WebSocket,
    dump: Dump,
) -> Result<(), Error> {
    stream
        .send(ws::Message::Text(
            serde_json::to_string(&dump).with_kind(ErrorKind::Serialization)?,
        ))
        .await
        .with_kind(ErrorKind::Network)?;
    Ok(())
}

pub async fn subscribe(
    ctx: RpcContext,
    headers: HeaderMap,
    ws: WebSocketUpgrade,
) -> Result<Response, Error> {
    let session = match async {
        let token = HashSessionToken::from_header(headers.get(COOKIE))?;
        let session = HasValidSession::from_header(headers.get(COOKIE), &ctx).await?;
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
    Ok(ws.on_upgrade(|ws| async move {
        match ws_handler(ctx, session, ws).await {
            Ok(()) => (),
            Err(e) => {
                tracing::error!("WebSocket Closed: {}", e);
                tracing::debug!("{:?}", e);
            }
        }
    }))
}

pub fn db<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("dump", from_fn_async(cli_dump).with_display_serializable())
        .subcommand("dump", from_fn_async(dump).no_cli())
        .subcommand("put", put::<C>())
        .subcommand("apply", from_fn_async(cli_apply).no_display())
        .subcommand("apply", from_fn_async(apply).no_cli())
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum RevisionsRes {
    Revisions(Vec<Arc<Revision>>),
    Dump(Dump),
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct CliDumpParams {
    #[arg(long = "include-private", short = 'p')]
    #[serde(default)]
    include_private: bool,
    path: Option<PathBuf>,
}

#[instrument(skip_all)]
async fn cli_dump(
    HandlerArgs {
        context,
        parent_method,
        method,
        params: CliDumpParams {
            include_private,
            path,
        },
        ..
    }: HandlerArgs<CliContext, CliDumpParams>,
) -> Result<Dump, RpcError> {
    let dump = if let Some(path) = path {
        PatchDb::open(path).await?.dump(&ROOT).await
    } else {
        let method = parent_method.into_iter().chain(method).join(".");
        from_value::<Dump>(
            context
                .call_remote::<RpcContext>(
                    &method,
                    imbl_value::json!({ "includePrivate":include_private }),
                )
                .await?,
        )?
    };

    Ok(dump)
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct DumpParams {
    #[arg(long = "include-private", short = 'p')]
    #[serde(default)]
    #[ts(skip)]
    include_private: bool,
}

pub async fn dump(
    ctx: RpcContext,
    DumpParams { include_private }: DumpParams,
) -> Result<Dump, Error> {
    Ok(if include_private {
        ctx.db.dump(&ROOT).await
    } else {
        ctx.db.dump(&PUBLIC).await
    })
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct CliApplyParams {
    expr: String,
    path: Option<PathBuf>,
}

#[instrument(skip_all)]
async fn cli_apply(
    HandlerArgs {
        context,
        parent_method,
        method,
        params: CliApplyParams { expr, path },
        ..
    }: HandlerArgs<CliContext, CliApplyParams>,
) -> Result<(), RpcError> {
    if let Some(path) = path {
        PatchDb::open(path)
            .await?
            .apply_function(|db| {
                let res = apply_expr(
                    serde_json::to_value(patch_db::Value::from(db))
                        .with_kind(ErrorKind::Deserialization)?
                        .into(),
                    &expr,
                )?;

                Ok::<_, Error>((
                    to_value(
                        &serde_json::from_value::<model::Database>(res.clone().into()).with_ctx(
                            |_| {
                                (
                                    crate::ErrorKind::Deserialization,
                                    "result does not match database model",
                                )
                            },
                        )?,
                    )?,
                    (),
                ))
            })
            .await?;
    } else {
        let method = parent_method.into_iter().chain(method).join(".");
        context
            .call_remote::<RpcContext>(&method, imbl_value::json!({ "expr": expr }))
            .await?;
    }

    Ok(())
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct ApplyParams {
    expr: String,
}

pub async fn apply(ctx: RpcContext, ApplyParams { expr }: ApplyParams) -> Result<(), Error> {
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

pub fn put<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand::<C, _>(
        "ui",
        from_fn_async(ui)
            .with_display_serializable()
            .with_call_remote::<CliContext>(),
    )
}
#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct UiParams {
    #[ts(type = "string")]
    pointer: JsonPointer,
    #[ts(type = "any")]
    value: Value,
}

// #[command(display(display_serializable))]
#[instrument(skip_all)]
pub async fn ui(ctx: RpcContext, UiParams { pointer, value, .. }: UiParams) -> Result<(), Error> {
    let ptr = "/public/ui"
        .parse::<JsonPointer>()
        .with_kind(ErrorKind::Database)?
        + &pointer;
    ctx.db.put(&ptr, &value).await?;
    Ok(())
}
