pub mod model;
pub mod prelude;

use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use axum::extract::ws;
use clap::Parser;
use imbl_value::InternedString;
use itertools::Itertools;
use patch_db::json_ptr::{JsonPointer, ROOT};
use patch_db::{Dump, Revision};
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{from_fn_async, Context, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tracing::instrument;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::prelude::*;
use crate::rpc_continuations::{Guid, RpcContinuation};
use crate::util::serde::{apply_expr, HandlerExtSerde};

lazy_static::lazy_static! {
    static ref PUBLIC: JsonPointer = "/public".parse().unwrap();
}

pub fn db<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("dump", from_fn_async(cli_dump).with_display_serializable())
        .subcommand("dump", from_fn_async(dump).no_cli())
        .subcommand(
            "subscribe",
            from_fn_async(subscribe)
                .with_metadata("get_session", Value::Bool(true))
                .no_cli(),
        )
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
                    imbl_value::json!({
                        "pointer": if include_private {
                            AsRef::<str>::as_ref(&ROOT)
                        } else {
                            AsRef::<str>::as_ref(&*PUBLIC)
                        }
                    }),
                )
                .await?,
        )?
    };

    Ok(dump)
}

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct DumpParams {
    #[ts(type = "string | null")]
    pointer: Option<JsonPointer>,
}

pub async fn dump(ctx: RpcContext, DumpParams { pointer }: DumpParams) -> Result<Dump, Error> {
    Ok(ctx.db.dump(pointer.as_ref().unwrap_or(&*PUBLIC)).await)
}

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct SubscribeParams {
    #[ts(type = "string | null")]
    pointer: Option<JsonPointer>,
    #[ts(skip)]
    #[serde(rename = "__auth_session")]
    session: InternedString,
}

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct SubscribeRes {
    #[ts(type = "{ id: number; value: unknown }")]
    pub dump: Dump,
    pub guid: Guid,
}

pub async fn subscribe(
    ctx: RpcContext,
    SubscribeParams { pointer, session }: SubscribeParams,
) -> Result<SubscribeRes, Error> {
    let (dump, mut sub) = ctx
        .db
        .dump_and_sub(pointer.unwrap_or_else(|| PUBLIC.clone()))
        .await;
    let guid = Guid::new();
    ctx.rpc_continuations.add(
        guid.clone(),
        RpcContinuation::ws_authed(
            &ctx,
            session,
            |mut ws| async move {
                if let Err(e) = async {
                    while let Some(rev) = sub.recv().await {
                        ws.send(ws::Message::Text(
                            serde_json::to_string(&rev).with_kind(ErrorKind::Serialization)?,
                        ))
                        .await
                        .with_kind(ErrorKind::Network)?;
                    }
                    ws.close().await.with_kind(ErrorKind::Network)?;
                    Ok::<_, Error>(())
                }
                .await
                {
                    tracing::error!("Error in db websocket: {e}");
                    tracing::debug!("{e:?}");
                }
            },
            Duration::from_secs(30),
        ),
    );

    Ok(SubscribeRes { dump, guid })
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
    ParentHandler::new().subcommand(
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
