use std::path::PathBuf;

use clap::Parser;
use itertools::Itertools;
use patch_db::json_ptr::{JsonPointer, ROOT};
use patch_db::Dump;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{from_fn_async, Context, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tracing::instrument;
use ts_rs::TS;

use crate::context::CliContext;
use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::registry::RegistryDatabase;
use crate::util::serde::{apply_expr, HandlerExtSerde};

pub fn db_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "dump",
            from_fn_async(cli_dump)
                .with_display_serializable()
                .with_about("Filter/query db to display tables and records"),
        )
        .subcommand(
            "dump",
            from_fn_async(dump)
                .with_metadata("admin", Value::Bool(true))
                .no_cli(),
        )
        .subcommand(
            "apply",
            from_fn_async(cli_apply)
                .no_display()
                .with_about("Update a db record"),
        )
        .subcommand(
            "apply",
            from_fn_async(apply)
                .with_metadata("admin", Value::Bool(true))
                .no_cli(),
        )
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct CliDumpParams {
    #[arg(long = "pointer", short = 'p')]
    pointer: Option<JsonPointer>,
    path: Option<PathBuf>,
}

#[instrument(skip_all)]
async fn cli_dump(
    HandlerArgs {
        context,
        parent_method,
        method,
        params: CliDumpParams { pointer, path },
        ..
    }: HandlerArgs<CliContext, CliDumpParams>,
) -> Result<Dump, RpcError> {
    let dump = if let Some(path) = path {
        PatchDb::open(path).await?.dump(&ROOT).await
    } else {
        let method = parent_method.into_iter().chain(method).join(".");
        from_value::<Dump>(
            context
                .call_remote::<RegistryContext>(&method, imbl_value::json!({ "pointer": pointer }))
                .await?,
        )?
    };

    Ok(dump)
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct DumpParams {
    #[arg(long = "pointer", short = 'p')]
    #[ts(type = "string | null")]
    pointer: Option<JsonPointer>,
}

pub async fn dump(ctx: RegistryContext, DumpParams { pointer }: DumpParams) -> Result<Dump, Error> {
    Ok(ctx
        .db
        .dump(&pointer.as_ref().map_or(ROOT, |p| p.borrowed()))
        .await)
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
                        &serde_json::from_value::<RegistryDatabase>(res.clone().into()).with_ctx(
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
            .call_remote::<RegistryContext>(&method, imbl_value::json!({ "expr": expr }))
            .await?;
    }

    Ok(())
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct ApplyParams {
    expr: String,
    path: Option<PathBuf>,
}

pub async fn apply(
    ctx: RegistryContext,
    ApplyParams { expr, .. }: ApplyParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            let res = apply_expr(
                serde_json::to_value(patch_db::Value::from(db.clone()))
                    .with_kind(ErrorKind::Deserialization)?
                    .into(),
                &expr,
            )?;

            db.ser(
                &serde_json::from_value::<RegistryDatabase>(res.clone().into()).with_ctx(|_| {
                    (
                        crate::ErrorKind::Deserialization,
                        "result does not match database model",
                    )
                })?,
            )
        })
        .await
}
