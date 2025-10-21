use std::collections::BTreeMap;
use std::net::SocketAddrV4;
use std::path::PathBuf;

use clap::builder::ValueParserFactory;
use clap::Parser;
use imbl::HashMap;
use imbl_value::InternedString;
use itertools::Itertools;
use models::{FromStrParser, GatewayId};
use patch_db::json_ptr::{JsonPointer, ROOT};
use patch_db::Dump;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{from_fn_async, Context, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tracing::instrument;
use ts_rs::TS;

use crate::auth::Sessions;
use crate::context::CliContext;
use crate::prelude::*;
use crate::sign::AnyVerifyingKey;
use crate::tunnel::auth::SignerInfo;
use crate::tunnel::context::TunnelContext;
use crate::tunnel::wg::WgServer;
use crate::util::serde::{apply_expr, deserialize_from_str, serialize_display, HandlerExtSerde};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GatewayPort(pub GatewayId, pub u16);
impl std::fmt::Display for GatewayPort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}
impl std::str::FromStr for GatewayPort {
    type Err = crate::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.splitn(2, ':');
        let gw: GatewayId = parts
            .next()
            .ok_or_else(|| Error::new(eyre!("missing gateway id"), ErrorKind::ParseNetAddress))?
            .parse()?;
        let port: u16 = parts
            .next()
            .ok_or_else(|| Error::new(eyre!("missing port"), ErrorKind::ParseNetAddress))?
            .parse()?;
        Ok(GatewayPort(gw, port))
    }
}
impl Serialize for GatewayPort {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serialize_display(self, serializer)
    }
}
impl<'de> Deserialize<'de> for GatewayPort {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserialize_from_str(deserializer)
    }
}
impl ValueParserFactory for GatewayPort {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        FromStrParser::new()
    }
}

#[derive(Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct TunnelDatabase {
    pub sessions: Sessions,
    pub password: String,
    pub auth_pubkeys: HashMap<AnyVerifyingKey, SignerInfo>,
    pub wg: WgServer,
    pub port_forwards: PortForwards,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct PortForwards(pub BTreeMap<GatewayPort, SocketAddrV4>);
impl Map for PortForwards {
    type Key = GatewayPort;
    type Value = SocketAddrV4;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Self::key_string(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(InternedString::from_display(key))
    }
}

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
                .call_remote::<TunnelContext>(&method, imbl_value::json!({ "pointer": pointer }))
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

pub async fn dump(ctx: TunnelContext, DumpParams { pointer }: DumpParams) -> Result<Dump, Error> {
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
                        &serde_json::from_value::<TunnelDatabase>(res.clone().into()).with_ctx(
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
            .await
            .result?;
    } else {
        let method = parent_method.into_iter().chain(method).join(".");
        context
            .call_remote::<TunnelContext>(&method, imbl_value::json!({ "expr": expr }))
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

pub async fn apply(ctx: TunnelContext, ApplyParams { expr, .. }: ApplyParams) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            let res = apply_expr(
                serde_json::to_value(patch_db::Value::from(db.clone()))
                    .with_kind(ErrorKind::Deserialization)?
                    .into(),
                &expr,
            )?;

            db.ser(
                &serde_json::from_value::<TunnelDatabase>(res.clone().into()).with_ctx(|_| {
                    (
                        crate::ErrorKind::Deserialization,
                        "result does not match database model",
                    )
                })?,
            )
        })
        .await
        .result
}
