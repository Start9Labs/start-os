use std::collections::{BTreeMap, BTreeSet};
use std::net::SocketAddrV4;
use std::path::PathBuf;
use std::time::Duration;

use axum::extract::ws;
use clap::Parser;
use imbl::{HashMap, OrdMap};
use imbl_value::InternedString;
use ipnet::Ipv4Net;
use itertools::Itertools;
use patch_db::Dump;
use patch_db::json_ptr::{JsonPointer, ROOT};
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{Context, HandlerArgs, HandlerExt, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};
use tracing::instrument;
use ts_rs::TS;

use crate::GatewayId;
use crate::auth::Sessions;
use crate::context::CliContext;
use crate::db::model::public::NetworkInterfaceInfo;
use crate::prelude::*;
use crate::rpc_continuations::{Guid, RpcContinuation};
use crate::sign::AnyVerifyingKey;
use crate::tunnel::auth::SignerInfo;
use crate::tunnel::context::TunnelContext;
use crate::tunnel::migrations;
use crate::tunnel::web::WebserverInfo;
use crate::tunnel::wg::{WgServer, WgSubnetConfig};
use crate::util::serde::{HandlerExtSerde, apply_expr};

#[derive(Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct TunnelDatabase {
    #[serde(default)]
    #[ts(skip)]
    pub migrations: BTreeSet<InternedString>,
    pub webserver: WebserverInfo,
    pub sessions: Sessions,
    pub password: Option<String>,
    #[ts(as = "std::collections::HashMap::<AnyVerifyingKey, SignerInfo>")]
    pub auth_pubkeys: HashMap<AnyVerifyingKey, SignerInfo>,
    #[ts(as = "std::collections::BTreeMap::<GatewayId, NetworkInterfaceInfo>")]
    pub gateways: OrdMap<GatewayId, NetworkInterfaceInfo>,
    pub wg: WgServer,
    pub port_forwards: PortForwards,
}

impl TunnelDatabase {
    pub fn init() -> Self {
        let mut db = Self {
            migrations: migrations::MIGRATIONS
                .iter()
                .map(|m| m.name().into())
                .collect(),
            ..Default::default()
        };
        db.wg.subnets.0.insert(
            Ipv4Net::new_assert([10, 59, rand::random(), 1].into(), 24),
            WgSubnetConfig {
                name: "Default Subnet".into(),
                ..Default::default()
            },
        );
        db
    }
}

impl Model<TunnelDatabase> {
    pub fn gc_forwards(&mut self) -> Result<BTreeSet<SocketAddrV4>, Error> {
        let mut keep_sources = BTreeSet::new();
        let mut keep_targets = BTreeSet::new();
        for (_, cfg) in self.as_wg().as_subnets().as_entries()? {
            keep_targets.extend(cfg.as_clients().keys()?);
        }
        self.as_port_forwards_mut().mutate(|pf| {
            Ok(pf.0.retain(|k, v| {
                if keep_targets.contains(v.target.ip()) {
                    keep_sources.insert(*k);
                    true
                } else {
                    false
                }
            }))
        })?;
        Ok(keep_sources)
    }
}

#[test]
fn export_bindings_tunnel_db() {
    use crate::tunnel::api::*;
    use crate::tunnel::auth::{AddKeyParams, RemoveKeyParams, SetPasswordParams};

    TunnelDatabase::export_all_to("bindings/tunnel").unwrap();
    SubnetParams::export_all_to("bindings/tunnel").unwrap();
    AddSubnetParams::export_all_to("bindings/tunnel").unwrap();
    AddDeviceParams::export_all_to("bindings/tunnel").unwrap();
    RemoveDeviceParams::export_all_to("bindings/tunnel").unwrap();
    ListDevicesParams::export_all_to("bindings/tunnel").unwrap();
    ShowConfigParams::export_all_to("bindings/tunnel").unwrap();
    AddPortForwardParams::export_all_to("bindings/tunnel").unwrap();
    RemovePortForwardParams::export_all_to("bindings/tunnel").unwrap();
    UpdatePortForwardLabelParams::export_all_to("bindings/tunnel").unwrap();
    SetPortForwardEnabledParams::export_all_to("bindings/tunnel").unwrap();
    AddKeyParams::export_all_to("bindings/tunnel").unwrap();
    RemoveKeyParams::export_all_to("bindings/tunnel").unwrap();
    SetPasswordParams::export_all_to("bindings/tunnel").unwrap();
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct PortForwardEntry {
    pub target: SocketAddrV4,
    pub label: Option<String>,
    #[serde(default = "default_true")]
    pub enabled: bool,
}

fn default_true() -> bool {
    true
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, TS)]
pub struct PortForwards(pub BTreeMap<SocketAddrV4, PortForwardEntry>);
impl Map for PortForwards {
    type Key = SocketAddrV4;
    type Value = PortForwardEntry;
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
                .with_about("about.filter-query-db-display-tables-records"),
        )
        .subcommand(
            "dump",
            from_fn_async(dump)
                .with_metadata("admin", Value::Bool(true))
                .no_cli(),
        )
        .subcommand(
            "subscribe",
            from_fn_async(subscribe)
                .with_metadata("get_session", Value::Bool(true))
                .no_cli(),
        )
        .subcommand(
            "apply",
            from_fn_async(cli_apply)
                .no_display()
                .with_about("about.update-db-record"),
        )
        .subcommand(
            "apply",
            from_fn_async(apply)
                .with_metadata("admin", Value::Bool(true))
                .no_cli(),
        )
}

#[derive(Deserialize, Serialize, Parser)]
#[group(skip)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct CliDumpParams {
    #[arg(long = "pointer", short = 'p', help = "help.arg.json-pointer")]
    pointer: Option<JsonPointer>,
    #[arg(help = "help.arg.database-path")]
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
#[group(skip)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct DumpParams {
    #[arg(long = "pointer", short = 'p', help = "help.arg.json-pointer")]
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
#[group(skip)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct CliApplyParams {
    #[arg(help = "help.arg.db-apply-expr")]
    expr: String,
    #[arg(help = "help.arg.database-path")]
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
#[group(skip)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct ApplyParams {
    #[arg(help = "help.arg.db-apply-expr")]
    expr: String,
    #[arg(help = "help.arg.database-path")]
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

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct SubscribeParams {
    #[ts(type = "string | null")]
    pointer: Option<JsonPointer>,
    #[ts(skip)]
    #[serde(rename = "__Auth_session")]
    session: Option<InternedString>,
}

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct SubscribeRes {
    #[ts(type = "{ id: number; value: unknown }")]
    pub dump: Dump,
    pub guid: Guid,
}

pub async fn subscribe(
    ctx: TunnelContext,
    SubscribeParams { pointer, session }: SubscribeParams,
) -> Result<SubscribeRes, Error> {
    let (dump, mut sub) = ctx
        .db
        .dump_and_sub(pointer.unwrap_or_else(|| ROOT.to_owned()))
        .await;
    let guid = Guid::new();
    ctx.rpc_continuations
        .add(
            guid.clone(),
            RpcContinuation::ws_authed(
                &ctx,
                session,
                |mut ws| async move {
                    if let Err(e) = async {
                        loop {
                            tokio::select! {
                                rev = sub.recv() => {
                                    if let Some(rev) = rev {
                                        ws.send(ws::Message::Text(
                                            serde_json::to_string(&rev)
                                                .with_kind(ErrorKind::Serialization)?
                                                .into(),
                                        ))
                                        .await
                                        .with_kind(ErrorKind::Network)?;
                                    } else {
                                        return ws.normal_close("complete").await;
                                    }
                                }
                                msg = ws.recv() => {
                                    if msg.transpose().with_kind(ErrorKind::Network)?.is_none() {
                                        return Ok(())
                                    }
                                }
                            }
                        }
                    }
                    .await
                    {
                        if !crate::util::net::is_ws_reset_without_close(&e) {
                            tracing::error!("Error in db websocket: {e}");
                            tracing::debug!("{e:?}");
                        }
                    }
                },
                Duration::from_secs(30),
            ),
        )
        .await;

    Ok(SubscribeRes { dump, guid })
}
