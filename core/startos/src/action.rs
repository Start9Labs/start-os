use std::fmt;

use clap::Parser;
pub use models::ActionId;
use models::PackageId;
use qrcode::QrCode;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tracing::instrument;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::prelude::*;
use crate::rpc_continuations::Guid;
use crate::util::serde::{
    display_serializable, HandlerExtSerde, StdinDeserializable, WithIoFormat,
};

pub fn action_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "get-input",
            from_fn_async(get_action_input)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "run",
            from_fn_async(run_action)
                .with_custom_display_fn(|_, res| {
                    if let Some(res) = res {
                        println!("{res}")
                    }
                    Ok(())
                })
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct ActionInput {
    #[ts(type = "Record<string, unknown>")]
    pub spec: Value,
    #[ts(type = "Record<string, unknown> | null")]
    pub value: Option<Value>,
}

#[derive(Deserialize, Serialize, TS, Parser)]
#[serde(rename_all = "camelCase")]
pub struct GetActionInputParams {
    pub package_id: PackageId,
    pub action_id: ActionId,
}

#[instrument(skip_all)]
pub async fn get_action_input(
    ctx: RpcContext,
    GetActionInputParams {
        package_id,
        action_id,
    }: GetActionInputParams,
) -> Result<Option<ActionInput>, Error> {
    ctx.services
        .get(&package_id)
        .await
        .as_ref()
        .or_not_found(lazy_format!("Manager for {}", package_id))?
        .get_action_input(Guid::new(), action_id)
        .await
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "version")]
pub enum ActionResult {
    #[serde(rename = "0")]
    V0(ActionResultV0),
}
impl fmt::Display for ActionResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::V0(res) => res.fmt(f),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ActionResultV0 {
    pub message: String,
    pub value: Option<String>,
    pub copyable: bool,
    pub qr: bool,
}
impl fmt::Display for ActionResultV0 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)?;
        if let Some(value) = &self.value {
            write!(f, ":\n{value}")?;
            if self.qr {
                use qrcode::render::unicode;
                write!(
                    f,
                    "\n{}",
                    QrCode::new(value.as_bytes())
                        .unwrap()
                        .render::<unicode::Dense1x2>()
                        .build()
                )?;
            }
        }
        Ok(())
    }
}

pub fn display_action_result<T: Serialize>(params: WithIoFormat<T>, result: Option<ActionResult>) {
    let Some(result) = result else {
        return;
    };
    if let Some(format) = params.format {
        return display_serializable(format, result);
    }
    println!("{result}")
}

#[derive(Deserialize, Serialize, TS, Parser)]
#[serde(rename_all = "camelCase")]
pub struct RunActionParams {
    pub package_id: PackageId,
    pub action_id: ActionId,
    #[ts(optional, type = "any")]
    #[command(flatten)]
    pub input: Option<StdinDeserializable<Value>>,
}

// #[command(about = "Executes an action", display(display_action_result))]
#[instrument(skip_all)]
pub async fn run_action(
    ctx: RpcContext,
    RunActionParams {
        package_id,
        action_id,
        input,
    }: RunActionParams,
) -> Result<Option<ActionResult>, Error> {
    ctx.services
        .get(&package_id)
        .await
        .as_ref()
        .or_not_found(lazy_format!("Manager for {}", package_id))?
        .run_action(Guid::new(), action_id, input.unwrap_or_default().0)
        .await
}
