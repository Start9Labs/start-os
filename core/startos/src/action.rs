use std::collections::BTreeMap;
use std::fmt;

use clap::{CommandFactory, FromArgMatches, Parser};
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
                .with_about("Get action input spec")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "run",
            from_fn_async(run_action)
                .with_display_serializable()
                .with_custom_display_fn(|_, res| {
                    if let Some(res) = res {
                        println!("{res}")
                    }
                    Ok(())
                })
                .with_about("Run service action")
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

#[derive(Debug, Serialize, Deserialize, TS)]
#[serde(tag = "version")]
#[ts(export)]
pub enum ActionResult {
    #[serde(rename = "0")]
    V0(ActionResultV0),
    #[serde(rename = "1")]
    V1(ActionResultV1),
}
impl fmt::Display for ActionResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::V0(res) => res.fmt(f),
            Self::V1(res) => res.fmt(f),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, TS)]
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

#[derive(Debug, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct ActionResultV1 {
    pub title: String,
    pub message: Option<String>,
    pub value: Option<ActionResultValue>,
}

#[derive(Debug, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct ActionResultMember {
    pub name: String,
    pub description: Option<String>,
    #[serde(flatten)]
    #[ts(flatten)]
    pub value: ActionResultValue,
}

#[derive(Debug, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[serde(rename_all_fields = "camelCase")]
#[serde(tag = "type")]
pub enum ActionResultValue {
    Single {
        value: String,
        copyable: bool,
        qr: bool,
        masked: bool,
    },
    Group {
        value: Vec<ActionResultMember>,
    },
}
impl ActionResultValue {
    fn fmt_rec(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        match self {
            Self::Single { value, qr, .. } => {
                for _ in 0..indent {
                    write!(f, "  ")?;
                }
                write!(f, "{value}")?;
                if *qr {
                    use qrcode::render::unicode;
                    writeln!(f)?;
                    for _ in 0..indent {
                        write!(f, "  ")?;
                    }
                    write!(
                        f,
                        "{}",
                        QrCode::new(value.as_bytes())
                            .unwrap()
                            .render::<unicode::Dense1x2>()
                            .build()
                    )?;
                }
            }
            Self::Group { value } => {
                for ActionResultMember {
                    name,
                    description,
                    value,
                } in value
                {
                    for _ in 0..indent {
                        write!(f, "  ")?;
                    }
                    write!(f, "{name}")?;
                    if let Some(description) = description {
                        write!(f, ": {description}")?;
                    }
                    writeln!(f, ":")?;
                    value.fmt_rec(f, indent + 1)?;
                }
            }
        }
        Ok(())
    }
}
impl fmt::Display for ActionResultV1 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}:", self.title)?;
        if let Some(message) = &self.message {
            writeln!(f, "{message}")?;
        }
        if let Some(value) = &self.value {
            value.fmt_rec(f, 1)?;
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

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct RunActionParams {
    pub package_id: PackageId,
    pub action_id: ActionId,
    #[ts(optional, type = "any")]
    pub input: Option<Value>,
}

#[derive(Parser)]
struct CliRunActionParams {
    pub package_id: PackageId,
    pub action_id: ActionId,
    #[command(flatten)]
    pub input: StdinDeserializable<Option<Value>>,
}
impl From<CliRunActionParams> for RunActionParams {
    fn from(
        CliRunActionParams {
            package_id,
            action_id,
            input,
        }: CliRunActionParams,
    ) -> Self {
        Self {
            package_id,
            action_id,
            input: input.0,
        }
    }
}
impl CommandFactory for RunActionParams {
    fn command() -> clap::Command {
        CliRunActionParams::command()
    }
    fn command_for_update() -> clap::Command {
        CliRunActionParams::command_for_update()
    }
}
impl FromArgMatches for RunActionParams {
    fn from_arg_matches(matches: &clap::ArgMatches) -> Result<Self, clap::Error> {
        CliRunActionParams::from_arg_matches(matches).map(Self::from)
    }
    fn from_arg_matches_mut(matches: &mut clap::ArgMatches) -> Result<Self, clap::Error> {
        CliRunActionParams::from_arg_matches_mut(matches).map(Self::from)
    }
    fn update_from_arg_matches(&mut self, matches: &clap::ArgMatches) -> Result<(), clap::Error> {
        *self = CliRunActionParams::from_arg_matches(matches).map(Self::from)?;
        Ok(())
    }
    fn update_from_arg_matches_mut(
        &mut self,
        matches: &mut clap::ArgMatches,
    ) -> Result<(), clap::Error> {
        *self = CliRunActionParams::from_arg_matches_mut(matches).map(Self::from)?;
        Ok(())
    }
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
        .run_action(Guid::new(), action_id, input.unwrap_or_default())
        .await
}
