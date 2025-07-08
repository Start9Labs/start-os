use std::fmt;

use clap::{CommandFactory, FromArgMatches, Parser};
pub use models::ActionId;
use models::{PackageId, ReplayId};
use qrcode::QrCode;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tracing::instrument;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::db::model::package::TaskSeverity;
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
        .subcommand(
            "clear-task",
            from_fn_async(clear_task)
                .no_display()
                .with_about("Clear a service task")
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
impl ActionResult {
    pub fn upcast(self) -> Self {
        match self {
            Self::V0(ActionResultV0 {
                message,
                value,
                copyable,
                qr,
            }) => Self::V1(ActionResultV1 {
                title: "Action Complete".into(),
                message: Some(message),
                result: value.map(|value| ActionResultValue::Single {
                    value,
                    copyable,
                    qr,
                    masked: false,
                }),
            }),
            Self::V1(a) => Self::V1(a),
        }
    }
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
    /// Primary text to display as the header of the response modal. e.g. "Success!", "Name Updated", or "Service Information", whatever makes sense
    pub title: String,
    /// (optional) A general message for the user, just under the title
    pub message: Option<String>,
    /// (optional) Structured data to present inside the modal
    pub result: Option<ActionResultValue>,
}

#[derive(Debug, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct ActionResultMember {
    /// A human-readable name or title of the value, such as "Last Active" or "Login Password"
    pub name: String,
    /// (optional) A description of the value, such as an explaining why it exists or how to use it
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
        /// The actual string value to display
        value: String,
        /// Whether or not to include a copy to clipboard icon to copy the value
        copyable: bool,
        /// Whether or not to also display the value as a QR code
        qr: bool,
        /// Whether or not to mask the value using ●●●●●●●, which is useful for password or other sensitive information
        masked: bool,
    },
    Group {
        /// An new group of nested values, experienced by the user as an accordion dropdown
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
        if let Some(result) = &self.result {
            result.fmt_rec(f, 1)?;
        }
        Ok(())
    }
}

pub fn display_action_result<T: Serialize>(
    params: WithIoFormat<T>,
    result: Option<ActionResult>,
) -> Result<(), Error> {
    let Some(result) = result else {
        return Ok(());
    };
    if let Some(format) = params.format {
        return display_serializable(format, result);
    }
    println!("{result}");
    Ok(())
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
        .map(|res| res.map(ActionResult::upcast))
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct ClearTaskParams {
    pub package_id: PackageId,
    pub replay_id: ReplayId,
    #[arg(long)]
    #[serde(default)]
    pub force: bool,
}

#[instrument(skip_all)]
pub async fn clear_task(
    ctx: RpcContext,
    ClearTaskParams {
        package_id,
        replay_id,
        force,
    }: ClearTaskParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            if let Some(task) = db
                .as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_tasks_mut()
                .remove(&replay_id)?
            {
                if !force && task.as_task().as_severity().de()? == TaskSeverity::Critical {
                    return Err(Error::new(
                        eyre!("Cannot clear critical task"),
                        ErrorKind::InvalidRequest,
                    ));
                }
            }
            Ok(())
        })
        .await
        .result
}
