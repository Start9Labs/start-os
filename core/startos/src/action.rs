use std::fmt;

use clap::{Arg, ArgAction, Args, FromArgMatches, Parser};
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
use crate::util::io::BackTrackingIO;
use crate::util::serde::{display_serializable, HandlerExtSerde, IoFormat, WithIoFormat};
use crate::util::Apply;

pub fn action_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "get-input",
            from_fn_async(get_action_input)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand("run", from_fn_async(run_action).no_cli())
        .subcommand(
            "run",
            from_fn_async(cli_run_action)
                .with_display_serializable()
                .with_custom_display_fn(|args, res| Ok(display_action_result(args.params, res))),
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

#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct RunActionParams {
    pub package_id: PackageId,
    pub action_id: ActionId,
    #[ts(optional)]
    pub prev: Option<ActionInput>,
    #[ts(optional, type = "any")]
    pub input: Option<Value>,
}

// #[command(about = "Executes an action", display(display_action_result))]
#[instrument(skip_all)]
pub async fn run_action(
    ctx: RpcContext,
    RunActionParams {
        package_id,
        action_id,
        prev,
        input,
    }: RunActionParams,
) -> Result<Option<ActionResult>, Error> {
    ctx.services
        .get(&package_id)
        .await
        .as_ref()
        .or_not_found(lazy_format!("Manager for {}", package_id))?
        .run_action(Guid::new(), action_id, prev, input.unwrap_or_default())
        .await
}

#[derive(Deserialize, Serialize, Parser)]
pub struct CliRunActionParams {
    package_id: PackageId,
    action_id: ActionId,
    #[command(flatten)]
    input: CliActionInput,
}

#[derive(Deserialize, Serialize)]
enum CliActionInput {
    Interactive,
    Stdin(Value),
}
impl Args for CliActionInput {
    fn augment_args(cmd: clap::Command) -> clap::Command {
        let cmd = cmd.arg(
            Arg::new("interactive")
                .short('i')
                .long("interactive")
                .action(ArgAction::SetTrue),
        );
        if !cmd.get_arguments().any(|a| a.get_id() == "format") {
            cmd.arg(
                clap::Arg::new("format")
                    .long("format")
                    .value_parser(|s: &str| s.parse::<IoFormat>().map_err(|e| eyre!("{e}"))),
            )
        } else {
            cmd
        }
    }
    fn augment_args_for_update(cmd: clap::Command) -> clap::Command {
        Self::augment_args(cmd)
    }
}
impl FromArgMatches for CliActionInput {
    fn from_arg_matches(matches: &clap::ArgMatches) -> Result<Self, clap::Error> {
        if matches.get_flag("interactive") {
            Ok(Self::Interactive)
        } else {
            let format = matches
                .get_one::<IoFormat>("format")
                .copied()
                .unwrap_or_default();
            let mut rdr = BackTrackingIO::new(std::io::stdin());
            match format.from_reader(&mut rdr) {
                Ok(input) => Ok(CliActionInput::Stdin(input)),
                Err(e) => {
                    if rdr.read_buffer().is_empty()
                        || rdr
                            .read_buffer()
                            .apply(std::str::from_utf8)
                            .ok()
                            .map_or(false, |s| s.trim().is_empty())
                    {
                        Ok(CliActionInput::Stdin(Value::Null))
                    } else {
                        Err(clap::Error::raw(clap::error::ErrorKind::Io, e))
                    }
                }
            }
        }
    }
    fn update_from_arg_matches(&mut self, matches: &clap::ArgMatches) -> Result<(), clap::Error> {
        *self = Self::from_arg_matches(matches)?;
        Ok(())
    }
}

#[instrument(skip_all)]
pub async fn cli_run_action(
    ctx: CliContext,
    CliRunActionParams {
        package_id,
        action_id,
        input,
    }: CliRunActionParams,
) -> Result<Option<ActionResult>, Error> {
    todo!()
}
