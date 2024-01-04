use clap::Parser;
use color_eyre::eyre::eyre;
pub use models::ActionId;
use models::{PackageId, ProcedureName};
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::config::Config;
use crate::context::RpcContext;
use crate::prelude::*;
use crate::util::serde::{display_serializable, IoFormat};
use crate::Error;

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "version")]
pub enum ActionResult {
    #[serde(rename = "0")]
    V0(ActionResultV0),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ActionResultV0 {
    pub message: String,
    pub value: Option<String>,
    pub copyable: bool,
    pub qr: bool,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum DockerStatus {
    Running,
    Stopped,
}

pub fn display_action_result(params: ActionParams, result: ActionResult) {
    if let Some(format) = params.format {
        return display_serializable(format, result);
    }
    match result {
        ActionResult::V0(ar) => {
            println!(
                "{}: {}",
                ar.message,
                serde_json::to_string(&ar.value).unwrap()
            );
        }
    }
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct ActionParams {
    #[arg(rename = "id")]
    pub pkg_id: PackageId,
    #[arg(rename = "action-id")]
    pub action_id: ActionId,
    // TODO #[arg(stdin, parse(parse_stdin_deserializable))]
    pub input: Option<Config>,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    pub format: Option<IoFormat>,
}

// #[command(about = "Executes an action", display(display_action_result))]
#[instrument(skip_all)]
pub async fn action(
    ctx: RpcContext,
    ActionParams {
        pkg_id,
        action_id,
        input,
        format,
    }: ActionParams,
) -> Result<ActionResult, Error> {
    ctx.managers
        .get(&pkg_id)
        .await
        .or_not_found(lazy_format!("Manager for {}", pkg_id))?
        .execute(
            ProcedureName::Action(action_id.clone()),
            input.map(|c| to_value(&c)).transpose()?.unwrap_or_default(),
            None,
        )
        .await?
        .map_err(|e| Error::new(eyre!("{}", e.1), ErrorKind::Action))
}
