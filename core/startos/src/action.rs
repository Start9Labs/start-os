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
use crate::util::serde::{display_serializable, StdinDeserializable, WithIoFormat};
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

pub fn display_action_result(params: WithIoFormat<ActionParams>, result: ActionResult) {
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
    #[arg(id = "id")]
    #[serde(rename = "id")]
    pub package_id: PackageId,
    #[arg(id = "action-id")]
    #[serde(rename = "action-id")]
    pub action_id: ActionId,
    #[command(flatten)]
    pub input: StdinDeserializable<Option<Config>>,
}
// impl C

// #[command(about = "Executes an action", display(display_action_result))]
#[instrument(skip_all)]
pub async fn action(
    ctx: RpcContext,
    ActionParams {
        package_id,
        action_id,
        input: StdinDeserializable(input),
    }: ActionParams,
) -> Result<ActionResult, Error> {
    ctx.managers
        .get(&package_id)
        .await
        .or_not_found(lazy_format!("Manager for {}", package_id))?
        .execute(
            ProcedureName::Action(action_id.clone()),
            input.map(|c| to_value(&c)).transpose()?.unwrap_or_default(),
            None,
        )
        .await?
        .map_err(|e| Error::new(eyre!("{}", e.1), ErrorKind::Action))
}
