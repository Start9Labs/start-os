use clap::ArgMatches;
use color_eyre::eyre::eyre;
pub use models::ActionId;
use models::{PackageId, ProcedureName};
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::config::Config;
use crate::context::RpcContext;
use crate::prelude::*;
use crate::util::serde::{display_serializable, parse_stdin_deserializable, IoFormat};
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

fn display_action_result(action_result: ActionResult, matches: &ArgMatches) {
    if matches.is_present("format") {
        return display_serializable(action_result, matches);
    }
    match action_result {
        ActionResult::V0(ar) => {
            println!(
                "{}: {}",
                ar.message,
                serde_json::to_string(&ar.value).unwrap()
            );
        }
    }
}

#[command(about = "Executes an action", display(display_action_result))]
#[instrument(skip_all)]
pub async fn action(
    #[context] ctx: RpcContext,
    #[arg(rename = "id")] pkg_id: PackageId,
    #[arg(rename = "action-id")] action_id: ActionId,
    #[arg(stdin, parse(parse_stdin_deserializable))] input: Option<Config>,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<ActionResult, Error> {
    let version = ctx
        .db
        .peek()
        .await
        .into_package_data()
        .into_idx(&pkg_id)
        .and_then(|pde| pde.into_installed())
        .map(|i| i.into_manifest().into_version().de())
        .transpose()?
        .or_not_found(&pkg_id)?;
    ctx.managers
        .get(&(pkg_id.clone(), version.clone()))
        .await
        .or_not_found(lazy_format!("Manager for {}@{}", pkg_id, version))?
        .execute(
            ProcedureName::Action(action_id.clone()),
            input.map(|c| to_value(&c)).transpose()?.unwrap_or_default(),
            None,
        )
        .await?
        .map_err(|e| Error::new(eyre!("{}", e.1), ErrorKind::Action))
}
