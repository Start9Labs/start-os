use std::collections::{BTreeMap, BTreeSet};

use clap::ArgMatches;
use color_eyre::eyre::eyre;
use indexmap::IndexSet;
pub use models::ActionId;
use models::ImageId;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::config::Config;
use crate::context::RpcContext;
use crate::prelude::*;
use crate::procedure::docker::DockerContainers;
use crate::procedure::{PackageProcedure, ProcedureName};
use crate::s9pk::manifest::PackageId;
use crate::util::serde::{display_serializable, parse_stdin_deserializable, IoFormat};
use crate::util::Version;
use crate::volume::Volumes;

#[derive(Clone, Debug, Default, Deserialize, Serialize, PartialEq, Eq)]
pub struct Actions(pub BTreeMap<ActionId, Action>);

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

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub struct Action {
    pub name: String,
    pub description: String,
    #[serde(default)]
    pub warning: Option<String>,
    pub implementation: PackageProcedure,
    pub allowed_statuses: IndexSet<DockerStatus>,
    #[serde(default)]
    pub input_spec: Value,
}
impl Action {
    #[instrument(skip_all)]
    pub fn validate(
        &self,
        container: &Option<DockerContainers>,
        eos_version: &Version,
        volumes: &Volumes,
        image_ids: &BTreeSet<ImageId>,
    ) -> Result<(), Error> {
        self.implementation
            .validate(container, eos_version, volumes, image_ids, true)
            .with_ctx(|_| (ErrorKind::ValidateS9pk, format!("Action {}", self.name)))
    }

    #[instrument(skip_all)]
    pub async fn execute(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        action_id: &ActionId,
        volumes: &Volumes,
        input: Option<Config>,
    ) -> Result<ActionResult, Error> {
        self.implementation
            .execute(
                ctx,
                pkg_id,
                pkg_version,
                ProcedureName::Action(action_id.clone()),
                volumes,
                input,
                None,
            )
            .await?
            .map_err(|e| Error::new(eyre!("{}", e.1), ErrorKind::Action))
    }
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
    let mut db = ctx.db.handle();
    let manifest = todo!(); /* crate::db::DatabaseModel::new()
                            .package_data()
                            .idx_model(&pkg_id)
                            .and_then(|p| p.installed())
                            .expect()
                            .await
                            .with_kind(ErrorKind::NotFound)?
                            .manifest()
                            .get()
                            .await?
                            .to_owned();*/

    if let Some(action) = manifest.actions.0.get(&action_id) {
        action
            .execute(
                &ctx,
                &manifest.id,
                &manifest.version,
                &action_id,
                &manifest.volumes,
                input,
            )
            .await
    } else {
        Err(Error::new(
            eyre!("Action not found in manifest"),
            ErrorKind::NotFound,
        ))
    }
}
