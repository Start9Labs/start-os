use std::collections::BTreeMap;

use rpc_toolkit::command;
use tracing::instrument;

use crate::config::not_found;
use crate::context::RpcContext;
use crate::db::model::CurrentDependents;
use crate::prelude::*;
use crate::s9pk::manifest::PackageId;
use crate::util::serde::display_serializable;
use crate::util::Version;
use crate::Error;

#[command(subcommands(dry))]
pub async fn update() -> Result<(), Error> {
    Ok(())
}
