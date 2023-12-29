use clap::ArgMatches;
use color_eyre::eyre::eyre;
use models::{PackageId, ProcedureName};
use rpc_toolkit::command;
use serde_json::Value;
use tracing::instrument;

use crate::context::RpcContext;
use crate::prelude::*;
use crate::{Error, ErrorKind};

pub fn display_properties(response: Value, _: &ArgMatches) {
    println!("{}", response);
}

#[command(display(display_properties))]
pub async fn properties(#[context] ctx: RpcContext, #[arg] id: PackageId) -> Result<Value, Error> {
    Ok(todo!())
}
