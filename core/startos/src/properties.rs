use clap::{ArgMatches, Parser};
use models::PackageId;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::context::RpcContext;
use crate::Error;

pub fn display_properties(response: Value) {
    println!("{}", response);
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct PropertiesParam {
    id: PackageId,
}
// #[command(display(display_properties))]
pub async fn properties(
    ctx: RpcContext,
    PropertiesParam { id }: PropertiesParam,
) -> Result<Value, Error> {
    Ok(todo!())
}
