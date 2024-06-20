use clap::Parser;
use imbl_value::{json, Value};
use models::PackageId;
use serde::{Deserialize, Serialize};

use crate::context::RpcContext;
use crate::prelude::*;
use crate::Error;

pub fn display_properties(response: Value) {
    println!("{}", response);
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct PropertiesParam {
    id: PackageId,
}
// #[command(display(display_properties))]
pub async fn properties(
    ctx: RpcContext,
    PropertiesParam { id }: PropertiesParam,
) -> Result<Value, Error> {
    match &*ctx.services.get(&id).await {
        Some(service) => Ok(json!({
            "version": 2,
            "data": service.properties().await?
        })),
        None => Err(Error::new(
            eyre!("Could not find a service with id {id}"),
            ErrorKind::NotFound,
        )),
    }
}
