use rpc_toolkit::{from_fn_async, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::{CliContext, RegistryContext};
use crate::prelude::*;
use crate::registry::server::os::OsIndex;
use crate::util::serde::HandlerExtSerde;

pub mod admin;
pub mod os;

#[derive(Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct RegistryDatabase {
    pub admins: (),
    pub index: FullIndex,
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct FullIndex {
    // pub package: PackageIndex,
    pub os: OsIndex,
}

pub async fn get_full_index(ctx: RegistryContext) -> Result<FullIndex, Error> {
    ctx.db.peek().await.into_index().de()
}

pub fn registry_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "index",
            from_fn_async(get_full_index).with_display_serializable(), // .with_call_remote::<CliContext>(),
        )
        .subcommand("os", os::os_api())
        .subcommand("admin", admin::admin_api())
}
