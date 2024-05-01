use rpc_toolkit::{from_fn_async, HandlerExt, ParentHandler};

use crate::context::CliContext;
use crate::util::serde::HandlerExtSerde;

pub const SIG_CONTEXT: &str = "startos";

pub mod asset;
pub mod index;
pub mod version;

pub fn os_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "index",
            from_fn_async(index::get_os_index)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand("asset", asset::asset_api())
        .subcommand("version", version::version_api())
}
