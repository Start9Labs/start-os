use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};

use crate::context::CliContext;
use crate::util::serde::HandlerExtSerde;

pub const SIG_CONTEXT: &str = "startos";

pub mod asset;
pub mod index;
pub mod version;

pub fn os_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand::<C, _>(
            "index",
            from_fn_async(index::get_os_index)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand("asset", asset::asset_api::<C>())
        .subcommand("version", version::version_api::<C>())
}
