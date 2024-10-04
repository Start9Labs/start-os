use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};

use crate::context::CliContext;
use crate::util::serde::HandlerExtSerde;

pub const SIG_CONTEXT: &str = "startos";

pub mod asset;
pub mod index;
pub mod version;

pub fn os_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "index",
            from_fn_async(index::get_os_index)
                .with_display_serializable()
                .with_call_remote::<CliContext>()
                .with_about("List index of OS versions"),
        )
        .subcommand(
            "asset",
            asset::asset_api::<C>().with_about("Commands to add, sign, or get registry assets"),
        )
        .subcommand(
            "version",
            version::version_api::<C>()
                .with_about("Commands to add, remove, or list versions or version signers"),
        )
}
