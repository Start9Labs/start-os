use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};

use crate::context::CliContext;
use crate::prelude::*;
use crate::util::serde::HandlerExtSerde;

pub mod add;
pub mod get;
pub mod index;

pub fn package_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "index",
            from_fn_async(index::get_package_index)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "add",
            from_fn_async(add::add_package)
                .with_metadata("get_signer", Value::Bool(true))
                .no_cli(),
        )
        .subcommand("add", from_fn_async(add::cli_add_package).no_display())
        .subcommand(
            "get",
            from_fn_async(get::get_package)
                .with_metadata("get_device_info", Value::Bool(true))
                .with_display_serializable()
                .with_custom_display_fn(|handle, result| {
                    get::display_package_info(handle.params, result)
                }),
        )
}
