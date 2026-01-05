use rpc_toolkit::{Context, HandlerExt, ParentHandler, from_fn_async, from_fn_async_local};

use crate::context::CliContext;
use crate::prelude::*;
use crate::util::serde::HandlerExtSerde;

pub mod add;
pub mod category;
pub mod get;
pub mod index;
pub mod signer;

pub fn package_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "index",
            from_fn_async(index::get_package_index)
                .with_metadata("authenticated", Value::Bool(false))
                .with_display_serializable()
                .with_about("List packages and categories")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "add",
            from_fn_async(add::add_package)
                .with_metadata("get_signer", Value::Bool(true))
                .no_cli(),
        )
        .subcommand(
            "add",
            from_fn_async(add::cli_add_package)
                .no_display()
                .with_about("Add package to registry index"),
        )
        .subcommand(
            "add-mirror",
            from_fn_async(add::add_mirror)
                .with_metadata("get_signer", Value::Bool(true))
                .no_cli(),
        )
        .subcommand(
            "add-mirror",
            from_fn_async(add::cli_add_mirror)
                .no_display()
                .with_about("Add a mirror for an s9pk"),
        )
        .subcommand(
            "remove",
            from_fn_async(add::remove_package)
                .with_metadata("get_signer", Value::Bool(true))
                .with_custom_display_fn(|args, changed| {
                    if !changed {
                        tracing::warn!(
                            "{}@{}{} does not exist, so not removed",
                            args.params.id,
                            args.params.version,
                            args.params
                                .sighash
                                .map_or(String::new(), |h| format!("#{h}"))
                        );
                    }
                    Ok(())
                })
                .with_about("Remove package from registry index")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove-mirror",
            from_fn_async(add::remove_mirror)
                .with_metadata("get_signer", Value::Bool(true))
                .no_display()
                .with_about("Remove a mirror from a package")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "signer",
            signer::signer_api::<C>().with_about("Add, remove, and list package signers"),
        )
        .subcommand(
            "get",
            from_fn_async(get::get_package)
                .with_metadata("authenticated", Value::Bool(false))
                .with_metadata("get_device_info", Value::Bool(true))
                .with_display_serializable()
                .with_custom_display_fn(|handle, result| {
                    get::display_package_info(handle.params, result)
                })
                .with_about("List installation candidate package(s)")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "download",
            from_fn_async_local(get::cli_download)
                .no_display()
                .with_about("Download an s9pk"),
        )
        .subcommand(
            "category",
            category::category_api::<C>()
                .with_about("Update the categories for packages on the registry"),
        )
}
