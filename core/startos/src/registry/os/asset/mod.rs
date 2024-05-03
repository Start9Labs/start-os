use rpc_toolkit::{from_fn_async, HandlerExt, ParentHandler};

pub mod add;
pub mod get;
pub mod sign;

pub fn asset_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand("add", add::add_api())
        .subcommand("add", from_fn_async(add::cli_add_asset).no_display())
        .subcommand("sign", sign::sign_api())
        .subcommand("sign", from_fn_async(sign::cli_sign_asset).no_display())
        .subcommand("get", get::get_api())
}
