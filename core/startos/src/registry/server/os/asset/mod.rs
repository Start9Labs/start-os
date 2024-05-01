use rpc_toolkit::ParentHandler;

pub mod add;
pub mod get;
pub mod sign;

pub fn asset_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand("add", add::add_api())
        .subcommand("sign", sign::sign_api())
        .subcommand("get", get::get_api())
}
