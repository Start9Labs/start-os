use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};

pub mod add;
pub mod get;
pub mod sign;

pub fn asset_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("add", add::add_api::<C>())
        .subcommand(
            "add",
            from_fn_async(add::cli_add_asset)
                .no_display()
                .with_about("Add asset to registry"),
        )
        .subcommand("remove", add::remove_api::<C>())
        .subcommand("sign", sign::sign_api::<C>())
        .subcommand(
            "sign",
            from_fn_async(sign::cli_sign_asset)
                .no_display()
                .with_about("Sign file and add to registry index"),
        )
        // TODO: remove signature api
        .subcommand(
            "get",
            get::get_api::<C>().with_about("Commands to download image, iso, or squashfs files"),
        )
}
