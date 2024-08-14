use rpc_toolkit::{from_fn, from_fn_async, Context, HandlerExt, ParentHandler};

use crate::echo;
use crate::prelude::*;
use crate::service::cli::ContainerCliContext;
use crate::service::effects::context::EffectContext;

mod action;
pub mod callbacks;
mod config;
pub mod context;
mod control;
mod dependency;
mod health;
mod image;
mod net;
mod prelude;
mod store;
mod system;

pub fn handler<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("gitInfo", from_fn(|_: C| crate::version::git_info()))
        .subcommand(
            "echo",
            from_fn(echo::<EffectContext>).with_call_remote::<ContainerCliContext>(),
        )
        // action
        .subcommand(
            "executeAction",
            from_fn_async(action::execute_action).no_cli(),
        )
        .subcommand(
            "exportAction",
            from_fn_async(action::export_action).no_cli(),
        )
        .subcommand(
            "clearActions",
            from_fn_async(action::clear_actions).no_cli(),
        )
        // callbacks
        .subcommand(
            "clearCallbacks",
            from_fn(callbacks::clear_callbacks).no_cli(),
        )
        // config
        .subcommand(
            "getConfigured",
            from_fn_async(config::get_configured).no_cli(),
        )
        .subcommand(
            "setConfigured",
            from_fn_async(config::set_configured)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        // control
        .subcommand(
            "restart",
            from_fn_async(control::restart)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "shutdown",
            from_fn_async(control::shutdown)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "setMainStatus",
            from_fn_async(control::set_main_status)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        // dependency
        .subcommand(
            "setDependencies",
            from_fn_async(dependency::set_dependencies)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "getDependencies",
            from_fn_async(dependency::get_dependencies)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "checkDependencies",
            from_fn_async(dependency::check_dependencies)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand("mount", from_fn_async(dependency::mount).no_cli())
        .subcommand(
            "getInstalledPackages",
            from_fn_async(dependency::get_installed_packages).no_cli(),
        )
        .subcommand(
            "exposeForDependents",
            from_fn_async(dependency::expose_for_dependents).no_cli(),
        )
        // health
        .subcommand("setHealth", from_fn_async(health::set_health).no_cli())
        // image
        .subcommand(
            "chroot",
            from_fn(image::chroot::<ContainerCliContext>).no_display(),
        )
        .subcommand(
            "createOverlayedImage",
            from_fn_async(image::create_overlayed_image)
                .with_custom_display_fn(|_, (path, _)| Ok(println!("{}", path.display())))
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "destroyOverlayedImage",
            from_fn_async(image::destroy_overlayed_image).no_cli(),
        )
        // net
        .subcommand("bind", from_fn_async(net::bind::bind).no_cli())
        .subcommand(
            "getServicePortForward",
            from_fn_async(net::bind::get_service_port_forward).no_cli(),
        )
        .subcommand(
            "clearBindings",
            from_fn_async(net::bind::clear_bindings).no_cli(),
        )
        .subcommand(
            "getHostInfo",
            from_fn_async(net::host::get_host_info).no_cli(),
        )
        .subcommand(
            "getPrimaryUrl",
            from_fn_async(net::host::get_primary_url).no_cli(),
        )
        .subcommand(
            "getContainerIp",
            from_fn_async(net::info::get_container_ip).no_cli(),
        )
        .subcommand(
            "exportServiceInterface",
            from_fn_async(net::interface::export_service_interface).no_cli(),
        )
        .subcommand(
            "getServiceInterface",
            from_fn_async(net::interface::get_service_interface).no_cli(),
        )
        .subcommand(
            "listServiceInterfaces",
            from_fn_async(net::interface::list_service_interfaces).no_cli(),
        )
        .subcommand(
            "clearServiceInterfaces",
            from_fn_async(net::interface::clear_service_interfaces).no_cli(),
        )
        .subcommand(
            "getSslCertificate",
            from_fn_async(net::ssl::get_ssl_certificate).no_cli(),
        )
        .subcommand("getSslKey", from_fn_async(net::ssl::get_ssl_key).no_cli())
        // store
        .subcommand("getStore", from_fn_async(store::get_store).no_cli())
        .subcommand("setStore", from_fn_async(store::set_store).no_cli())
        .subcommand(
            "setDataVersion",
            from_fn_async(store::set_data_version)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "getDataVersion",
            from_fn_async(store::get_data_version)
                .with_custom_display_fn(|_, v| {
                    if let Some(v) = v {
                        println!("{v}")
                    } else {
                        println!("N/A")
                    }
                    Ok(())
                })
                .with_call_remote::<ContainerCliContext>(),
        )
        // system
        .subcommand(
            "getSystemSmtp",
            from_fn_async(system::get_system_smtp).no_cli(),
        )

    // TODO Callbacks
}
