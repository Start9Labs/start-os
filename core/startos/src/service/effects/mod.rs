use rpc_toolkit::{from_fn, from_fn_async, from_fn_blocking, Context, HandlerExt, ParentHandler};

use crate::echo;
use crate::prelude::*;
use crate::service::cli::ContainerCliContext;
use crate::service::effects::context::EffectContext;

mod action;
pub mod callbacks;
pub mod context;
mod control;
mod dependency;
mod health;
mod net;
mod prelude;
mod store;
mod subcontainer;
mod system;

pub fn handler<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("git-info", from_fn(|_: C| crate::version::git_info()))
        .subcommand(
            "echo",
            from_fn(echo::<EffectContext>).with_call_remote::<ContainerCliContext>(),
        )
        // action
        .subcommand("action", action::action_api::<C>())
        // callbacks
        .subcommand(
            "clear-callbacks",
            from_fn(callbacks::clear_callbacks).no_cli(),
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
            "set-main-status",
            from_fn_async(control::set_main_status)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        // dependency
        .subcommand(
            "set-dependencies",
            from_fn_async(dependency::set_dependencies)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "get-dependencies",
            from_fn_async(dependency::get_dependencies)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "check-dependencies",
            from_fn_async(dependency::check_dependencies)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand("mount", from_fn_async(dependency::mount).no_cli())
        .subcommand(
            "get-installed-packages",
            from_fn_async(dependency::get_installed_packages).no_cli(),
        )
        .subcommand(
            "expose-for-dependents",
            from_fn_async(dependency::expose_for_dependents).no_cli(),
        )
        // health
        .subcommand("set-health", from_fn_async(health::set_health).no_cli())
        // subcontainer
        .subcommand(
            "subcontainer",
            ParentHandler::<C>::new()
                .subcommand(
                    "launch",
                    from_fn_blocking(subcontainer::launch).no_display(),
                )
                .subcommand(
                    "launch-init",
                    from_fn_blocking(subcontainer::launch_init).no_display(),
                )
                .subcommand("exec", from_fn_blocking(subcontainer::exec).no_display())
                .subcommand(
                    "exec-command",
                    from_fn_blocking(subcontainer::exec_command).no_display(),
                )
                .subcommand(
                    "create-fs",
                    from_fn_async(subcontainer::create_subcontainer_fs)
                        .with_custom_display_fn(|_, (path, _)| Ok(println!("{}", path.display())))
                        .with_call_remote::<ContainerCliContext>(),
                )
                .subcommand(
                    "destroy-fs",
                    from_fn_async(subcontainer::destroy_subcontainer_fs)
                        .no_display()
                        .with_call_remote::<ContainerCliContext>(),
                ),
        )
        // net
        .subcommand("bind", from_fn_async(net::bind::bind).no_cli())
        .subcommand(
            "get-service-port-forward",
            from_fn_async(net::bind::get_service_port_forward).no_cli(),
        )
        .subcommand(
            "clear-bindings",
            from_fn_async(net::bind::clear_bindings).no_cli(),
        )
        .subcommand(
            "get-host-info",
            from_fn_async(net::host::get_host_info).no_cli(),
        )
        .subcommand(
            "get-primary-url",
            from_fn_async(net::host::get_primary_url).no_cli(),
        )
        .subcommand(
            "get-container-ip",
            from_fn_async(net::info::get_container_ip).no_cli(),
        )
        .subcommand(
            "export-service-interface",
            from_fn_async(net::interface::export_service_interface).no_cli(),
        )
        .subcommand(
            "get-service-interface",
            from_fn_async(net::interface::get_service_interface).no_cli(),
        )
        .subcommand(
            "list-service-interfaces",
            from_fn_async(net::interface::list_service_interfaces).no_cli(),
        )
        .subcommand(
            "clear-service-interfaces",
            from_fn_async(net::interface::clear_service_interfaces).no_cli(),
        )
        .subcommand(
            "get-ssl-certificate",
            from_fn_async(net::ssl::get_ssl_certificate).no_cli(),
        )
        .subcommand("get-ssl-key", from_fn_async(net::ssl::get_ssl_key).no_cli())
        // store
        .subcommand(
            "store",
            ParentHandler::<C>::new()
                .subcommand("get", from_fn_async(store::get_store).no_cli())
                .subcommand("set", from_fn_async(store::set_store).no_cli()),
        )
        .subcommand(
            "set-data-version",
            from_fn_async(store::set_data_version)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "get-data-version",
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
            "get-system-smtp",
            from_fn_async(system::get_system_smtp).no_cli(),
        )

    // TODO Callbacks
}
