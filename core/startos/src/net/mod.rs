use rpc_toolkit::{Context, HandlerExt, ParentHandler};

pub mod acme;
pub mod dns;
pub mod forward;
pub mod host;
pub mod keys;
pub mod mdns;
pub mod net_controller;
pub mod network_interface;
pub mod proxy;
pub mod service_interface;
pub mod ssl;
pub mod static_server;
pub mod tor;
pub mod utils;
pub mod vhost;
pub mod web_server;
pub mod wifi;

pub fn net<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "tor",
            tor::tor::<C>().with_about("Tor commands such as list-services, logs, and reset"),
        )
        .subcommand(
            "acme",
            acme::acme::<C>().with_about("Setup automatic clearnet certificate acquisition"),
        )
        .subcommand(
            "network-interface",
            network_interface::network_interface_api::<C>()
                .with_about("View and edit network interface configurations"),
        )
        .subcommand(
            "vhost",
            vhost::vhost_api::<C>().with_about("Manage ssl virtual host proxy"),
        )
        .subcommand(
            "proxy",
            proxy::proxy_api::<C>().with_about("Manage wireguard proxies"),
        )
}
