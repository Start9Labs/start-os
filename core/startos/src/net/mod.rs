use rpc_toolkit::{Context, HandlerExt, ParentHandler};

pub mod acme;
pub mod dns;
pub mod forward;
pub mod gateway;
pub mod host;
pub mod keys;
pub mod mdns;
pub mod net_controller;
pub mod service_interface;
pub mod socks;
pub mod ssl;
pub mod static_server;
pub mod tor;
pub mod tunnel;
pub mod utils;
pub mod vhost;
pub mod web_server;
pub mod wifi;

pub fn net_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "tor",
            tor::tor_api::<C>().with_about("Tor commands such as list-services, logs, and reset"),
        )
        .subcommand(
            "acme",
            acme::acme_api::<C>().with_about("Setup automatic clearnet certificate acquisition"),
        )
        .subcommand(
            "dns",
            dns::dns_api::<C>().with_about("Manage and query DNS"),
        )
        .subcommand(
            "gateway",
            gateway::gateway_api::<C>().with_about("View and edit gateway configurations"),
        )
        .subcommand(
            "tunnel",
            tunnel::tunnel_api::<C>().with_about("Manage tunnels"),
        )
        .subcommand(
            "vhost",
            vhost::vhost_api::<C>().with_about("Manage ssl virtual host proxy"),
        )
}
