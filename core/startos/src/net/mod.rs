use rpc_toolkit::{Context, HandlerExt, ParentHandler};

pub mod acme;
pub mod dns;
pub mod forward;
pub mod host;
pub mod keys;
pub mod mdns;
pub mod net_controller;
pub mod network_interface;
pub mod service_interface;
pub mod ssl;
pub mod static_server;
pub mod tor;
pub mod utils;
pub mod vhost;
pub mod web_server;
pub mod wifi;

pub const PACKAGE_CERT_PATH: &str = "/var/lib/embassy/ssl";

pub fn net<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "tor",
            tor::tor::<C>().with_about("Tor commands such as list-services, logs, and reset"),
        )
        // .subcommand(
        //     "dhcp",
        //     network_interface::dhcp::<C>().with_about("Command to update IP assigned from dhcp"),
        // )
        .subcommand(
            "acme",
            acme::acme::<C>().with_about("Setup automatic clearnet certificate acquisition"),
        )
}
