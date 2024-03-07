use rpc_toolkit::ParentHandler;

pub mod dhcp;
pub mod dns;
pub mod forward;
pub mod host;
pub mod keys;
pub mod mdns;
pub mod net_controller;
pub mod ssl;
pub mod static_server;
pub mod tor;
pub mod utils;
pub mod vhost;
pub mod web_server;
pub mod wifi;

pub const PACKAGE_CERT_PATH: &str = "/var/lib/embassy/ssl";

pub fn net() -> ParentHandler {
    ParentHandler::new()
        .subcommand("tor", tor::tor())
        .subcommand("dhcp", dhcp::dhcp())
}
