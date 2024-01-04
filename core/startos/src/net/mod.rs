use std::sync::Arc;

use futures::future::BoxFuture;
use hyper::{Body, Error as HyperError, Request, Response};
use rpc_toolkit::{from_fn_async, ParentHandler};

pub mod dhcp;
pub mod dns;
pub mod interface;
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
        .subcommand("ssl", ssl::ssl())
        .subcommand(
            "rotate-key",
            from_fn_async(keys::rotate_key).with_custom_display_fn(|handle, result| {
                Ok(keys::display_requires_reboot(handle.params, result))
            }),
        )
}

pub type HttpHandler = Arc<
    dyn Fn(Request<Body>) -> BoxFuture<'static, Result<Response<Body>, HyperError>> + Send + Sync,
>;
