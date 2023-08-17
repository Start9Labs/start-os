use std::sync::Arc;

use futures::future::BoxFuture;
use hyper::{Body, Error as HyperError, Request, Response};
use rpc_toolkit::command;

use crate::Error;

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

#[command(subcommands(tor::tor, dhcp::dhcp))]
pub fn net() -> Result<(), Error> {
    Ok(())
}

pub type HttpHandler = Arc<
    dyn Fn(Request<Body>) -> BoxFuture<'static, Result<Response<Body>, HyperError>> + Send + Sync,
>;
