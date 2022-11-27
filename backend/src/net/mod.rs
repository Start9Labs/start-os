use std::collections::BTreeMap;
use std::sync::Arc;

use futures::future::BoxFuture;
use hyper::{Body, Error as HyperError, Request, Response};
use indexmap::IndexSet;
use rpc_toolkit::command;

use self::interface::InterfaceId;
use crate::net::interface::LanPortConfig;
use crate::util::serde::Port;
use crate::Error;

pub mod cert_resolver;
pub mod dns;
pub mod embassy_service_http_server;
pub mod interface;
#[cfg(feature = "avahi")]
pub mod mdns;
pub mod net_controller;
pub mod net_utils;
pub mod proxy_controller;
pub mod ssl;
pub mod static_server;
pub mod tor;
pub mod vhost_controller;
pub mod wifi;

const PACKAGE_CERT_PATH: &str = "/var/lib/embassy/ssl";

#[command(subcommands(tor::tor))]
pub fn net() -> Result<(), Error> {
    Ok(())
}

#[derive(Default)]
struct PackageNetInfo {
    interfaces: BTreeMap<InterfaceId, InterfaceMetadata>,
}
pub struct InterfaceMetadata {
    pub fqdn: String,
    pub lan_config: BTreeMap<Port, LanPortConfig>,
    pub protocols: IndexSet<String>,
}

/// Indicates that the net controller has created the
/// SSL keys
#[derive(Clone, Copy)]
pub struct GeneratedCertificateMountPoint(());

pub type HttpHandler = Arc<
    dyn Fn(Request<Body>) -> BoxFuture<'static, Result<Response<Body>, HyperError>> + Send + Sync,
>;
