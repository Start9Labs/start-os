use std::collections::BTreeMap;
use std::sync::Arc;

use futures::future::BoxFuture;
use hyper::{Body, Client, Error as HyperError, Request, Response};
use indexmap::IndexSet;
use rpc_toolkit::command;

use self::interface::InterfaceId;

use crate::net::interface::LanPortConfig;

use crate::util::serde::Port;
use openssl::pkey::{PKey, Private};
use torut::onion::{OnionAddressV3, TorSecretKeyV3};
use tracing::instrument;

use self::interface::Interface;
#[cfg(feature = "avahi")]
use self::ssl::SslManager;

use crate::Error;

pub mod dns;
pub mod interface;
#[cfg(feature = "avahi")]
pub mod mdns;
//pub mod nginx;
pub mod embassy_service_http_server;
pub mod net_controller;
pub mod net_utils;
pub mod proxy_controller;
pub mod ssl;
pub mod CertResolver;
pub mod static_server;
pub mod tor;
pub mod vhost_controller;
pub mod wifi;

const PACKAGE_CERT_PATH: &str = "/var/lib/embassy/ssl";

#[command(subcommands(tor::tor))]
pub fn net() -> Result<(), Error> {
    Ok(())
}

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

pub type HttpClient = Client<hyper::client::HttpConnector>;
