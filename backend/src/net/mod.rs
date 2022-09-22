use std::collections::BTreeMap;

use indexmap::IndexSet;
use rpc_toolkit::command;

use self::interface::InterfaceId;

use crate::net::interface::LanPortConfig;

use crate::util::serde::Port;
use crate::Error;

pub mod dns;
pub mod interface;
#[cfg(feature = "avahi")]
pub mod mdns;
//pub mod nginx;
pub mod embassy_service_http_server;
pub mod net_controller;
pub mod proxy_controller;
pub mod ssl;
pub mod static_server;
pub mod tor;
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
