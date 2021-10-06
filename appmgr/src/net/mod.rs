use std::net::{Ipv4Addr, SocketAddr};
use std::path::PathBuf;

use rpc_toolkit::command;
use sqlx::SqlitePool;
use torut::onion::{OnionAddressV3, TorSecretKeyV3};

use self::interface::{Interface, InterfaceId};
#[cfg(feature = "avahi")]
use self::mdns::MdnsController;
use self::nginx::NginxController;
use self::tor::TorController;
use crate::net::interface::TorConfig;
use crate::net::nginx::InterfaceMetadata;
use crate::s9pk::manifest::PackageId;
use crate::Error;

pub mod interface;
#[cfg(feature = "avahi")]
pub mod mdns;
pub mod nginx;
pub mod ssl;
pub mod tor;
pub mod wifi;

#[command(subcommands(tor::tor))]
pub fn net() -> Result<(), Error> {
    Ok(())
}

pub struct NetController {
    pub tor: TorController,
    #[cfg(feature = "avahi")]
    pub mdns: MdnsController,
    pub nginx: NginxController,
}
impl NetController {
    pub async fn init(
        embassyd_addr: SocketAddr,
        embassyd_tor_key: TorSecretKeyV3,
        tor_control: SocketAddr,
        db: SqlitePool,
    ) -> Result<Self, Error> {
        Ok(Self {
            tor: TorController::init(embassyd_addr, embassyd_tor_key, tor_control).await?,
            #[cfg(feature = "avahi")]
            mdns: MdnsController::init(),
            nginx: NginxController::init(PathBuf::from("/etc/nginx"), db).await?,
        })
    }

    pub async fn add<
        'a,
        I: IntoIterator<Item = (InterfaceId, &'a Interface, TorSecretKeyV3)> + Clone,
    >(
        &self,
        pkg_id: &PackageId,
        ip: Ipv4Addr,
        interfaces: I,
    ) -> Result<(), Error> {
        let interfaces_tor = interfaces
            .clone()
            .into_iter()
            .filter_map(|i| match i.1.tor_config.clone() {
                None => None,
                Some(cfg) => Some((i.0, cfg, i.2)),
            })
            .collect::<Vec<(InterfaceId, TorConfig, TorSecretKeyV3)>>();
        let (tor_res, _, nginx_res) = tokio::join!(
            self.tor.add(pkg_id, ip, interfaces_tor),
            {
                #[cfg(feature = "avahi")]
                let mdns_fut = self.mdns.add(
                    pkg_id,
                    interfaces
                        .clone()
                        .into_iter()
                        .map(|(interface_id, _, key)| (interface_id, key)),
                );
                #[cfg(not(feature = "avahi"))]
                let mdns_fut = futures::future::ready(());
                mdns_fut
            },
            {
                let interfaces = interfaces
                    .into_iter()
                    .filter_map(|(id, interface, tor_key)| match &interface.lan_config {
                        None => None,
                        Some(cfg) => Some((
                            id,
                            InterfaceMetadata {
                                dns_base: OnionAddressV3::from(&tor_key.public())
                                    .get_address_without_dot_onion(),
                                lan_config: cfg.clone(),
                                protocols: interface.protocols.clone(),
                            },
                        )),
                    });
                self.nginx.add(pkg_id.clone(), ip, interfaces)
            }
        );
        tor_res?;
        nginx_res?;

        Ok(())
    }

    pub async fn remove<I: IntoIterator<Item = InterfaceId> + Clone>(
        &self,
        pkg_id: &PackageId,
        interfaces: I,
    ) -> Result<(), Error> {
        let (tor_res, _, nginx_res) = tokio::join!(
            self.tor.remove(pkg_id, interfaces.clone()),
            {
                #[cfg(feature = "avahi")]
                let mdns_fut = self.mdns.remove(pkg_id, interfaces);
                #[cfg(not(feature = "avahi"))]
                let mdns_fut = futures::future::ready(());
                mdns_fut
            },
            self.nginx.remove(pkg_id)
        );
        tor_res?;
        nginx_res?;
        Ok(())
    }
}
