use std::net::{Ipv4Addr, SocketAddr};

use torut::onion::TorSecretKeyV3;

use self::interface::{Interface, InterfaceId};
#[cfg(feature = "avahi")]
use self::mdns::MdnsController;
use self::tor::TorController;
use crate::s9pk::manifest::PackageId;
use crate::{Error, ResultExt};

pub mod interface;
#[cfg(feature = "avahi")]
pub mod mdns;
pub mod tor;
pub mod wifi;

pub struct NetController {
    tor: TorController,
    #[cfg(feature = "avahi")]
    mdns: MdnsController,
    // nginx: NginxController, // TODO
}
impl NetController {
    pub async fn init(tor_control: SocketAddr) -> Result<Self, Error> {
        Ok(Self {
            tor: TorController::init(tor_control).await?,
            #[cfg(feature = "avahi")]
            mdns: MdnsController::init(),
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
        let (tor_res, _) = tokio::join!(self.tor.add(pkg_id, ip, interfaces.clone()), {
            #[cfg(feature = "avahi")]
            let mdns_fut = self.mdns.add(
                pkg_id,
                interfaces
                    .into_iter()
                    .map(|(interface_id, _, key)| (interface_id, key)),
            );
            #[cfg(not(feature = "avahi"))]
            let mdns_fut = futures::future::ready(());
            mdns_fut
        },);
        tor_res?;
        Ok(())
    }

    pub async fn remove<I: IntoIterator<Item = InterfaceId> + Clone>(
        &self,
        pkg_id: &PackageId,
        interfaces: I,
    ) -> Result<(), Error> {
        let (tor_res, _) = tokio::join!(self.tor.remove(pkg_id, interfaces.clone()), {
            #[cfg(feature = "avahi")]
            let mdns_fut = self.mdns.remove(pkg_id, interfaces);
            #[cfg(not(feature = "avahi"))]
            let mdns_fut = futures::future::ready(());
            mdns_fut
        });
        tor_res?;
        Ok(())
    }
}
