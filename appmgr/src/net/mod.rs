use std::net::{Ipv4Addr, SocketAddr};
use std::path::PathBuf;

use futures::FutureExt;
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use rpc_toolkit::command;
use sqlx::SqlitePool;
use torut::onion::{OnionAddressV3, TorSecretKeyV3};
use tracing::instrument;

use self::interface::{Interface, InterfaceId};
#[cfg(feature = "avahi")]
use self::mdns::MdnsController;
use self::nginx::NginxController;
use self::ssl::SslManager;
use self::tor::TorController;
use crate::net::interface::TorConfig;
use crate::net::nginx::InterfaceMetadata;
use crate::s9pk::manifest::PackageId;
use crate::{Error, ErrorKind, ResultExt};

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
    pub ssl: SslManager,
}
impl NetController {
    #[instrument(skip(db))]
    pub async fn init(
        embassyd_addr: SocketAddr,
        embassyd_tor_key: TorSecretKeyV3,
        tor_control: SocketAddr,
        db: SqlitePool,
        import_root_ca: Option<(PKey<Private>, X509)>,
    ) -> Result<Self, Error> {
        let ssl = match import_root_ca {
            None => SslManager::init(db).await,
            Some(a) => SslManager::import_root_ca(db, a.0, a.1).await,
        }?;

        // write main ssl key/cert to fs location
        let nginx_root = PathBuf::from("/etc/nginx");
        let (key, cert) = ssl
            .certificate_for(&crate::hostname::get_hostname().await?)
            .await?;
        let ssl_path_key = nginx_root.join(format!("ssl/embassy_main.key.pem"));
        let ssl_path_cert = nginx_root.join(format!("ssl/embassy_main.cert.pem"));
        tokio::try_join!(
            tokio::fs::write(&ssl_path_key, key.private_key_to_pem_pkcs8()?),
            tokio::fs::write(
                &ssl_path_cert,
                cert.into_iter()
                    .flat_map(|c| c.to_pem().unwrap())
                    .collect::<Vec<u8>>()
            )
        )?;

        Ok(Self {
            tor: TorController::init(embassyd_addr, embassyd_tor_key, tor_control).await?,
            #[cfg(feature = "avahi")]
            mdns: MdnsController::init(),
            nginx: NginxController::init(nginx_root, &ssl).await?,
            ssl,
        })
    }

    #[instrument(skip(self, interfaces))]
    pub async fn add<'a, I>(
        &self,
        pkg_id: &PackageId,
        ip: Ipv4Addr,
        interfaces: I,
    ) -> Result<(), Error>
    where
        I: IntoIterator<Item = (InterfaceId, &'a Interface, TorSecretKeyV3)> + Clone,
        for<'b> &'b I: IntoIterator<Item = &'b (InterfaceId, &'a Interface, TorSecretKeyV3)>,
    {
        let package_path = self.nginx.nginx_root.join(format!("ssl/{}", pkg_id));
        tokio::fs::create_dir_all(package_path).await?;
        // write certificates for all interfaces
        for (id, _, key) in &interfaces {
            let dns_base = OnionAddressV3::from(&key.public()).get_address_without_dot_onion();
            let ssl_path_key = self
                .nginx
                .nginx_root
                .join(format!("ssl/{}/{}.key.pem", pkg_id, id));
            let ssl_path_cert = self
                .nginx
                .nginx_root
                .join(format!("ssl/{}/{}.cert.pem", pkg_id, id));
            let (key, chain) = self.ssl.certificate_for(&dns_base).await?;
            tokio::try_join!(
                tokio::fs::write(&ssl_path_key, key.private_key_to_pem_pkcs8()?).map(|res| res
                    .with_ctx(|_| (ErrorKind::Filesystem, ssl_path_key.display().to_string()))),
                tokio::fs::write(
                    &ssl_path_cert,
                    chain
                        .into_iter()
                        .flat_map(|c| c.to_pem().unwrap())
                        .collect::<Vec<u8>>()
                )
                .map(|res| res
                    .with_ctx(|_| (ErrorKind::Filesystem, ssl_path_cert.display().to_string()))),
            )?;
        }

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
                self.nginx.add(&self.ssl, pkg_id.clone(), ip, interfaces)
            }
        );
        tor_res?;
        nginx_res?;

        Ok(())
    }

    #[instrument(skip(self, interfaces))]
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

    pub async fn export_root_ca(&self) -> Result<(PKey<Private>, X509), Error> {
        self.ssl.export_root_ca().await
    }
}
