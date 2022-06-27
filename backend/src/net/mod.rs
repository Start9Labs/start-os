use std::net::{Ipv4Addr, SocketAddr};
use std::path::PathBuf;

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
use crate::net::dns::DnsController;
use crate::net::interface::TorConfig;
use crate::net::nginx::InterfaceMetadata;
use crate::s9pk::manifest::PackageId;
use crate::Error;

pub mod dns;
pub mod interface;
#[cfg(feature = "avahi")]
pub mod mdns;
pub mod nginx;
pub mod ssl;
pub mod tor;
pub mod wifi;

const PACKAGE_CERT_PATH: &str = "/var/lib/embassy/ssl";

#[command(subcommands(tor::tor))]
pub fn net() -> Result<(), Error> {
    Ok(())
}

/// Indicates that the net controller has created the
/// SSL keys
#[derive(Clone, Copy)]
pub struct GeneratedCertificateMountPoint(());

pub struct NetController {
    pub tor: TorController,
    #[cfg(feature = "avahi")]
    pub mdns: MdnsController,
    pub nginx: NginxController,
    pub ssl: SslManager,
    pub dns: DnsController,
}
impl NetController {
    #[instrument(skip(db))]
    pub async fn init(
        embassyd_addr: SocketAddr,
        embassyd_tor_key: TorSecretKeyV3,
        tor_control: SocketAddr,
        dns_bind: &[SocketAddr],
        db: SqlitePool,
        import_root_ca: Option<(PKey<Private>, X509)>,
    ) -> Result<Self, Error> {
        let ssl = match import_root_ca {
            None => SslManager::init(db).await,
            Some(a) => SslManager::import_root_ca(db, a.0, a.1).await,
        }?;
        Ok(Self {
            tor: TorController::init(embassyd_addr, embassyd_tor_key, tor_control).await?,
            #[cfg(feature = "avahi")]
            mdns: MdnsController::init(),
            nginx: NginxController::init(PathBuf::from("/etc/nginx"), &ssl).await?,
            ssl,
            dns: DnsController::init(dns_bind).await?,
        })
    }

    pub fn ssl_directory_for(pkg_id: &PackageId) -> PathBuf {
        PathBuf::from(format!("{}/{}", PACKAGE_CERT_PATH, pkg_id))
    }

    #[instrument(skip(self, interfaces, _generated_certificate))]
    pub async fn add<'a, I>(
        &self,
        pkg_id: &PackageId,
        ip: Ipv4Addr,
        interfaces: I,
        _generated_certificate: GeneratedCertificateMountPoint,
    ) -> Result<(), Error>
    where
        I: IntoIterator<Item = (InterfaceId, &'a Interface, TorSecretKeyV3)> + Clone,
        for<'b> &'b I: IntoIterator<Item = &'b (InterfaceId, &'a Interface, TorSecretKeyV3)>,
    {
        let interfaces_tor = interfaces
            .clone()
            .into_iter()
            .filter_map(|i| match i.1.tor_config.clone() {
                None => None,
                Some(cfg) => Some((i.0, cfg, i.2)),
            })
            .collect::<Vec<(InterfaceId, TorConfig, TorSecretKeyV3)>>();
        let (tor_res, _, nginx_res, _) = tokio::join!(
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
            },
            self.dns.add(pkg_id, ip),
        );
        tor_res?;
        nginx_res?;

        Ok(())
    }

    #[instrument(skip(self, interfaces))]
    pub async fn remove<I: IntoIterator<Item = InterfaceId> + Clone>(
        &self,
        pkg_id: &PackageId,
        ip: Ipv4Addr,
        interfaces: I,
    ) -> Result<(), Error> {
        let (tor_res, _, nginx_res, _) = tokio::join!(
            self.tor.remove(pkg_id, interfaces.clone()),
            {
                #[cfg(feature = "avahi")]
                let mdns_fut = self.mdns.remove(pkg_id, interfaces);
                #[cfg(not(feature = "avahi"))]
                let mdns_fut = futures::future::ready(());
                mdns_fut
            },
            self.nginx.remove(pkg_id),
            self.dns.remove(pkg_id, ip),
        );
        tor_res?;
        nginx_res?;
        Ok(())
    }

    pub async fn generate_certificate_mountpoint<'a, I>(
        &self,
        pkg_id: &PackageId,
        interfaces: &I,
    ) -> Result<GeneratedCertificateMountPoint, Error>
    where
        I: IntoIterator<Item = (InterfaceId, &'a Interface, TorSecretKeyV3)> + Clone,
        for<'b> &'b I: IntoIterator<Item = &'b (InterfaceId, &'a Interface, TorSecretKeyV3)>,
    {
        tracing::info!("Generating SSL Certificate mountpoints for {}", pkg_id);
        let package_path = PathBuf::from(PACKAGE_CERT_PATH).join(pkg_id);
        tokio::fs::create_dir_all(&package_path).await?;
        for (id, _, key) in interfaces {
            let dns_base = OnionAddressV3::from(&key.public()).get_address_without_dot_onion();
            let ssl_path_key = package_path.join(format!("{}.key.pem", id));
            let ssl_path_cert = package_path.join(format!("{}.cert.pem", id));
            let (key, chain) = self.ssl.certificate_for(&dns_base, pkg_id).await?;
            tokio::try_join!(
                crate::net::ssl::export_key(&key, &ssl_path_key),
                crate::net::ssl::export_cert(&chain, &ssl_path_cert)
            )?;
        }
        Ok(GeneratedCertificateMountPoint(()))
    }

    pub async fn export_root_ca(&self) -> Result<(PKey<Private>, X509), Error> {
        self.ssl.export_root_ca().await
    }
}
