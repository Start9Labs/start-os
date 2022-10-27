use crate::context::RpcContext;
use crate::hostname::{get_hostname, HostNameReceipt};
#[cfg(feature = "avahi")]
use crate::net::mdns::MdnsController;
use crate::net::{
    static_server, GeneratedCertificateMountPoint, InterfaceMetadata, PACKAGE_CERT_PATH,
};
use crate::Error;
use hyper::Client;
use models::InterfaceId;
use openssl::pkey::{PKey, Private};
use openssl::x509::X509;
use patch_db::{DbHandle, PatchDb};
use sqlx::PgPool;
use std::net::{Ipv4Addr, SocketAddr};
use std::path::PathBuf;
use torut::onion::{OnionAddressV3, TorSecretKeyV3};
use tracing::instrument;

use crate::net::dns::DnsController;
use crate::net::interface::{Interface, TorConfig};
use crate::net::proxy_controller::ProxyController;

use crate::net::ssl::SslManager;
use crate::net::tor::TorController;
use crate::s9pk::manifest::PackageId;

pub struct NetController {
    pub tor: TorController,
    #[cfg(feature = "avahi")]
    pub mdns: MdnsController,
    //pub nginx: NginxController,
    pub proxy: ProxyController,
    pub ssl: SslManager,
    pub dns: DnsController,
}
impl NetController {
    #[instrument(skip(db, db_handle))]
    pub async fn init<Db: DbHandle>(
        embassyd_addr: SocketAddr,
        embassyd_tor_key: TorSecretKeyV3,
        tor_control: SocketAddr,
        dns_bind: &[SocketAddr],
        db: PgPool,
        db_handle: &mut Db,
        import_root_ca: Option<(PKey<Private>, X509)>,
    ) -> Result<Self, Error> {
        let receipts = HostNameReceipt::new(db_handle).await?;
        let embassy_host_name = get_hostname(db_handle, &receipts).await?;
        let name = embassy_host_name.local_name();

    

        let ssl = match import_root_ca {
            None => SslManager::init(db.clone(), db_handle).await,
            Some(a) => SslManager::import_root_ca(db.clone(), a.0, a.1).await,
        }?;
        dbg!("got ssl cert for root ca");

        Ok(Self {
            tor: TorController::init(embassyd_addr, embassyd_tor_key, tor_control).await?,
            #[cfg(feature = "avahi")]
            mdns: MdnsController::init().await?,
            //nginx: NginxController::init(PathBuf::from("/etc/nginx"), &ssl).await?,
            proxy: ProxyController::init(embassyd_addr, name, ssl.clone()).await?,
            ssl,
            dns: DnsController::init(dns_bind).await?,
        })
    }

    pub fn ssl_directory_for(pkg_id: &PackageId) -> PathBuf {
        PathBuf::from(PACKAGE_CERT_PATH).join(pkg_id)
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
        dbg!("this is the start of add()");
        let interfaces_tor = interfaces
            .clone()
            .into_iter()
            .filter_map(|i| match i.1.tor_config.clone() {
                None => None,
                Some(cfg) => Some((i.0, cfg, i.2)),
            })
            .collect::<Vec<(InterfaceId, TorConfig, TorSecretKeyV3)>>();
        dbg!("got interfaces_tor");
        let (tor_res, _, proxy_res, _) = tokio::join!(
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
                dbg!("mdns add: done");

                #[cfg(not(feature = "avahi"))]
                let mdns_fut = futures::future::ready(());
                mdns_fut
            },
            {
                let interfaces =
                    interfaces
                        .clone()
                        .into_iter()
                        .filter_map(|(id, interface, tor_key)| {
                            interface.lan_config.as_ref().map(|cfg| {
                                (
                                    id,
                                    InterfaceMetadata {
                                        fqdn: OnionAddressV3::from(&tor_key.public())
                                            .get_address_without_dot_onion()
                                            + ".local",
                                        lan_config: cfg.clone(),
                                        protocols: interface.protocols.clone(),
                                    },
                                )
                            })
                        });
                //self.nginx.add(&self.ssl, pkg_id.clone(), ip, interfaces)
                dbg!("adding docker svc to proxy"); 
                self.proxy
                    .add_docker_service(pkg_id.clone(), ip, interfaces)
            },
            self.dns.add(pkg_id, ip),
        );
        tor_res?;
        //nginx_res?;
        proxy_res?;

        dbg!("proxy:: add done");

        Ok(())
    }

    #[instrument(skip(self, interfaces))]
    pub async fn remove<I: IntoIterator<Item = InterfaceId> + Clone>(
        &self,
        pkg_id: &PackageId,
        ip: Ipv4Addr,
        interfaces: I,
    ) -> Result<(), Error> {
        let (tor_res, _, proxy_res, _) = tokio::join!(
            self.tor.remove(pkg_id, interfaces.clone()),
            {
                #[cfg(feature = "avahi")]
                let mdns_fut = self.mdns.remove(pkg_id, interfaces);
                #[cfg(not(feature = "avahi"))]
                let mdns_fut = futures::future::ready(());
                mdns_fut
            },
            self.proxy.remove_docker_service(pkg_id),
            self.dns.remove(pkg_id, ip),
        );
        tor_res?;
        proxy_res?;
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
