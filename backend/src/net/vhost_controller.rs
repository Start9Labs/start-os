use crate::net::embassy_service_http_server::{EmbassyCertResolver, EmbassyServiceHTTPServer};
use crate::net::proxy_controller::ProxyController;
use crate::net::ssl::SslManager;
use crate::Error;
use futures::FutureExt;
use lazy_static::__Deref;
use models::PackageId;
use std::collections::BTreeMap;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio_rustls::rustls::server::ResolvesServerCertUsingSni;
use tokio_rustls::rustls::ServerConfig;

use crate::net::{HttpClient, HttpHandler};

pub struct VHOSTController {
    pub service_servers: BTreeMap<u16, EmbassyServiceHTTPServer>,
    pub svc_dns_base_package_names: BTreeMap<String, PackageId>,
    pub cert_resolver: EmbassyCertResolver,
    embassyd_addr: SocketAddr,
}

impl VHOSTController {
    pub fn init(embassyd_addr: SocketAddr) -> Self {
        dbg!("vhost controller init");
        Self {
            embassyd_addr,
            service_servers: BTreeMap::new(),
            svc_dns_base_package_names: BTreeMap::new(),
            cert_resolver: EmbassyCertResolver::new(),
        }
        
    }

    pub fn build_ssl_svr_cfg(&self) -> Result<Arc<ServerConfig>, Error> {
        let ssl_cfg = ServerConfig::builder()
            .with_safe_default_cipher_suites()
            .with_safe_default_kx_groups()
            .with_safe_default_protocol_versions()
            .unwrap()
            .with_no_client_auth()
            .with_cert_resolver(Arc::new(self.cert_resolver.clone()));

        Ok(Arc::new(ssl_cfg))
    }

    pub async fn add_server_or_handle(
        &mut self,
        external_svc_port: u16,
        fqdn: String,
        svc_handler: HttpHandler,
        is_ssl: bool,
    ) -> Result<(), Error> {
        if let Some(server) = self.service_servers.get_mut(&external_svc_port) {
            server.add_svc_mapping(fqdn, svc_handler).await;
        } else {
            let ssl_cfg = if is_ssl {
                Some(self.build_ssl_svr_cfg()?)
            } else {
                None
            };
            let mut new_service_server =
                EmbassyServiceHTTPServer::new(self.embassyd_addr.ip(), external_svc_port, ssl_cfg)
                    .await?;
            new_service_server.add_svc_mapping(fqdn, svc_handler).await;

            self.service_servers
                .insert(external_svc_port, new_service_server);
        }

        Ok(())
    }
}
