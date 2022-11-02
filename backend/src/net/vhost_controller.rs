use std::collections::BTreeMap;
use std::net::SocketAddr;
use std::sync::Arc;

use tokio_rustls::rustls::ServerConfig;

use crate::net::embassy_service_http_server::{EmbassyCertResolver, EmbassyServiceHTTPServer};

use crate::net::HttpHandler;
use crate::Error;
use crate::net::net_utils::Fqdn;

pub struct VHOSTController {
    pub service_servers: BTreeMap<u16, EmbassyServiceHTTPServer>,
    pub cert_resolver: EmbassyCertResolver,
    embassyd_addr: SocketAddr,
}

impl VHOSTController {
    pub fn init(embassyd_addr: SocketAddr) -> Self {
        dbg!("vhost controller init");
        Self {
            embassyd_addr,
            service_servers: BTreeMap::new(),
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
        fqdn: Fqdn,
        svc_handler: HttpHandler,
        is_ssl: bool,
    ) -> Result<(), Error> {
        if let Some(server) = self.service_servers.get_mut(&external_svc_port) {
            server.add_svc_handler_mapping(fqdn, svc_handler).await?;
        } else {
            self.add_server(is_ssl, external_svc_port, fqdn, svc_handler)
                .await?;
        }

        Ok(())
    }

    async fn add_server(
        &mut self,
        is_ssl: bool,
        external_svc_port: u16,
        fqdn: Fqdn,
        svc_handler: HttpHandler,
    ) -> Result<(), Error> {
        let ssl_cfg = if is_ssl {
            Some(self.build_ssl_svr_cfg()?)
        } else {
            None
        };

        dbg!("add server fqdn:", fqdn.clone());

        let mut new_service_server =
            EmbassyServiceHTTPServer::new(self.embassyd_addr.ip(), external_svc_port, ssl_cfg)
                .await?;
        new_service_server
            .add_svc_handler_mapping(fqdn, svc_handler)
            .await?;
        self.service_servers
            .insert(external_svc_port, new_service_server);
   
        Ok(())
    }
}
