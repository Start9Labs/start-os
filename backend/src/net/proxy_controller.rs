use color_eyre::eyre::eyre;
use std::collections::BTreeMap;
use std::net::{Ipv4Addr, SocketAddr};

use models::{InterfaceId, PackageId};
use tokio::sync::Mutex;
use tracing::{info, instrument};

use crate::net::embassy_service_http_server::EmbassyServiceHTTPServer;
use crate::net::ssl::SslManager;
use crate::net::vhost_controller::VHOSTController;
use crate::net::{InterfaceMetadata, PackageNetInfo};

use crate::{Error, ResultExt};

pub struct ProxyController {
    inner: Mutex<ProxyControllerInner>,
}

impl ProxyController {
    pub async fn init(embassyd_addr: SocketAddr, ssl_manager: &SslManager) -> Result<Self, Error> {
        Ok(ProxyController {
            inner: Mutex::new(ProxyControllerInner::init(embassyd_addr, ssl_manager).await?),
        })
    }

    pub async fn add<I: IntoIterator<Item = (InterfaceId, InterfaceMetadata)>>(
        &self,
        ssl_manager: &SslManager,
        package: PackageId,
        ipv4: Ipv4Addr,
        interfaces: I,
    ) -> Result<(), Error> {
        self.inner
            .lock()
            .await
            .add(ssl_manager, package, ipv4, interfaces)
            .await
    }

    pub async fn remove(&self, package: &PackageId) -> Result<(), Error> {
        self.inner.lock().await.remove(package).await
    }
}
struct ProxyControllerInner {
    embassyd_addr: SocketAddr,
    vhosts: VHOSTController, //  service_servers: BTreeMap<u16, EmbassyServiceHTTPServer>,
}

impl ProxyControllerInner {
    #[instrument]
    async fn init(embassyd_addr: SocketAddr, ssl_manager: &SslManager) -> Result<Self, Error> {
        let inner = ProxyControllerInner {
            embassyd_addr,
            vhosts: todo!(),
        };

        // let emnbassyd_port_80_svc = EmbassyHTTPServer::new(embassyd_addr).await?;
        Ok(inner)
    }

    async fn proxy(client: HttpClient, req: Request<Body>) -> Result<Response<Body>, HyperError> {
        if Method::CONNECT == req.method() {
            // Received an HTTP request like:
            // ```
            // CONNECT www.domain.com:443 HTTP/1.1s
            // Host: www.domain.com:443
            // Proxy-Connection: Keep-Alive
            // ```
            //
            // When HTTP method is CONNECT we should return an empty body
            // then we can eventually upgrade the connection and talk a new protocol.
            //
            // Note: only after client received an empty body with STATUS_OK can the
            // connection be upgraded, so we can't return a response inside
            // `on_upgrade` future.
            match Self::host_addr(&req) {
                Ok(host) => {
                    tokio::task::spawn(async move {
                        match hyper::upgrade::on(req).await {
                            Ok(upgraded) => {
                                if let Err(e) = Self::tunnel(upgraded, host).await {
                                    error!("server io error: {}", e);
                                };
                            }
                            Err(e) => error!("upgrade error: {}", e),
                        }
                    });

                    Ok(Response::new(Body::empty()))
                }
                Err(e) => {
                    let err_txt = format!("CONNECT host is not socket addr: {:?}", &req.uri());
                    let mut resp = Response::new(Body::from(format!(
                        "CONNECT must be to a socket address: {}: {}",
                        err_txt, e
                    )));
                    *resp.status_mut() = http::StatusCode::BAD_REQUEST;

                    Ok(resp)
                }
            }
        } else {
            client.request(req).await
        }
    }

    // Create a TCP connection to host:port, build a tunnel between the connection and
    // the upgraded connection
    async fn tunnel(mut upgraded: Upgraded, addr: String) -> std::io::Result<()> {
        let mut server = TcpStream::connect(addr).await?;

        let (from_client, from_server) =
            tokio::io::copy_bidirectional(&mut upgraded, &mut server).await?;

        info!(
            "client wrote {} bytes and received {} bytes",
            from_client, from_server
        );

        Ok(())
    }

    fn host_addr(req: &Request<Body>) -> Result<String, Error> {
        let host = req.headers().get(http::header::HOST);

        match host {
            Some(host) => {
                let host = host
                    .to_str()
                    .map_err(|e| Error::new(eyre!("{}", e), crate::ErrorKind::AsciiError))?
                    .to_string();

                Ok(host)
            }

            None => Err(Error::new(eyre!("No Host"), crate::ErrorKind::NoHost)),
        }
    }

}
