use std::net::{Ipv4Addr, SocketAddr, SocketAddrV4};
use std::sync::Arc;
use std::time::Duration;

use socks5_impl::protocol::{Address, Reply};
use socks5_impl::server::auth::NoAuth;
use socks5_impl::server::{AuthAdaptor, ClientConnection, Server};
use tokio::net::{TcpListener, TcpStream};

use crate::HOST_IP;
use crate::net::tor::TorController;
use crate::prelude::*;
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::future::NonDetachingJoinHandle;

pub const DEFAULT_SOCKS_LISTEN: SocketAddr = SocketAddr::V4(SocketAddrV4::new(
    Ipv4Addr::new(HOST_IP[0], HOST_IP[1], HOST_IP[2], HOST_IP[3]),
    9050,
));

pub struct SocksController {
    _thread: NonDetachingJoinHandle<()>,
}
impl SocksController {
    pub fn new(listen: SocketAddr, tor: TorController) -> Result<Self, Error> {
        Ok(Self {
            _thread: tokio::spawn(async move {
                let auth: AuthAdaptor<()> = Arc::new(NoAuth);
                let listener;
                loop {
                    if let Some(l) = TcpListener::bind(listen)
                        .await
                        .with_kind(ErrorKind::Network)
                        .log_err()
                    {
                        listener = l;
                        break;
                    }
                    tokio::time::sleep(Duration::from_secs(1)).await;
                }
                let (bg, mut runner) = BackgroundJobQueue::new();
                runner
                    .run_while(async {
                        let server = Server::new(listener, auth);
                        loop {
                            match server.accept().await {
                                Ok((stream, _)) => {
                                    let tor = tor.clone();
                                    bg.add_job(async move {
                                        if let Err(e) = async {
                                            match stream
                                                .authenticate()
                                                .await
                                                .with_kind(ErrorKind::Network)?
                                                .0
                                                .wait_request()
                                                .await
                                                .with_kind(ErrorKind::Network)?
                                            {
                                                ClientConnection::Connect(
                                                    reply,
                                                    Address::DomainAddress(domain, port),
                                                ) if domain.ends_with(".onion") => {
                                                    if let Ok(mut target) = tor
                                                        .connect_onion(&domain.parse()?, port)
                                                        .await
                                                    {
                                                        let mut sock = reply
                                                            .reply(
                                                                Reply::Succeeded,
                                                                Address::unspecified(),
                                                            )
                                                            .await
                                                            .with_kind(ErrorKind::Network)?;
                                                        tokio::io::copy_bidirectional(
                                                            &mut sock,
                                                            &mut target,
                                                        )
                                                        .await
                                                        .with_kind(ErrorKind::Network)?;
                                                    } else {
                                                        let mut sock = reply
                                                            .reply(
                                                                Reply::HostUnreachable,
                                                                Address::unspecified(),
                                                            )
                                                            .await
                                                            .with_kind(ErrorKind::Network)?;
                                                        sock.shutdown()
                                                            .await
                                                            .with_kind(ErrorKind::Network)?;
                                                    }
                                                }
                                                ClientConnection::Connect(reply, addr) => {
                                                    if let Ok(mut target) = match addr {
                                                        Address::DomainAddress(domain, port) => {
                                                            TcpStream::connect((domain, port)).await
                                                        }
                                                        Address::SocketAddress(addr) => {
                                                            TcpStream::connect(addr).await
                                                        }
                                                    } {
                                                        let mut sock = reply
                                                            .reply(
                                                                Reply::Succeeded,
                                                                Address::unspecified(),
                                                            )
                                                            .await
                                                            .with_kind(ErrorKind::Network)?;
                                                        tokio::io::copy_bidirectional(
                                                            &mut sock,
                                                            &mut target,
                                                        )
                                                        .await
                                                        .with_kind(ErrorKind::Network)?;
                                                    } else {
                                                        let mut sock = reply
                                                            .reply(
                                                                Reply::HostUnreachable,
                                                                Address::unspecified(),
                                                            )
                                                            .await
                                                            .with_kind(ErrorKind::Network)?;
                                                        sock.shutdown()
                                                            .await
                                                            .with_kind(ErrorKind::Network)?;
                                                    }
                                                }
                                                ClientConnection::Bind(bind, _) => {
                                                    let mut sock = bind
                                                        .reply(
                                                            Reply::CommandNotSupported,
                                                            Address::unspecified(),
                                                        )
                                                        .await
                                                        .with_kind(ErrorKind::Network)?;
                                                    sock.shutdown()
                                                        .await
                                                        .with_kind(ErrorKind::Network)?;
                                                }
                                                ClientConnection::UdpAssociate(associate, _) => {
                                                    let mut sock = associate
                                                        .reply(
                                                            Reply::CommandNotSupported,
                                                            Address::unspecified(),
                                                        )
                                                        .await
                                                        .with_kind(ErrorKind::Network)?;
                                                    sock.shutdown()
                                                        .await
                                                        .with_kind(ErrorKind::Network)?;
                                                }
                                            }

                                            Ok::<_, Error>(())
                                        }
                                        .await
                                        {
                                            tracing::trace!("SOCKS5 Stream Error: {e}");
                                            tracing::trace!("{e:?}");
                                        }
                                    });
                                }
                                Err(e) => {
                                    tracing::error!("SOCKS5 TCP Accept Error: {e}");
                                    tracing::debug!("{e:?}");
                                }
                            }
                        }
                    })
                    .await;
            })
            .into(),
        })
    }
}
