use std::collections::HashMap;
use std::net::{Ipv4Addr, SocketAddr};
use std::time::Duration;

use anyhow::anyhow;
use futures::future::BoxFuture;
use futures::FutureExt;
use tokio::net::TcpStream;
use tokio::sync::Mutex;
use torut::control::{AsyncEvent, AuthenticatedConn, ConnError};
use torut::onion::{OnionAddressV3, TorSecretKey, TorSecretKeyV3};

use super::interface::{InterfaceId, TorConfig};
use crate::s9pk::manifest::PackageId;
use crate::{Error, ResultExt as _};

fn event_handler(event: AsyncEvent<'static>) -> BoxFuture<'static, Result<(), ConnError>> {
    async move { Ok(()) }.boxed()
}

pub struct TorController(Mutex<TorControllerInner>);
impl TorController {
    pub async fn init(
        embassyd_addr: SocketAddr,
        embassyd_tor_key: TorSecretKeyV3,
        tor_control: SocketAddr,
    ) -> Result<Self, Error> {
        Ok(TorController(Mutex::new(
            TorControllerInner::init(embassyd_addr, embassyd_tor_key, tor_control).await?,
        )))
    }

    pub async fn add<
        'a,
        I: IntoIterator<Item = (InterfaceId, TorConfig, TorSecretKeyV3)> + Clone,
    >(
        &self,
        pkg_id: &PackageId,
        ip: Ipv4Addr,
        interfaces: I,
    ) -> Result<(), Error> {
        self.0.lock().await.add(pkg_id, ip, interfaces).await
    }

    pub async fn remove<I: IntoIterator<Item = InterfaceId> + Clone>(
        &self,
        pkg_id: &PackageId,
        interfaces: I,
    ) -> Result<(), Error> {
        self.0.lock().await.remove(pkg_id, interfaces).await
    }

    pub async fn replace(&self) -> Result<(), Error> {
        self.0.lock().await.replace().await
    }

    pub async fn embassyd_onion(&self) -> OnionAddressV3 {
        self.0.lock().await.embassyd_onion()
    }
}

type AuthenticatedConnection = AuthenticatedConn<
    TcpStream,
    fn(AsyncEvent<'static>) -> BoxFuture<'static, Result<(), ConnError>>,
>;

#[derive(Clone, Debug, PartialEq, Eq)]
struct HiddenServiceConfig {
    ip: Ipv4Addr,
    cfg: TorConfig,
}

pub struct TorControllerInner {
    embassyd_addr: SocketAddr,
    embassyd_tor_key: TorSecretKeyV3,
    control_addr: SocketAddr,
    connection: Option<AuthenticatedConnection>,
    services: HashMap<(PackageId, InterfaceId), (TorSecretKeyV3, TorConfig, Ipv4Addr)>,
}
impl TorControllerInner {
    async fn add<'a, I: IntoIterator<Item = (InterfaceId, TorConfig, TorSecretKeyV3)>>(
        &mut self,
        pkg_id: &PackageId,
        ip: Ipv4Addr,
        interfaces: I,
    ) -> Result<(), Error> {
        for (interface_id, tor_cfg, key) in interfaces {
            let id = (pkg_id.clone(), interface_id);
            match self.services.get(&id) {
                Some(k) if k.0 != key => {
                    self.remove(pkg_id, std::iter::once(id.1.clone())).await?;
                }
                Some(_) => return Ok(()), // TODO: is this right??? if a single interface key matches we terminate the whole loop??
                None => (),
            }
            match self.connection.as_mut() {
                None => unreachable!(),
                Some(c) => {
                    c.add_onion_v3(
                        &key,
                        false,
                        false,
                        false,
                        None,
                        &mut tor_cfg
                            .port_mapping
                            .iter()
                            .map(|(external, internal)| {
                                (external.0, SocketAddr::from((ip, internal.0)))
                            })
                            .collect::<Vec<_>>()
                            .iter(),
                    )
                    .await?;
                }
            }
            self.services.insert(id, (key, tor_cfg.clone(), ip));
        }
        Ok(())
    }

    async fn remove<I: IntoIterator<Item = InterfaceId>>(
        &mut self,
        pkg_id: &PackageId,
        interfaces: I,
    ) -> Result<(), Error> {
        for interface_id in interfaces {
            if let Some((key, cfg, ip)) = self.services.remove(&(pkg_id.clone(), interface_id)) {
                match self.connection.as_mut() {
                    None => unreachable!(),
                    Some(c) => {
                        c.del_onion(
                            &key.public()
                                .get_onion_address()
                                .get_address_without_dot_onion(),
                        )
                        .await?;
                    }
                }
            }
        }
        Ok(())
    }

    async fn init(
        embassyd_addr: SocketAddr,
        embassyd_tor_key: TorSecretKeyV3,
        tor_control: SocketAddr,
    ) -> Result<Self, Error> {
        let mut conn = torut::control::UnauthenticatedConn::new(
            TcpStream::connect(tor_control).await?, // TODO
        );
        let auth = conn
            .load_protocol_info()
            .await?
            .make_auth_data()?
            .ok_or_else(|| anyhow!("Cookie Auth Not Available"))
            .with_kind(crate::ErrorKind::Tor)?;
        conn.authenticate(&auth).await?;
        let mut connection: AuthenticatedConnection = conn.into_authenticated().await;
        connection.set_async_event_handler(Some(event_handler));
        Ok(TorControllerInner {
            embassyd_addr,
            embassyd_tor_key,
            control_addr: tor_control,
            connection: Some(connection),
            services: HashMap::new(),
        })
    }

    async fn replace(&mut self) -> Result<(), Error> {
        let connection = self.connection.take();
        match connection {
            // this should be unreachable because the only time when this should be none is for the duration of tor's
            // restart lower down in this method, which is held behind a Mutex
            None => unreachable!(),
            Some(mut c) => {
                // when connection closes below, tor daemon is restarted
                c.take_ownership().await?;
                // this should close the connection
                drop(c);

                // wait for environment to restart tor
                // TODO: consider taking over tor administration to get better visibility into whether it is running or
                // not
                tokio::time::sleep(Duration::from_secs(1)).await;

                // attempt to reconnect to the control socket, not clear how long this should take
                let stream;
                loop {
                    match TcpStream::connect(self.control_addr).await {
                        Ok(s) => {
                            stream = s;
                            break;
                        }
                        Err(e) => {
                            log::info!("Failed to reconnect to tor control socket: {}", e);
                            tokio::time::sleep(Duration::from_secs(1)).await;
                        }
                    }
                }
                let mut new_conn = torut::control::UnauthenticatedConn::new(stream);
                let auth = new_conn
                    .load_protocol_info()
                    .await?
                    .make_auth_data()?
                    .ok_or_else(|| anyhow!("Cookie Auth Not Available"))
                    .with_kind(crate::ErrorKind::Tor)?;
                new_conn.authenticate(&auth).await?;
                let mut new_connection: AuthenticatedConnection =
                    new_conn.into_authenticated().await;
                new_connection.set_async_event_handler(Some(event_handler));

                // replace the connection object here on the new copy of the tor daemon
                self.connection.replace(new_connection);

                // swap empty map for owned old service map
                let old_services = std::mem::replace(&mut self.services, HashMap::new());

                // re add all of the services on the new control socket
                for ((package_id, interface_id), (tor_key, tor_cfg, ipv4)) in old_services {
                    self.add(
                        &package_id,
                        ipv4,
                        std::iter::once((interface_id, tor_cfg, tor_key)),
                    )
                    .await?;
                }
            }
        }
        Ok(())
    }

    fn embassyd_onion(&self) -> OnionAddressV3 {
        self.embassyd_tor_key.public().get_onion_address()
    }
}

#[tokio::test]
async fn test() {
    let mut conn = torut::control::UnauthenticatedConn::new(
        TcpStream::connect(SocketAddr::from(([127, 0, 0, 1], 9051)))
            .await
            .unwrap(), // TODO
    );
    let auth = conn
        .load_protocol_info()
        .await
        .unwrap()
        .make_auth_data()
        .unwrap()
        .ok_or_else(|| anyhow!("Cookie Auth Not Available"))
        .with_kind(crate::ErrorKind::Tor)
        .unwrap();
    conn.authenticate(&auth).await.unwrap();
    let mut connection: AuthenticatedConn<
        TcpStream,
        fn(AsyncEvent<'static>) -> BoxFuture<'static, Result<(), ConnError>>,
    > = conn.into_authenticated().await;
    let tor_key = torut::onion::TorSecretKeyV3::generate();
    dbg!(connection.get_conf("SocksPort").await.unwrap());
    connection
        .add_onion_v3(
            &tor_key,
            false,
            false,
            false,
            None,
            &mut [(443_u16, SocketAddr::from(([127, 0, 0, 1], 8443)))].iter(),
        )
        .await
        .unwrap();
    connection
        .add_onion_v3(
            &tor_key,
            false,
            false,
            false,
            None,
            &mut [(8443_u16, SocketAddr::from(([127, 0, 0, 1], 8443)))].iter(),
        )
        .await
        .unwrap();
}
