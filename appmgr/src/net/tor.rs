use std::collections::HashMap;
use std::net::{Ipv4Addr, SocketAddr};

use anyhow::anyhow;
use futures::future::BoxFuture;
use futures::FutureExt;
use tokio::net::TcpStream;
use tokio::sync::Mutex;
use torut::control::{AsyncEvent, AuthenticatedConn, ConnError};
use torut::onion::TorSecretKeyV3;

use super::interface::{Interface, InterfaceId, TorConfig};
use crate::s9pk::manifest::PackageId;
use crate::{Error, ResultExt as _};

fn event_handler(event: AsyncEvent<'static>) -> BoxFuture<'static, Result<(), ConnError>> {
    async move { Ok(()) }.boxed()
}

pub struct TorController(Mutex<TorControllerInner>);
impl TorController {
    pub async fn init(tor_control: SocketAddr) -> Result<Self, Error> {
        Ok(TorController(Mutex::new(
            TorControllerInner::init(tor_control).await?,
        )))
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
        self.0.lock().await.add(pkg_id, ip, interfaces).await
    }

    pub async fn remove<I: IntoIterator<Item = InterfaceId> + Clone>(
        &self,
        pkg_id: &PackageId,
        interfaces: I,
    ) -> Result<(), Error> {
        self.0.lock().await.remove(pkg_id, interfaces).await
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
    connection: AuthenticatedConnection,
    services: HashMap<(PackageId, InterfaceId), TorSecretKeyV3>,
}
impl TorControllerInner {
    async fn add<'a, I: IntoIterator<Item = (InterfaceId, &'a Interface, TorSecretKeyV3)>>(
        &mut self,
        pkg_id: &PackageId,
        ip: Ipv4Addr,
        interfaces: I,
    ) -> Result<(), Error> {
        for (interface_id, interface, key) in interfaces {
            let id = (pkg_id.clone(), interface_id);
            match self.services.get(&id) {
                Some(k) if k != &key => {
                    self.remove(pkg_id, std::iter::once(id.1.clone())).await?;
                }
                Some(_) => return Ok(()),
                None => (),
            }
            if let Some(tor_cfg) = &interface.tor_config {
                self.connection
                    .add_onion_v3(
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
            self.services.insert(id, key);
        }
        Ok(())
    }

    async fn remove<I: IntoIterator<Item = InterfaceId>>(
        &mut self,
        pkg_id: &PackageId,
        interfaces: I,
    ) -> Result<(), Error> {
        for interface_id in interfaces {
            if let Some(key) = self.services.remove(&(pkg_id.clone(), interface_id)) {
                self.connection
                    .del_onion(
                        &key.public()
                            .get_onion_address()
                            .get_address_without_dot_onion(),
                    )
                    .await?;
            }
        }
        Ok(())
    }

    async fn init(tor_control: SocketAddr) -> Result<Self, Error> {
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
            connection,
            services: HashMap::new(),
        })
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
