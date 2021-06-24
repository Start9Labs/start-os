use std::net::{Ipv4Addr, SocketAddr};
use std::sync::Arc;

use anyhow::anyhow;
use futures::future::BoxFuture;
use futures::FutureExt;
use indexmap::IndexMap;
use patch_db::DbHandle;
use sqlx::{Executor, Sqlite};
use tokio::net::TcpStream;
use tokio::sync::RwLock;
use torut::control::{AsyncEvent, AuthenticatedConn, ConnError};
use torut::onion::TorSecretKeyV3;

use super::interface::TorConfig;
use crate::{Error, ResultExt as _};

fn event_handler(event: AsyncEvent<'static>) -> BoxFuture<'static, Result<(), ConnError>> {
    async move { Ok(()) }.boxed()
}

#[derive(Clone)]
pub struct TorController(Arc<RwLock<TorControllerInner>>);
impl TorController {
    pub async fn init<Db: DbHandle, Ex>(
        tor_cp: SocketAddr,
        db: &mut Db,
        secrets: &mut Ex,
    ) -> Result<Self, Error>
    where
        for<'a> &'a mut Ex: Executor<'a, Database = Sqlite>,
    {
        Ok(TorController(Arc::new(RwLock::new(
            TorControllerInner::init(tor_cp, db, secrets).await?,
        ))))
    }

    pub async fn sync<Db: DbHandle, Ex>(&self, db: &mut Db, secrets: &mut Ex) -> Result<(), Error>
    where
        for<'a> &'a mut Ex: Executor<'a, Database = Sqlite>,
    {
        let new = TorControllerInner::get_services(db, secrets).await?;
        if &new != &self.0.read().await.services {
            self.0.write().await.sync(new).await?;
        }
        Ok(())
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
    services: IndexMap<[u8; 64], HiddenServiceConfig>,
}
impl TorControllerInner {
    async fn get_services<Db: DbHandle, Ex>(
        db: &mut Db,
        secrets: &mut Ex,
    ) -> Result<IndexMap<[u8; 64], HiddenServiceConfig>, Error>
    where
        for<'a> &'a mut Ex: Executor<'a, Database = Sqlite>,
    {
        let pkg_ids = crate::db::DatabaseModel::new()
            .package_data()
            .keys(db)
            .await?;
        let mut services = IndexMap::new();
        for pkg_id in pkg_ids {
            if let Some(installed) = crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(&pkg_id)
                .expect(db)
                .await?
                .installed()
                .check(db)
                .await?
            {
                let ifaces = installed
                    .clone()
                    .manifest()
                    .interfaces()
                    .get(db)
                    .await?
                    .to_owned();
                for (iface_id, cfgs) in ifaces.0 {
                    if let Some(tor_cfg) = cfgs.tor_config {
                        if let Some(key) = sqlx::query!(
                            "SELECT key FROM tor WHERE package = ? AND interface = ?",
                            *pkg_id,
                            *iface_id,
                        )
                        .fetch_optional(&mut *secrets)
                        .await?
                        {
                            if key.key.len() != 64 {
                                return Err(Error::new(
                                    anyhow!("Invalid key length"),
                                    crate::ErrorKind::Database,
                                ));
                            }
                            let mut buf = [0; 64];
                            buf.clone_from_slice(&key.key);
                            services.insert(
                                buf,
                                HiddenServiceConfig {
                                    ip: installed
                                        .clone()
                                        .interface_info()
                                        .ip()
                                        .get(db)
                                        .await?
                                        .to_owned(),
                                    cfg: tor_cfg,
                                },
                            );
                        }
                    }
                }
            }
        }
        Ok(services)
    }

    async fn add_svc(
        &mut self,
        key: &TorSecretKeyV3,
        config: &HiddenServiceConfig,
    ) -> Result<(), Error> {
        self.connection
            .add_onion_v3(
                key,
                false,
                false,
                false,
                None,
                &mut config
                    .cfg
                    .port_mapping
                    .iter()
                    .map(|(external, internal)| {
                        (*external, SocketAddr::from((config.ip, *internal)))
                    })
                    .collect::<Vec<_>>()
                    .iter(),
            )
            .await?;
        Ok(())
    }

    async fn sync(
        &mut self,
        services: IndexMap<[u8; 64], HiddenServiceConfig>,
    ) -> Result<(), Error> {
        for (key, new) in &services {
            let tor_key = TorSecretKeyV3::from(key.clone());
            if let Some(old) = self.services.remove(&key[..]) {
                if new != &old {
                    self.connection
                        .del_onion(
                            &tor_key
                                .public()
                                .get_onion_address()
                                .get_address_without_dot_onion(),
                        )
                        .await?;
                    self.add_svc(&tor_key, new).await?;
                }
            } else {
                self.add_svc(&tor_key, new).await?;
            }
        }
        for (key, _) in self.services.drain(..) {
            self.connection
                .del_onion(
                    &TorSecretKeyV3::from(key)
                        .public()
                        .get_onion_address()
                        .get_address_without_dot_onion(),
                )
                .await?;
        }
        self.services = services;
        Ok(())
    }

    async fn init<Db: DbHandle, Ex>(
        tor_cp: SocketAddr,
        db: &mut Db,
        secrets: &mut Ex,
    ) -> Result<Self, Error>
    where
        for<'a> &'a mut Ex: Executor<'a, Database = Sqlite>,
    {
        let mut conn = torut::control::UnauthenticatedConn::new(
            TcpStream::connect(tor_cp).await?, // TODO
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
        let mut res = TorControllerInner {
            connection,
            services: IndexMap::new(),
        };
        res.sync(Self::get_services(db, secrets).await?).await?;
        Ok(res)
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
