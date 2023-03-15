use std::collections::BTreeMap;
use std::net::SocketAddr;
use std::sync::{Arc, Weak};

use clap::ArgMatches;
use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::FutureExt;
use rpc_toolkit::command;
use tokio::net::TcpStream;
use tokio::sync::Mutex;
use torut::control::{AsyncEvent, AuthenticatedConn, ConnError};
use torut::onion::{OnionAddressV3, TorSecretKeyV3};
use tracing::instrument;

use crate::context::RpcContext;
use crate::util::serde::{display_serializable, IoFormat};
use crate::{Error, ErrorKind, ResultExt as _};

#[test]
fn random_key() {
    println!("x'{}'", hex::encode(rand::random::<[u8; 32]>()));
}

#[command(subcommands(list_services))]
pub fn tor() -> Result<(), Error> {
    Ok(())
}

fn display_services(services: Vec<OnionAddressV3>, matches: &ArgMatches) {
    use prettytable::*;

    if matches.is_present("format") {
        return display_serializable(services, matches);
    }

    let mut table = Table::new();
    for service in services {
        let row = row![&service.to_string()];
        table.add_row(row);
    }
    table.print_tty(false).unwrap();
}

#[command(rename = "list-services", display(display_services))]
pub async fn list_services(
    #[context] ctx: RpcContext,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<Vec<OnionAddressV3>, Error> {
    ctx.net_controller.tor.list_services().await
}

fn event_handler(_event: AsyncEvent<'static>) -> BoxFuture<'static, Result<(), ConnError>> {
    async move { Ok(()) }.boxed()
}

pub struct TorController(Mutex<TorControllerInner>);
impl TorController {
    pub async fn init(tor_control: SocketAddr) -> Result<Self, Error> {
        Ok(TorController(Mutex::new(
            TorControllerInner::init(tor_control).await?,
        )))
    }

    pub async fn add(
        &self,
        key: &TorSecretKeyV3,
        external: u16,
        target: SocketAddr,
    ) -> Result<Arc<()>, Error> {
        self.0.lock().await.add(key, external, target).await
    }

    pub async fn gc(&self, key: &TorSecretKeyV3, external: u16) -> Result<(), Error> {
        self.0.lock().await.gc(key, external).await
    }

    pub async fn list_services(&self) -> Result<Vec<OnionAddressV3>, Error> {
        self.0.lock().await.list_services().await
    }
}

type AuthenticatedConnection = AuthenticatedConn<
    TcpStream,
    fn(AsyncEvent<'static>) -> BoxFuture<'static, Result<(), ConnError>>,
>;

pub struct TorControllerInner {
    control_addr: SocketAddr,
    connection: AuthenticatedConnection,
    services: BTreeMap<String, BTreeMap<u16, BTreeMap<SocketAddr, Weak<()>>>>,
}
impl TorControllerInner {
    #[instrument(skip_all)]
    async fn add(
        &mut self,
        key: &TorSecretKeyV3,
        external: u16,
        target: SocketAddr,
    ) -> Result<Arc<()>, Error> {
        let mut rm_res = Ok(());
        let onion_base = key
            .public()
            .get_onion_address()
            .get_address_without_dot_onion();
        let mut service = if let Some(service) = self.services.remove(&onion_base) {
            rm_res = self.connection.del_onion(&onion_base).await;
            service
        } else {
            BTreeMap::new()
        };
        let mut binding = service.remove(&external).unwrap_or_default();
        let rc = if let Some(rc) = Weak::upgrade(&binding.remove(&target).unwrap_or_default()) {
            rc
        } else {
            Arc::new(())
        };
        binding.insert(target, Arc::downgrade(&rc));
        service.insert(external, binding);
        let bindings = service
            .iter()
            .flat_map(|(ext, int)| {
                int.iter()
                    .find(|(_, rc)| rc.strong_count() > 0)
                    .map(|(addr, _)| (*ext, SocketAddr::from(*addr)))
            })
            .collect::<Vec<_>>();
        self.services.insert(onion_base, service);
        rm_res?;
        self.connection
            .add_onion_v3(key, false, false, false, None, &mut bindings.iter())
            .await?;
        Ok(rc)
    }

    #[instrument(skip_all)]
    async fn gc(&mut self, key: &TorSecretKeyV3, external: u16) -> Result<(), Error> {
        let onion_base = key
            .public()
            .get_onion_address()
            .get_address_without_dot_onion();
        if let Some(mut service) = self.services.remove(&onion_base) {
            if let Some(mut binding) = service.remove(&external) {
                binding = binding
                    .into_iter()
                    .filter(|(_, rc)| rc.strong_count() > 0)
                    .collect();
                if !binding.is_empty() {
                    service.insert(external, binding);
                }
            }
            let rm_res = self.connection.del_onion(&onion_base).await;
            if !service.is_empty() {
                let bindings = service
                    .iter()
                    .flat_map(|(ext, int)| {
                        int.iter()
                            .find(|(_, rc)| rc.strong_count() > 0)
                            .map(|(addr, _)| (*ext, SocketAddr::from(*addr)))
                    })
                    .collect::<Vec<_>>();
                self.services.insert(onion_base, service);
                rm_res?;
                self.connection
                    .add_onion_v3(&key, false, false, false, None, &mut bindings.iter())
                    .await?;
            } else {
                rm_res?;
            }
        }

        Ok(())
    }

    #[instrument(skip_all)]
    async fn init(tor_control: SocketAddr) -> Result<Self, Error> {
        let mut conn = torut::control::UnauthenticatedConn::new(
            TcpStream::connect(tor_control).await?, // TODO
        );
        let auth = conn
            .load_protocol_info()
            .await?
            .make_auth_data()?
            .ok_or_else(|| eyre!("Cookie Auth Not Available"))
            .with_kind(crate::ErrorKind::Tor)?;
        conn.authenticate(&auth).await?;
        let mut connection: AuthenticatedConnection = conn.into_authenticated().await;
        connection.set_async_event_handler(Some(event_handler));

        Ok(Self {
            control_addr: tor_control,
            connection,
            services: BTreeMap::new(),
        })
    }

    #[instrument(skip_all)]
    async fn list_services(&mut self) -> Result<Vec<OnionAddressV3>, Error> {
        self.connection
            .get_info("onions/current")
            .await?
            .lines()
            .map(|l| l.trim())
            .filter(|l| !l.is_empty())
            .map(|l| l.parse().with_kind(ErrorKind::Tor))
            .collect()
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
        .ok_or_else(|| eyre!("Cookie Auth Not Available"))
        .with_kind(crate::ErrorKind::Tor)
        .unwrap();
    conn.authenticate(&auth).await.unwrap();
    let mut connection: AuthenticatedConn<
        TcpStream,
        fn(AsyncEvent<'static>) -> BoxFuture<'static, Result<(), ConnError>>,
    > = conn.into_authenticated().await;
    let tor_key = torut::onion::TorSecretKeyV3::generate();
    connection.get_conf("SocksPort").await.unwrap();
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
        .del_onion(
            &tor_key
                .public()
                .get_onion_address()
                .get_address_without_dot_onion(),
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
