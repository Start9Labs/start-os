use std::collections::BTreeMap;
use std::net::SocketAddr;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Weak};
use std::time::Duration;

use clap::Parser;
use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::{FutureExt, TryStreamExt};
use helpers::NonDetachingJoinHandle;
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::net::TcpStream;
use tokio::process::Command;
use tokio::sync::{mpsc, oneshot};
use tokio::time::Instant;
use torut::control::{AsyncEvent, AuthenticatedConn, ConnError};
use torut::onion::{OnionAddressV3, TorSecretKeyV3};
use tracing::instrument;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::logs::{journalctl, LogSource, LogsParams};
use crate::prelude::*;
use crate::util::serde::{display_serializable, HandlerExtSerde, WithIoFormat};
use crate::util::Invoke;

pub const SYSTEMD_UNIT: &str = "tor@default";
const STARTING_HEALTH_TIMEOUT: u64 = 120; // 2min

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct OnionStore(BTreeMap<OnionAddressV3, TorSecretKeyV3>);
impl Map for OnionStore {
    type Key = OnionAddressV3;
    type Value = TorSecretKeyV3;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key.get_address_without_dot_onion())
    }
}
impl OnionStore {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn insert(&mut self, key: TorSecretKeyV3) {
        self.0.insert(key.public().get_onion_address(), key);
    }
}
impl Model<OnionStore> {
    pub fn new_key(&mut self) -> Result<TorSecretKeyV3, Error> {
        let key = TorSecretKeyV3::generate();
        self.insert(&key.public().get_onion_address(), &key)?;
        Ok(key)
    }
    pub fn insert_key(&mut self, key: &TorSecretKeyV3) -> Result<(), Error> {
        self.insert(&key.public().get_onion_address(), &key)
    }
    pub fn get_key(&self, address: &OnionAddressV3) -> Result<TorSecretKeyV3, Error> {
        self.as_idx(address).or_not_found(address)?.de()
    }
}

enum ErrorLogSeverity {
    Fatal { wipe_state: bool },
    Unknown { wipe_state: bool },
}

lazy_static! {
    static ref LOG_REGEXES: Vec<(Regex, ErrorLogSeverity)> = vec![(
        Regex::new("This could indicate a route manipulation attack, network overload, bad local network connectivity, or a bug\\.").unwrap(),
        ErrorLogSeverity::Unknown { wipe_state: true }
    ),(
        Regex::new("died due to an invalid selected path").unwrap(),
        ErrorLogSeverity::Fatal { wipe_state: false }
    ),(
        Regex::new("Tor has not observed any network activity for the past").unwrap(),
        ErrorLogSeverity::Unknown { wipe_state: false }
    )];
    static ref PROGRESS_REGEX: Regex = Regex::new("PROGRESS=([0-9]+)").unwrap();
}

pub fn tor<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "list-services",
            from_fn_async(list_services)
                .with_display_serializable()
                .with_custom_display_fn(|handle, result| {
                    Ok(display_services(handle.params, result))
                })
                .with_call_remote::<CliContext>(),
        )
        .subcommand("logs", logs())
        .subcommand(
            "logs",
            from_fn_async(crate::logs::cli_logs::<RpcContext, Empty>).no_display(),
        )
        .subcommand(
            "reset",
            from_fn_async(reset)
                .no_display()
                .with_call_remote::<CliContext>(),
        )
}
#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct ResetParams {
    #[arg(name = "wipe-state", short = 'w', long = "wipe-state")]
    wipe_state: bool,
    reason: String,
}

pub async fn reset(
    ctx: RpcContext,
    ResetParams { reason, wipe_state }: ResetParams,
) -> Result<(), Error> {
    ctx.net_controller
        .tor
        .reset(wipe_state, Error::new(eyre!("{reason}"), ErrorKind::Tor))
        .await
}

pub fn display_services(params: WithIoFormat<Empty>, services: Vec<OnionAddressV3>) {
    use prettytable::*;

    if let Some(format) = params.format {
        return display_serializable(format, services);
    }

    let mut table = Table::new();
    for service in services {
        let row = row![&service.to_string()];
        table.add_row(row);
    }
    table.print_tty(false).unwrap();
}

pub async fn list_services(ctx: RpcContext, _: Empty) -> Result<Vec<OnionAddressV3>, Error> {
    ctx.net_controller.tor.list_services().await
}

pub fn logs() -> ParentHandler<RpcContext, LogsParams> {
    crate::logs::logs::<RpcContext, Empty>(|_: &RpcContext, _| async {
        Ok(LogSource::Unit(SYSTEMD_UNIT))
    })
}

fn event_handler(_event: AsyncEvent<'static>) -> BoxFuture<'static, Result<(), ConnError>> {
    async move { Ok(()) }.boxed()
}

pub struct TorController(TorControl);
impl TorController {
    pub fn new(tor_control: SocketAddr, tor_socks: SocketAddr) -> Self {
        TorController(TorControl::new(tor_control, tor_socks))
    }

    pub async fn add(
        &self,
        key: TorSecretKeyV3,
        bindings: Vec<(u16, SocketAddr)>,
    ) -> Result<Vec<Arc<()>>, Error> {
        let (reply, res) = oneshot::channel();
        self.0
            .send
            .send(TorCommand::AddOnion {
                key,
                bindings,
                reply,
            })
            .map_err(|_| Error::new(eyre!("TorControl died"), ErrorKind::Tor))?;
        res.await
            .map_err(|_| Error::new(eyre!("TorControl died"), ErrorKind::Tor))
    }

    pub async fn gc(
        &self,
        addr: Option<OnionAddressV3>,
        external: Option<u16>,
    ) -> Result<(), Error> {
        self.0
            .send
            .send(TorCommand::GC { addr, external })
            .ok()
            .ok_or_else(|| Error::new(eyre!("TorControl died"), ErrorKind::Tor))
    }

    pub async fn reset(&self, wipe_state: bool, context: Error) -> Result<(), Error> {
        self.0
            .send
            .send(TorCommand::Reset {
                wipe_state,
                context,
            })
            .ok()
            .ok_or_else(|| Error::new(eyre!("TorControl died"), ErrorKind::Tor))
    }

    pub async fn list_services(&self) -> Result<Vec<OnionAddressV3>, Error> {
        let (reply, res) = oneshot::channel();
        self.0
            .send
            .send(TorCommand::GetInfo {
                query: "onions/current".into(),
                reply,
            })
            .ok()
            .ok_or_else(|| Error::new(eyre!("TorControl died"), ErrorKind::Tor))?;
        res.await
            .ok()
            .ok_or_else(|| Error::new(eyre!("TorControl died"), ErrorKind::Tor))??
            .lines()
            .map(|l| l.trim())
            .filter(|l| !l.is_empty())
            .map(|l| l.parse::<OnionAddressV3>().with_kind(ErrorKind::Tor))
            .collect()
    }
}

type AuthenticatedConnection = AuthenticatedConn<
    TcpStream,
    Box<dyn Fn(AsyncEvent<'static>) -> BoxFuture<'static, Result<(), ConnError>> + Send + Sync>,
>;

enum TorCommand {
    AddOnion {
        key: TorSecretKeyV3,
        bindings: Vec<(u16, SocketAddr)>,
        reply: oneshot::Sender<Vec<Arc<()>>>,
    },
    GC {
        addr: Option<OnionAddressV3>,
        external: Option<u16>,
    },
    GetInfo {
        query: String,
        reply: oneshot::Sender<Result<String, Error>>,
    },
    Reset {
        wipe_state: bool,
        context: Error,
    },
}

#[instrument(skip_all)]
async fn torctl(
    tor_control: SocketAddr,
    tor_socks: SocketAddr,
    recv: &mut mpsc::UnboundedReceiver<TorCommand>,
    services: &mut BTreeMap<
        OnionAddressV3,
        (
            TorSecretKeyV3,
            BTreeMap<u16, BTreeMap<SocketAddr, Weak<()>>>,
        ),
    >,
    wipe_state: &AtomicBool,
    health_timeout: &mut Duration,
) -> Result<(), Error> {
    let bootstrap = async {
        if Command::new("systemctl")
            .arg("is-active")
            .arg("--quiet")
            .arg("tor")
            .invoke(ErrorKind::Tor)
            .await
            .is_ok()
        {
            Command::new("systemctl")
                .arg("stop")
                .arg("tor")
                .invoke(ErrorKind::Tor)
                .await?;
            for _ in 0..30 {
                if TcpStream::connect(tor_control).await.is_err() {
                    break;
                }
                tokio::time::sleep(Duration::from_secs(1)).await;
            }
            if TcpStream::connect(tor_control).await.is_ok() {
                return Err(Error::new(
                    eyre!("Tor is failing to shut down"),
                    ErrorKind::Tor,
                ));
            }
        }
        if wipe_state.load(std::sync::atomic::Ordering::SeqCst) {
            tokio::fs::remove_dir_all("/var/lib/tor").await?;
            wipe_state.store(false, std::sync::atomic::Ordering::SeqCst);
        }
        tokio::fs::create_dir_all("/var/lib/tor").await?;
        Command::new("chown")
            .arg("-R")
            .arg("debian-tor")
            .arg("/var/lib/tor")
            .invoke(ErrorKind::Filesystem)
            .await?;
        Command::new("systemctl")
            .arg("start")
            .arg("tor")
            .invoke(ErrorKind::Tor)
            .await?;

        let logs = journalctl(
            LogSource::Unit(SYSTEMD_UNIT),
            0,
            None,
            Some("0"),
            false,
            true,
        )
        .await?;

        let mut tcp_stream = None;
        for _ in 0..60 {
            if let Ok(conn) = TcpStream::connect(tor_control).await {
                tcp_stream = Some(conn);
                break;
            }
            tokio::time::sleep(Duration::from_secs(1)).await;
        }
        let tcp_stream = tcp_stream.ok_or_else(|| {
            Error::new(eyre!("Timed out waiting for tor to start"), ErrorKind::Tor)
        })?;
        tracing::info!("Tor is started");

        let mut conn = torut::control::UnauthenticatedConn::new(tcp_stream);
        let auth = conn
            .load_protocol_info()
            .await?
            .make_auth_data()?
            .ok_or_else(|| eyre!("Cookie Auth Not Available"))
            .with_kind(crate::ErrorKind::Tor)?;
        conn.authenticate(&auth).await?;
        let mut connection: AuthenticatedConnection = conn.into_authenticated().await;
        connection.set_async_event_handler(Some(Box::new(|event| event_handler(event))));

        let mut bootstrapped = false;
        let mut last_increment = (String::new(), Instant::now());
        for _ in 0..300 {
            match connection.get_info("status/bootstrap-phase").await {
                Ok(a) => {
                    if a.contains("TAG=done") {
                        bootstrapped = true;
                        break;
                    }
                    if let Some(p) = PROGRESS_REGEX.captures(&a) {
                        if let Some(p) = p.get(1) {
                            if p.as_str() != &*last_increment.0 {
                                last_increment = (p.as_str().into(), Instant::now());
                            }
                        }
                    }
                }
                Err(e) => {
                    let e = Error::from(e);
                    tracing::error!("{}", e);
                    tracing::debug!("{:?}", e);
                }
            }
            if last_increment.1.elapsed() > Duration::from_secs(30) {
                return Err(Error::new(
                    eyre!("Tor stuck bootstrapping at {}%", last_increment.0),
                    ErrorKind::Tor,
                ));
            }
            tokio::time::sleep(Duration::from_secs(1)).await;
        }
        if !bootstrapped {
            return Err(Error::new(
                eyre!("Timed out waiting for tor to bootstrap"),
                ErrorKind::Tor,
            ));
        }
        Ok((connection, logs))
    };
    let pre_handler = async {
        while let Some(command) = recv.recv().await {
            match command {
                TorCommand::AddOnion {
                    key,
                    bindings,
                    reply,
                } => {
                    let addr = key.public().get_onion_address();
                    let mut service = if let Some((_key, service)) = services.remove(&addr) {
                        debug_assert_eq!(key, _key);
                        service
                    } else {
                        BTreeMap::new()
                    };
                    let mut rcs = Vec::with_capacity(bindings.len());
                    for (external, target) in bindings {
                        let mut binding = service.remove(&external).unwrap_or_default();
                        let rc = if let Some(rc) =
                            Weak::upgrade(&binding.remove(&target).unwrap_or_default())
                        {
                            rc
                        } else {
                            Arc::new(())
                        };
                        binding.insert(target, Arc::downgrade(&rc));
                        service.insert(external, binding);
                        rcs.push(rc);
                    }
                    services.insert(addr, (key, service));
                    reply.send(rcs).unwrap_or_default();
                }
                TorCommand::GetInfo { reply, .. } => {
                    reply
                        .send(Err(Error::new(
                            eyre!("Tor has not finished bootstrapping..."),
                            ErrorKind::Tor,
                        )))
                        .unwrap_or_default();
                }
                TorCommand::GC { .. } => (),
                TorCommand::Reset {
                    wipe_state: new_wipe_state,
                    context,
                } => {
                    wipe_state.fetch_or(new_wipe_state, std::sync::atomic::Ordering::SeqCst);
                    return Err(context);
                }
            }
        }
        Ok(())
    };

    let (mut connection, mut logs) = tokio::select! {
        res = bootstrap => res?,
        res = pre_handler => return res,
    };

    let hck_key = TorSecretKeyV3::generate();
    connection
        .add_onion_v3(
            &hck_key,
            false,
            false,
            false,
            None,
            &mut [(80, SocketAddr::from(([127, 0, 0, 1], 80)))].iter(),
        )
        .await?;

    for (addr, (key, service)) in std::mem::take(services) {
        let bindings = service
            .iter()
            .flat_map(|(ext, int)| {
                int.iter()
                    .find(|(_, rc)| rc.strong_count() > 0)
                    .map(|(addr, _)| (*ext, SocketAddr::from(*addr)))
            })
            .collect::<Vec<_>>();
        if !bindings.is_empty() {
            services.insert(addr, (key.clone(), service));
            connection
                .add_onion_v3(&key, false, false, false, None, &mut bindings.iter())
                .await?;
        }
    }

    let handler = async {
        while let Some(command) = recv.recv().await {
            match command {
                TorCommand::AddOnion {
                    key,
                    bindings,
                    reply,
                } => {
                    let mut rm_res = Ok(());
                    let addr = key.public().get_onion_address();
                    let onion_base = addr.get_address_without_dot_onion();
                    let mut service = if let Some((_key, service)) = services.remove(&addr) {
                        debug_assert_eq!(_key, key);
                        rm_res = connection.del_onion(&onion_base).await;
                        service
                    } else {
                        BTreeMap::new()
                    };
                    let mut rcs = Vec::with_capacity(bindings.len());
                    for (external, target) in bindings {
                        let mut binding = service.remove(&external).unwrap_or_default();
                        let rc = if let Some(rc) =
                            Weak::upgrade(&binding.remove(&target).unwrap_or_default())
                        {
                            rc
                        } else {
                            Arc::new(())
                        };
                        binding.insert(target, Arc::downgrade(&rc));
                        service.insert(external, binding);
                        rcs.push(rc);
                    }
                    let bindings = service
                        .iter()
                        .flat_map(|(ext, int)| {
                            int.iter()
                                .find(|(_, rc)| rc.strong_count() > 0)
                                .map(|(addr, _)| (*ext, SocketAddr::from(*addr)))
                        })
                        .collect::<Vec<_>>();
                    services.insert(addr, (key.clone(), service));
                    reply.send(rcs).unwrap_or_default();
                    rm_res?;
                    connection
                        .add_onion_v3(&key, false, false, false, None, &mut bindings.iter())
                        .await?;
                }
                TorCommand::GC { addr, external } => {
                    for addr in if addr.is_some() {
                        itertools::Either::Left(addr.into_iter())
                    } else {
                        itertools::Either::Right(services.keys().cloned().collect_vec().into_iter())
                    } {
                        if let Some((key, mut service)) = services.remove(&addr) {
                            let onion_base: String = addr.get_address_without_dot_onion();
                            for external in if external.is_some() {
                                itertools::Either::Left(external.into_iter())
                            } else {
                                itertools::Either::Right(
                                    service.keys().copied().collect_vec().into_iter(),
                                )
                            } {
                                if let Some(mut binding) = service.remove(&external) {
                                    binding = binding
                                        .into_iter()
                                        .filter(|(_, rc)| rc.strong_count() > 0)
                                        .collect();
                                    if !binding.is_empty() {
                                        service.insert(external, binding);
                                    }
                                }
                            }
                            let rm_res = connection.del_onion(&onion_base).await;
                            if !service.is_empty() {
                                let bindings = service
                                    .iter()
                                    .flat_map(|(ext, int)| {
                                        int.iter()
                                            .find(|(_, rc)| rc.strong_count() > 0)
                                            .map(|(addr, _)| (*ext, SocketAddr::from(*addr)))
                                    })
                                    .collect::<Vec<_>>();
                                if !bindings.is_empty() {
                                    services.insert(addr, (key.clone(), service));
                                }
                                rm_res?;
                                if !bindings.is_empty() {
                                    connection
                                        .add_onion_v3(
                                            &key,
                                            false,
                                            false,
                                            false,
                                            None,
                                            &mut bindings.iter(),
                                        )
                                        .await?;
                                }
                            } else {
                                rm_res?;
                            }
                        }
                    }
                }
                TorCommand::GetInfo { query, reply } => {
                    reply
                        .send(connection.get_info(&query).await.with_kind(ErrorKind::Tor))
                        .unwrap_or_default();
                }
                TorCommand::Reset {
                    wipe_state: new_wipe_state,
                    context,
                } => {
                    wipe_state.fetch_or(new_wipe_state, std::sync::atomic::Ordering::SeqCst);
                    return Err(context);
                }
            }
        }
        Ok(())
    };
    let log_parser = async {
        while let Some(log) = logs.try_next().await? {
            for (regex, severity) in &*LOG_REGEXES {
                if regex.is_match(&log.message) {
                    let (check, wipe_state) = match severity {
                        ErrorLogSeverity::Fatal { wipe_state } => (false, *wipe_state),
                        ErrorLogSeverity::Unknown { wipe_state } => (true, *wipe_state),
                    };
                    if !check
                        || tokio::time::timeout(
                            Duration::from_secs(30),
                            tokio_socks::tcp::Socks5Stream::connect(
                                tor_socks,
                                (hck_key.public().get_onion_address().to_string(), 80),
                            ),
                        )
                        .await
                        .map_err(|e| tracing::warn!("Tor is confirmed to be down: {e}"))
                        .and_then(|a| {
                            a.map_err(|e| tracing::warn!("Tor is confirmed to be down: {e}"))
                        })
                        .is_err()
                    {
                        if wipe_state {
                            Command::new("systemctl")
                                .arg("stop")
                                .arg("tor")
                                .invoke(ErrorKind::Tor)
                                .await?;
                            tokio::fs::remove_dir_all("/var/lib/tor").await?;
                        }
                        return Err(Error::new(eyre!("{}", log.message), ErrorKind::Tor));
                    }
                }
            }
        }
        Err(Error::new(eyre!("Log stream terminated"), ErrorKind::Tor))
    };
    let health_checker = async {
        let mut last_success = Instant::now();
        loop {
            tokio::time::sleep(Duration::from_secs(30)).await;
            if tokio::time::timeout(
                Duration::from_secs(30),
                tokio_socks::tcp::Socks5Stream::connect(
                    tor_socks,
                    (hck_key.public().get_onion_address().to_string(), 80),
                ),
            )
            .await
            .map_err(|e| e.to_string())
            .and_then(|e| e.map_err(|e| e.to_string()))
            .is_err()
            {
                if last_success.elapsed() > *health_timeout {
                    let err = Error::new(eyre!("Tor health check failed for longer than current timeout ({health_timeout:?})"), crate::ErrorKind::Tor);
                    *health_timeout *= 2;
                    wipe_state.store(true, std::sync::atomic::Ordering::SeqCst);
                    return Err(err);
                }
            } else {
                last_success = Instant::now();
            }
        }
    };

    tokio::select! {
        res = handler => res?,
        res = log_parser => res?,
        res = health_checker => res?,
    }

    Ok(())
}

struct TorControl {
    _thread: NonDetachingJoinHandle<()>,
    send: mpsc::UnboundedSender<TorCommand>,
}
impl TorControl {
    pub fn new(tor_control: SocketAddr, tor_socks: SocketAddr) -> Self {
        let (send, mut recv) = mpsc::unbounded_channel();
        Self {
            _thread: tokio::spawn(async move {
                let mut services = BTreeMap::new();
                let wipe_state = AtomicBool::new(false);
                let mut health_timeout = Duration::from_secs(STARTING_HEALTH_TIMEOUT);
                while let Err(e) = torctl(
                    tor_control,
                    tor_socks,
                    &mut recv,
                    &mut services,
                    &wipe_state,
                    &mut health_timeout,
                )
                .await
                {
                    tracing::error!("{e}: Restarting tor");
                    tracing::debug!("{e:?}");
                }
                tracing::info!("TorControl is shut down.")
            })
            .into(),
            send,
        }
    }
}

#[tokio::test]
#[ignore]
async fn test() {
    let mut conn = torut::control::UnauthenticatedConn::new(
        TcpStream::connect(SocketAddr::from(([127, 0, 0, 1], 9051)))
            .await
            .unwrap(),
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
