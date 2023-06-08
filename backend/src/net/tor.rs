use std::collections::BTreeMap;
use std::net::SocketAddr;
use std::sync::{Arc, Weak};
use std::time::Duration;

use clap::ArgMatches;
use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::{FutureExt, TryStreamExt};
use helpers::NonDetachingJoinHandle;
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;
use tokio::net::TcpStream;
use tokio::process::Command;
use tokio::sync::{mpsc, oneshot};
use tokio::time::Instant;
use torut::control::{AsyncEvent, AuthenticatedConn, ConnError};
use torut::onion::{OnionAddressV3, TorSecretKeyV3};
use tracing::instrument;

use crate::context::{CliContext, RpcContext};
use crate::logs::{
    cli_logs_generic_follow, cli_logs_generic_nofollow, fetch_logs, follow_logs, journalctl,
    LogFollowResponse, LogResponse, LogSource,
};
use crate::util::serde::{display_serializable, IoFormat};
use crate::util::{display_none, Invoke};
use crate::{Error, ErrorKind, ResultExt as _};

pub const SYSTEMD_UNIT: &str = "tor@default";

enum ErrorLogSeverity {
    Fatal { wipe_state: bool },
    Unknown { wipe_state: bool },
}

lazy_static! {
    static ref LOG_REGEXES: Vec<(Regex, ErrorLogSeverity)> = vec![(
        Regex::new("Most likely this means the Tor network is overloaded").unwrap(),
        ErrorLogSeverity::Unknown { wipe_state: true }
    ),(
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

#[test]
fn random_key() {
    println!("x'{}'", hex::encode(rand::random::<[u8; 32]>()));
}

#[command(subcommands(list_services, logs, reset))]
pub fn tor() -> Result<(), Error> {
    Ok(())
}

#[command(display(display_none))]
pub async fn reset(
    #[context] ctx: RpcContext,
    #[arg(rename = "wipe-state", short = 'w', long = "wipe-state")] wipe_state: bool,
    #[arg] reason: String,
) -> Result<(), Error> {
    ctx.net_controller
        .tor
        .reset(wipe_state, Error::new(eyre!("{reason}"), ErrorKind::Tor))
        .await
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

#[command(
    custom_cli(cli_logs(async, context(CliContext))),
    subcommands(self(logs_nofollow(async)), logs_follow),
    display(display_none)
)]
pub async fn logs(
    #[arg(short = 'l', long = "limit")] limit: Option<usize>,
    #[arg(short = 'c', long = "cursor")] cursor: Option<String>,
    #[arg(short = 'B', long = "before", default)] before: bool,
    #[arg(short = 'f', long = "follow", default)] follow: bool,
) -> Result<(Option<usize>, Option<String>, bool, bool), Error> {
    Ok((limit, cursor, before, follow))
}
pub async fn cli_logs(
    ctx: CliContext,
    (limit, cursor, before, follow): (Option<usize>, Option<String>, bool, bool),
) -> Result<(), RpcError> {
    if follow {
        if cursor.is_some() {
            return Err(RpcError::from(Error::new(
                eyre!("The argument '--cursor <cursor>' cannot be used with '--follow'"),
                crate::ErrorKind::InvalidRequest,
            )));
        }
        if before {
            return Err(RpcError::from(Error::new(
                eyre!("The argument '--before' cannot be used with '--follow'"),
                crate::ErrorKind::InvalidRequest,
            )));
        }
        cli_logs_generic_follow(ctx, "net.tor.logs.follow", None, limit).await
    } else {
        cli_logs_generic_nofollow(ctx, "net.tor.logs", None, limit, cursor, before).await
    }
}
pub async fn logs_nofollow(
    _ctx: (),
    (limit, cursor, before, _): (Option<usize>, Option<String>, bool, bool),
) -> Result<LogResponse, Error> {
    fetch_logs(LogSource::Service(SYSTEMD_UNIT), limit, cursor, before).await
}

#[command(rpc_only, rename = "follow", display(display_none))]
pub async fn logs_follow(
    #[context] ctx: RpcContext,
    #[parent_data] (limit, _, _, _): (Option<usize>, Option<String>, bool, bool),
) -> Result<LogFollowResponse, Error> {
    follow_logs(ctx, LogSource::Service(SYSTEMD_UNIT), limit).await
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
        external: u16,
        target: SocketAddr,
    ) -> Result<Arc<()>, Error> {
        let (reply, res) = oneshot::channel();
        self.0
            .send
            .send(TorCommand::AddOnion {
                key,
                external,
                target,
                reply,
            })
            .ok()
            .ok_or_else(|| Error::new(eyre!("TorControl died"), ErrorKind::Tor))?;
        res.await
            .ok()
            .ok_or_else(|| Error::new(eyre!("TorControl died"), ErrorKind::Tor))
    }

    pub async fn gc(
        &self,
        key: Option<TorSecretKeyV3>,
        external: Option<u16>,
    ) -> Result<(), Error> {
        self.0
            .send
            .send(TorCommand::GC { key, external })
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
            .map(|l| l.parse().with_kind(ErrorKind::Tor))
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
        external: u16,
        target: SocketAddr,
        reply: oneshot::Sender<Arc<()>>,
    },
    GC {
        key: Option<TorSecretKeyV3>,
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
    services: &mut BTreeMap<[u8; 64], BTreeMap<u16, BTreeMap<SocketAddr, Weak<()>>>>,
) -> Result<(), Error> {
    let bootstrap = async {
        tokio::fs::create_dir_all("/var/lib/tor").await?;
        Command::new("chown")
            .arg("-R")
            .arg("debian-tor")
            .arg("/var/lib/tor")
            .invoke(ErrorKind::Filesystem)
            .await?;
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
        }
        Command::new("systemctl")
            .arg("start")
            .arg("tor")
            .invoke(ErrorKind::Tor)
            .await?;

        let logs = journalctl(LogSource::Service(SYSTEMD_UNIT), 0, None, false, true).await?;

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
                    external,
                    target,
                    reply,
                } => {
                    let mut service = if let Some(service) = services.remove(&key.as_bytes()) {
                        service
                    } else {
                        BTreeMap::new()
                    };
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
                    services.insert(key.as_bytes(), service);
                    reply.send(rc).unwrap_or_default();
                }
                TorCommand::GetInfo { reply, .. } => {
                    reply
                        .send(Err(Error::new(
                            eyre!("Tor has not finished bootstrapping..."),
                            ErrorKind::Tor,
                        )))
                        .unwrap_or_default();
                }
                _ => (),
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

    for (key, service) in &*services {
        let key = TorSecretKeyV3::from(*key);
        let bindings = service
            .iter()
            .flat_map(|(ext, int)| {
                int.iter()
                    .find(|(_, rc)| rc.strong_count() > 0)
                    .map(|(addr, _)| (*ext, SocketAddr::from(*addr)))
            })
            .collect::<Vec<_>>();
        connection
            .add_onion_v3(&key, false, false, false, None, &mut bindings.iter())
            .await?;
    }

    let handler = async {
        while let Some(command) = recv.recv().await {
            match command {
                TorCommand::AddOnion {
                    key,
                    external,
                    target,
                    reply,
                } => {
                    let mut rm_res = Ok(());
                    let onion_base = key
                        .public()
                        .get_onion_address()
                        .get_address_without_dot_onion();
                    let mut service = if let Some(service) = services.remove(&key.as_bytes()) {
                        rm_res = connection.del_onion(&onion_base).await;
                        service
                    } else {
                        BTreeMap::new()
                    };
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
                    let bindings = service
                        .iter()
                        .flat_map(|(ext, int)| {
                            int.iter()
                                .find(|(_, rc)| rc.strong_count() > 0)
                                .map(|(addr, _)| (*ext, SocketAddr::from(*addr)))
                        })
                        .collect::<Vec<_>>();
                    services.insert(key.as_bytes(), service);
                    reply.send(rc).unwrap_or_default();
                    rm_res?;
                    connection
                        .add_onion_v3(&key, false, false, false, None, &mut bindings.iter())
                        .await?;
                }
                TorCommand::GC { key, external } => {
                    for key in if key.is_some() {
                        itertools::Either::Left(key.into_iter().map(|k| k.as_bytes()))
                    } else {
                        itertools::Either::Right(services.keys().cloned().collect_vec().into_iter())
                    } {
                        let key = TorSecretKeyV3::from(key);
                        let onion_base = key
                            .public()
                            .get_onion_address()
                            .get_address_without_dot_onion();
                        if let Some(mut service) = services.remove(&key.as_bytes()) {
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
                                services.insert(key.as_bytes(), service);
                                rm_res?;
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
                    wipe_state,
                    context,
                } => {
                    if wipe_state {
                        Command::new("systemctl")
                            .arg("stop")
                            .arg("tor")
                            .invoke(ErrorKind::Tor)
                            .await?;
                        tokio::fs::remove_dir_all("/var/lib/tor").await?;
                    }
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

    tokio::select! {
        res = handler => res?,
        res = log_parser => res?,
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
                while let Err(e) = torctl(tor_control, tor_socks, &mut recv, &mut services).await {
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
