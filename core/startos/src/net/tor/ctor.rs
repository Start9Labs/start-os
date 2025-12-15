use std::collections::{BTreeMap, BTreeSet};
use std::net::{Ipv4Addr, SocketAddr, SocketAddrV4};
use std::str::FromStr;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Weak};
use std::time::Duration;

use base64::Engine;
use clap::Parser;
use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::{FutureExt, TryFutureExt, TryStreamExt};
use helpers::NonDetachingJoinHandle;
use imbl::OrdMap;
use imbl_value::InternedString;
use lazy_static::lazy_static;
use regex::Regex;
use rpc_toolkit::{Context, Empty, HandlerExt, ParentHandler, from_fn_async};
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
use crate::logs::{LogSource, LogsParams, journalctl};
use crate::prelude::*;
use crate::util::Invoke;
use crate::util::collections::ordmap_retain;
use crate::util::io::{ReadWriter, write_file_atomic};
use crate::util::serde::{
    BASE64, Base64, HandlerExtSerde, WithIoFormat, deserialize_from_str, display_serializable,
    serialize_display,
};
use crate::util::sync::Watch;

pub const SYSTEMD_UNIT: &str = "tor@default";
const STARTING_HEALTH_TIMEOUT: u64 = 120; // 2min

const TOR_CONTROL: SocketAddr =
    SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::new(127, 0, 1, 1), 9051));
const TOR_SOCKS: SocketAddr = SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::new(127, 0, 1, 1), 9050));

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OnionAddress(OnionAddressV3);
impl std::fmt::Display for OnionAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl FromStr for OnionAddress {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(
            s.strip_suffix(".onion")
                .unwrap_or(s)
                .rsplit(".")
                .next()
                .unwrap_or(s)
                .parse::<OnionAddressV3>()
                .with_kind(ErrorKind::Tor)?,
        ))
    }
}
impl Serialize for OnionAddress {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serialize_display(self, serializer)
    }
}
impl<'de> Deserialize<'de> for OnionAddress {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserialize_from_str(deserializer)
    }
}
impl Ord for OnionAddress {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.get_raw_bytes().cmp(&other.0.get_raw_bytes())
    }
}
impl PartialOrd for OnionAddress {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TorSecretKey(pub TorSecretKeyV3);
impl TorSecretKey {
    pub fn onion_address(&self) -> OnionAddress {
        OnionAddress(self.0.public().get_onion_address())
    }
    pub fn from_bytes(bytes: [u8; 64]) -> Result<Self, Error> {
        Ok(Self(TorSecretKeyV3::from(bytes)))
    }
    pub fn generate() -> Self {
        Self(TorSecretKeyV3::generate())
    }
    pub fn is_valid(&self) -> bool {
        let bytes = self.0.as_bytes()[..32].try_into().unwrap();
        curve25519_dalek::scalar::clamp_integer(bytes) == bytes
    }
}
impl std::fmt::Display for TorSecretKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", BASE64.encode(self.0.as_bytes()))
    }
}
impl FromStr for TorSecretKey {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_bytes(Base64::<[u8; 64]>::from_str(s)?.0)
    }
}
impl Serialize for TorSecretKey {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serialize_display(self, serializer)
    }
}
impl<'de> Deserialize<'de> for TorSecretKey {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserialize_from_str(deserializer)
    }
}

#[test]
fn test_generated_is_valid() {
    for _ in 0..100 {
        assert!(TorSecretKey::generate().is_valid());
    }
}

#[test]
fn test_tor_key() {
    // let key = crate::util::crypto::ed25519_expand_key(
    //     &hex::decode("c4b1a617bfdbcfb3f31e98c95542ce61718100e81cc6766eeebaa0dab42f0a93")
    //         .unwrap()
    //         .try_into()
    //         .unwrap(),
    // );
    let key =
        "4FpKpT4GZeEkUvH32AWMsndW+EG3XH46EmSFTh286G4AfG2U/Cc7y7L6k1dW5bl996QGDwe8gnaglq2hR2aD2w"
            .parse::<TorSecretKey>()
            .unwrap();
    assert_eq!(
        InternedString::from_display(&key.onion_address()),
        InternedString::from("ja24lucrzgcusm72r2kmiujaa2g6b5o2w4wrwt5crfrhaz2qek5ozhqd.onion")
    );
    eprintln!("{:?}", key.0.as_bytes());
    dbg!(key.to_string());
    dbg!(key.0.as_bytes()[0] & 0b111);
    dbg!(key.onion_address());
    assert!(key.is_valid());
}

#[derive(Default, Deserialize, Serialize)]
pub struct OnionStore(BTreeMap<OnionAddress, TorSecretKey>);
impl Map for OnionStore {
    type Key = OnionAddress;
    type Value = TorSecretKey;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Self::key_string(key)
    }
    fn key_string(key: &Self::Key) -> Result<imbl_value::InternedString, Error> {
        Ok(InternedString::from_display(key))
    }
}
impl OnionStore {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn insert(&mut self, key: TorSecretKey) {
        self.0.insert(key.onion_address(), key);
    }
}
impl Model<OnionStore> {
    pub fn new_key(&mut self) -> Result<TorSecretKey, Error> {
        let key = TorSecretKey::generate();
        self.insert_key(&key)?;
        Ok(key)
    }
    pub fn insert_key(&mut self, key: &TorSecretKey) -> Result<(), Error> {
        self.insert(&key.onion_address(), &key).map(|_| ())
    }
    pub fn get_key(&self, address: &OnionAddress) -> Result<TorSecretKey, Error> {
        self.as_idx(address)
            .or_not_found(lazy_format!("private key for {address}"))?
            .de()
    }
}
impl std::fmt::Debug for OnionStore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct OnionStoreMap<'a>(&'a BTreeMap<OnionAddress, TorSecretKey>);
        impl<'a> std::fmt::Debug for OnionStoreMap<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #[derive(Debug)]
                struct KeyFor(#[allow(unused)] OnionAddress);
                let mut map = f.debug_map();
                for (k, v) in self.0 {
                    map.key(k);
                    map.value(&KeyFor(v.onion_address()));
                }
                map.finish()
            }
        }
        f.debug_tuple("OnionStore")
            .field(&OnionStoreMap(&self.0))
            .finish()
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

pub fn tor_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "list-services",
            from_fn_async(list_services)
                .with_display_serializable()
                .with_custom_display_fn(|handle, result| display_services(handle.params, result))
                .with_about("Display Tor V3 Onion Addresses")
                .with_call_remote::<CliContext>(),
        )
        .subcommand("logs", logs().with_about("Display Tor logs"))
        .subcommand(
            "logs",
            from_fn_async(crate::logs::cli_logs::<RpcContext, Empty>)
                .no_display()
                .with_about("Display Tor logs"),
        )
        .subcommand(
            "reset",
            from_fn_async(reset)
                .no_display()
                .with_about("Reset Tor daemon")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "key",
            key::<C>().with_about("Manage the onion service key store"),
        )
}

pub fn key<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "generate",
            from_fn_async(generate_key)
                .with_about("Generate an onion service key and add it to the key store")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "add",
            from_fn_async(add_key)
                .with_about("Add an onion service key to the key store")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "list",
            from_fn_async(list_keys)
                .with_custom_display_fn(|_, res| {
                    for addr in res {
                        println!("{addr}");
                    }
                    Ok(())
                })
                .with_about("List onion services with keys in the key store")
                .with_call_remote::<CliContext>(),
        )
}

pub async fn generate_key(ctx: RpcContext) -> Result<OnionAddress, Error> {
    ctx.db
        .mutate(|db| {
            Ok(db
                .as_private_mut()
                .as_key_store_mut()
                .as_onion_mut()
                .new_key()?
                .onion_address())
        })
        .await
        .result
}

#[derive(Deserialize, Serialize, Parser)]
pub struct AddKeyParams {
    pub key: Base64<[u8; 64]>,
}

pub async fn add_key(
    ctx: RpcContext,
    AddKeyParams { key }: AddKeyParams,
) -> Result<OnionAddress, Error> {
    let key = TorSecretKey::from_bytes(key.0)?;
    ctx.db
        .mutate(|db| {
            db.as_private_mut()
                .as_key_store_mut()
                .as_onion_mut()
                .insert_key(&key)
        })
        .await
        .result?;
    Ok(key.onion_address())
}

pub async fn list_keys(ctx: RpcContext) -> Result<BTreeSet<OnionAddress>, Error> {
    ctx.db
        .peek()
        .await
        .into_private()
        .into_key_store()
        .into_onion()
        .keys()
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

pub fn display_services(
    params: WithIoFormat<Empty>,
    services: Vec<OnionAddress>,
) -> Result<(), Error> {
    use prettytable::*;

    if let Some(format) = params.format {
        return display_serializable(format, services);
    }

    let mut table = Table::new();
    for service in services {
        let row = row![&service.to_string()];
        table.add_row(row);
    }
    table.print_tty(false)?;
    Ok(())
}

pub async fn list_services(ctx: RpcContext, _: Empty) -> Result<Vec<OnionAddress>, Error> {
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

#[derive(Clone)]
pub struct TorController(Arc<TorControl>);
impl TorController {
    pub fn new() -> Result<Self, Error> {
        Ok(TorController(Arc::new(TorControl::new(
            TOR_CONTROL,
            TOR_SOCKS,
        ))))
    }

    pub fn service(&self, key: TorSecretKey) -> Result<TorService, Error> {
        Ok(TorService {
            services: self.0.services.clone(),
            key,
        })
    }

    pub async fn gc(&self, addr: Option<OnionAddress>) -> Result<(), Error> {
        self.0.services.send_if_modified(|services| {
            let mut changed = false;
            let mut gc = |bindings: &mut OrdMap<u16, BTreeMap<SocketAddr, Weak<()>>>| {
                ordmap_retain(bindings, |_, targets| {
                    let start_len = targets.len();
                    targets.retain(|_, rc| rc.strong_count() > 0);
                    changed |= start_len != targets.len();
                    !targets.is_empty()
                });
                if bindings.is_empty() {
                    changed = true;
                    false
                } else {
                    true
                }
            };
            if let Some(addr) = addr {
                if !if let Some((_, bindings, needs_sync)) = services.get_mut(&addr) {
                    let keep = gc(bindings);
                    if !keep {
                        *needs_sync = Some(SyncState::Remove);
                    }
                    keep
                } else {
                    true
                } {
                    services.remove(&addr);
                }
            } else {
                services.retain(|_, (_, bindings, _)| gc(bindings));
            }
            changed
        });
        Ok(())
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

    pub async fn list_services(&self) -> Result<Vec<OnionAddress>, Error> {
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
            .map(|l| l.parse::<OnionAddress>().with_kind(ErrorKind::Tor))
            .collect()
    }

    pub async fn connect_onion(
        &self,
        addr: &OnionAddress,
        port: u16,
    ) -> Result<Box<dyn ReadWriter + Unpin + Send + Sync + 'static>, Error> {
        if let Some(target) = self.0.services.peek(|s| {
            s.get(addr).and_then(|(_, bindings, _)| {
                bindings.get(&port).and_then(|b| {
                    b.iter()
                        .find(|(_, rc)| rc.strong_count() > 0)
                        .map(|(a, _)| *a)
                })
            })
        }) {
            tracing::debug!("Resolving {addr} internally to {target}");
            Ok(Box::new(
                TcpStream::connect(target)
                    .await
                    .with_kind(ErrorKind::Network)?,
            ))
        } else {
            let mut stream = TcpStream::connect(TOR_SOCKS)
                .await
                .with_kind(ErrorKind::Tor)?;
            socks5_impl::client::connect(&mut stream, (addr.to_string(), port), None)
                .await
                .with_kind(ErrorKind::Tor)?;
            Ok(Box::new(stream))
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SyncState {
    Add,
    Update,
    Remove,
}

pub struct TorService {
    services: Watch<
        BTreeMap<
            OnionAddress,
            (
                TorSecretKey,
                OrdMap<u16, BTreeMap<SocketAddr, Weak<()>>>,
                Option<SyncState>,
            ),
        >,
    >,
    key: TorSecretKey,
}

impl TorService {
    pub fn proxy_all<Rcs: FromIterator<Arc<()>>>(
        &self,
        bindings: impl IntoIterator<Item = (u16, SocketAddr)>,
    ) -> Rcs {
        self.services.send_modify(|services| {
            let (_, entry, needs_sync) = services
                .entry(self.key.onion_address())
                .or_insert_with(|| (self.key.clone(), OrdMap::new(), Some(SyncState::Add)));
            let rcs = bindings
                .into_iter()
                .map(|(external, target)| {
                    let binding = entry.entry(external).or_default();
                    let target = binding.entry(target).or_default();
                    let rc = if let Some(rc) = Weak::upgrade(&*target) {
                        rc
                    } else {
                        if needs_sync.is_none() {
                            *needs_sync = Some(SyncState::Update);
                        }
                        Arc::new(())
                    };
                    *target = Arc::downgrade(&rc);
                    rc
                })
                .collect();

            rcs
        })
    }
}

type AuthenticatedConnection = AuthenticatedConn<
    TcpStream,
    Box<dyn Fn(AsyncEvent<'static>) -> BoxFuture<'static, Result<(), ConnError>> + Send + Sync>,
>;

enum TorCommand {
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
    services: &mut Watch<
        BTreeMap<
            OnionAddress,
            (
                TorSecretKey,
                OrdMap<u16, BTreeMap<SocketAddr, Weak<()>>>,
                Option<SyncState>,
            ),
        >,
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
        write_file_atomic(
            "/etc/tor/torrc",
            format!("SocksPort {TOR_SOCKS}\nControlPort {TOR_CONTROL}\nCookieAuthentication 1\n"),
        )
        .await?;
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
        Ok(connection)
    };
    let pre_handler = async {
        while let Some(command) = recv.recv().await {
            match command {
                TorCommand::GetInfo { reply, .. } => {
                    reply
                        .send(Err(Error::new(
                            eyre!("Tor has not finished bootstrapping..."),
                            ErrorKind::Tor,
                        )))
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

    let mut connection = tokio::select! {
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

    services.send_modify(|s| {
        for (_, _, s) in s.values_mut() {
            *s = Some(SyncState::Add);
        }
    });

    let handler = async {
        loop {
            let recv = recv.recv();
            tokio::pin!(recv);
            let mut changed = services.changed().boxed();

            match futures::future::select(recv, &mut changed).await {
                futures::future::Either::Left((Some(command), _)) => match command {
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
                },
                futures::future::Either::Left((None, _)) => break,
                futures::future::Either::Right(_) => {
                    drop(changed);
                    let to_add = services.peek_and_mark_seen(|services| {
                        services
                            .iter()
                            .filter(|(_, (_, _, s))| s.is_some())
                            .map(|(k, v)| (k.clone(), (*v).clone()))
                            .collect::<BTreeMap<_, _>>()
                    });

                    for (addr, (key, bindings, state)) in &to_add {
                        if matches!(state, Some(SyncState::Update) | Some(SyncState::Remove)) {
                            connection
                                .del_onion(&addr.0.get_address_without_dot_onion())
                                .await
                                .with_kind(ErrorKind::Tor)?;
                        }
                        let bindings = bindings
                            .iter()
                            .filter_map(|(external, targets)| {
                                targets
                                    .iter()
                                    .find(|(_, rc)| rc.strong_count() > 0)
                                    .map(|(target, _)| (*external, *target))
                            })
                            .collect::<Vec<_>>();
                        if !bindings.is_empty() {
                            connection
                                .add_onion_v3(
                                    &key.0,
                                    false,
                                    false,
                                    false,
                                    None,
                                    &mut bindings.iter(),
                                )
                                .await?;
                        }
                    }
                    services.send_if_modified(|services| {
                        for (addr, (_, bindings_a, _)) in to_add {
                            if let Some((_, bindings_b, needs_sync)) = services.get_mut(&addr) {
                                if OrdMap::ptr_eq(&bindings_a, bindings_b)
                                    || bindings_a.len() == bindings_b.len()
                                        && bindings_a.iter().zip(bindings_b.iter()).all(
                                            |((a_port, a), (b_port, b))| {
                                                a_port == b_port
                                                    && a.len() == b.len()
                                                    && a.keys().zip(b.keys()).all(|(a, b)| a == b)
                                            },
                                        )
                                {
                                    *needs_sync = None;
                                } else {
                                    *needs_sync = Some(SyncState::Update);
                                }
                            }
                        }
                        false
                    });
                }
            }
        }

        Ok(())
    };
    let log_parser = async {
        loop {
            let mut logs = journalctl(
                LogSource::Unit(SYSTEMD_UNIT),
                Some(0),
                None,
                Some("0"),
                false,
                true,
            )
            .await?;
            while let Some(log) = logs.try_next().await? {
                for (regex, severity) in &*LOG_REGEXES {
                    if regex.is_match(&log.message) {
                        let (check, wipe_state) = match severity {
                            ErrorLogSeverity::Fatal { wipe_state } => (false, *wipe_state),
                            ErrorLogSeverity::Unknown { wipe_state } => (true, *wipe_state),
                        };
                        let addr = hck_key.public().get_onion_address().to_string();
                        if !check
                            || TcpStream::connect(tor_socks)
                                .map_err(|e| Error::new(e, ErrorKind::Tor))
                                .and_then(|mut tor_socks| async move {
                                    tokio::time::timeout(
                                        Duration::from_secs(30),
                                        socks5_impl::client::connect(
                                            &mut tor_socks,
                                            (addr, 80),
                                            None,
                                        )
                                        .map_err(|e| Error::new(e, ErrorKind::Tor)),
                                    )
                                    .map_err(|e| Error::new(e, ErrorKind::Tor))
                                    .await?
                                })
                                .await
                                .with_ctx(|_| (ErrorKind::Tor, "Tor is confirmed to be down"))
                                .log_err()
                                .is_some()
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
        }
    };
    let health_checker = async {
        let mut last_success = Instant::now();
        loop {
            tokio::time::sleep(Duration::from_secs(30)).await;
            let addr = hck_key.public().get_onion_address().to_string();
            if TcpStream::connect(tor_socks)
                .map_err(|e| Error::new(e, ErrorKind::Tor))
                .and_then(|mut tor_socks| async move {
                    tokio::time::timeout(
                        Duration::from_secs(30),
                        socks5_impl::client::connect(&mut tor_socks, (addr, 80), None)
                            .map_err(|e| Error::new(e, ErrorKind::Tor)),
                    )
                    .map_err(|e| Error::new(e, ErrorKind::Tor))
                    .await
                })
                .await
                .is_err()
            {
                if last_success.elapsed() > *health_timeout {
                    let err = Error::new(
                        eyre!(
                            "Tor health check failed for longer than current timeout ({health_timeout:?})"
                        ),
                        crate::ErrorKind::Tor,
                    );
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
    services: Watch<
        BTreeMap<
            OnionAddress,
            (
                TorSecretKey,
                OrdMap<u16, BTreeMap<SocketAddr, Weak<()>>>,
                Option<SyncState>,
            ),
        >,
    >,
}
impl TorControl {
    pub fn new(tor_control: SocketAddr, tor_socks: SocketAddr) -> Self {
        let (send, mut recv) = mpsc::unbounded_channel();
        let services = Watch::new(BTreeMap::new());
        let mut thread_services = services.clone();
        Self {
            _thread: tokio::spawn(async move {
                let wipe_state = AtomicBool::new(false);
                let mut health_timeout = Duration::from_secs(STARTING_HEALTH_TIMEOUT);
                loop {
                    if let Err(e) = torctl(
                        tor_control,
                        tor_socks,
                        &mut recv,
                        &mut thread_services,
                        &wipe_state,
                        &mut health_timeout,
                    )
                    .await
                    {
                        tracing::error!("TorControl : {e}");
                        tracing::debug!("{e:?}");
                    }
                    tracing::info!("Restarting Tor");
                    tokio::time::sleep(Duration::from_secs(1)).await;
                }
            })
            .into(),
            send,
            services,
        }
    }
}

#[tokio::test]
#[ignore]
async fn test_connection() {
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
