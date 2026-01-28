use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::net::SocketAddr;
use std::str::FromStr;
use std::sync::{Arc, Weak};
use std::time::{Duration, Instant};

use arti_client::config::onion_service::OnionServiceConfigBuilder;
use arti_client::{TorClient, TorClientConfig};
use base64::Engine;
use clap::Parser;
use color_eyre::eyre::eyre;
use futures::{FutureExt, StreamExt};
use imbl_value::InternedString;
use itertools::Itertools;
use rpc_toolkit::{Context, Empty, HandlerExt, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpStream;
use tokio::sync::Notify;
use tor_cell::relaycell::msg::Connected;
use tor_hscrypto::pk::{HsId, HsIdKeypair};
use tor_hsservice::status::State as ArtiOnionServiceState;
use tor_hsservice::{HsNickname, RunningOnionService};
use tor_keymgr::config::ArtiKeystoreKind;
use tor_proto::client::stream::IncomingStreamRequest;
use tor_rtcompat::tokio::TokioRustlsRuntime;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::prelude::*;
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::future::{NonDetachingJoinHandle, Until};
use crate::util::io::ReadWriter;
use crate::util::serde::{
    BASE64, Base64, HandlerExtSerde, WithIoFormat, deserialize_from_str, display_serializable,
    serialize_display,
};
use crate::util::sync::{SyncMutex, SyncRwLock, Watch};

const BOOTSTRAP_PROGRESS_TIMEOUT: Duration = Duration::from_secs(300);
const HS_BOOTSTRAP_TIMEOUT: Duration = Duration::from_secs(300);
const RETRY_COOLDOWN: Duration = Duration::from_secs(15);
const HEALTH_CHECK_FAILURE_ALLOWANCE: usize = 5;
const HEALTH_CHECK_COOLDOWN: Duration = Duration::from_secs(120);

#[derive(Debug, Clone, Copy)]
pub struct OnionAddress(pub HsId);
impl std::fmt::Display for OnionAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        safelog::DisplayRedacted::fmt_unredacted(&self.0, f)
    }
}
impl FromStr for OnionAddress {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(
            if s.ends_with(".onion") {
                Cow::Borrowed(s)
            } else {
                Cow::Owned(format!("{s}.onion"))
            }
            .parse::<HsId>()
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
impl PartialEq for OnionAddress {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ref() == other.0.as_ref()
    }
}
impl Eq for OnionAddress {}
impl PartialOrd for OnionAddress {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.as_ref().partial_cmp(other.0.as_ref())
    }
}
impl Ord for OnionAddress {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.as_ref().cmp(other.0.as_ref())
    }
}

pub struct TorSecretKey(pub HsIdKeypair);
impl TorSecretKey {
    pub fn onion_address(&self) -> OnionAddress {
        OnionAddress(HsId::from(self.0.as_ref().public().to_bytes()))
    }
    pub fn from_bytes(bytes: [u8; 64]) -> Result<Self, Error> {
        Ok(Self(
            tor_llcrypto::pk::ed25519::ExpandedKeypair::from_secret_key_bytes(bytes)
                .ok_or_else(|| {
                    Error::new(
                        eyre!("{}", t!("net.tor.invalid-ed25519-key")),
                        ErrorKind::Tor,
                    )
                })?
                .into(),
        ))
    }
    pub fn generate() -> Self {
        Self(
            tor_llcrypto::pk::ed25519::ExpandedKeypair::from(
                &tor_llcrypto::pk::ed25519::Keypair::generate(&mut rand::rng()),
            )
            .into(),
        )
    }
}
impl Clone for TorSecretKey {
    fn clone(&self) -> Self {
        Self(HsIdKeypair::from(
            tor_llcrypto::pk::ed25519::ExpandedKeypair::from_secret_key_bytes(
                self.0.as_ref().to_secret_key_bytes(),
            )
            .unwrap(),
        ))
    }
}
impl std::fmt::Display for TorSecretKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            BASE64.encode(self.0.as_ref().to_secret_key_bytes())
        )
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
        self.insert(&key.onion_address(), &key)?;
        Ok(key)
    }
    pub fn insert_key(&mut self, key: &TorSecretKey) -> Result<(), Error> {
        self.insert(&key.onion_address(), &key)
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

pub fn tor_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "list-services",
            from_fn_async(list_services)
                .with_display_serializable()
                .with_custom_display_fn(|handle, result| display_services(handle.params, result))
                .with_about("about.display-tor-v3-onion-addresses")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "reset",
            from_fn_async(reset)
                .no_display()
                .with_about("about.reset-tor-daemon")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "key",
            key::<C>().with_about("about.manage-onion-service-key-store"),
        )
}

pub fn key<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "generate",
            from_fn_async(generate_key)
                .with_about("about.generate-onion-service-key-add-to-store")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "add",
            from_fn_async(add_key)
                .with_about("about.add-onion-service-key-to-store")
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
                .with_about("about.list-onion-services-with-keys-in-store")
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
    #[arg(help = "help.arg.onion-secret-key")]
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
    #[arg(
        name = "wipe-state",
        short = 'w',
        long = "wipe-state",
        help = "help.arg.wipe-tor-state"
    )]
    wipe_state: bool,
}

pub async fn reset(ctx: RpcContext, ResetParams { wipe_state }: ResetParams) -> Result<(), Error> {
    ctx.net_controller.tor.reset(wipe_state).await
}

pub fn display_services(
    params: WithIoFormat<Empty>,
    services: BTreeMap<OnionAddress, OnionServiceInfo>,
) -> Result<(), Error> {
    use prettytable::*;

    if let Some(format) = params.format {
        return display_serializable(format, services);
    }

    let mut table = Table::new();
    table.add_row(row![bc => "ADDRESS", "STATE", "BINDINGS"]);
    for (service, info) in services {
        let row = row![
            &service.to_string(),
            &format!("{:?}", info.state),
            &info
                .bindings
                .into_iter()
                .map(|(port, addr)| lazy_format!("{port} -> {addr}"))
                .join("; ")
        ];
        table.add_row(row);
    }
    table.print_tty(false)?;
    Ok(())
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum OnionServiceState {
    Shutdown,
    Bootstrapping,
    DegradedReachable,
    DegradedUnreachable,
    Running,
    Recovering,
    Broken,
}
impl From<ArtiOnionServiceState> for OnionServiceState {
    fn from(value: ArtiOnionServiceState) -> Self {
        match value {
            ArtiOnionServiceState::Shutdown => Self::Shutdown,
            ArtiOnionServiceState::Bootstrapping => Self::Bootstrapping,
            ArtiOnionServiceState::DegradedReachable => Self::DegradedReachable,
            ArtiOnionServiceState::DegradedUnreachable => Self::DegradedUnreachable,
            ArtiOnionServiceState::Running => Self::Running,
            ArtiOnionServiceState::Recovering => Self::Recovering,
            ArtiOnionServiceState::Broken => Self::Broken,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct OnionServiceInfo {
    pub state: OnionServiceState,
    pub bindings: BTreeMap<u16, SocketAddr>,
}

pub async fn list_services(
    ctx: RpcContext,
    _: Empty,
) -> Result<BTreeMap<OnionAddress, OnionServiceInfo>, Error> {
    ctx.net_controller.tor.list_services().await
}

#[derive(Clone)]
pub struct TorController(Arc<TorControllerInner>);
struct TorControllerInner {
    client: Watch<(usize, TorClient<TokioRustlsRuntime>)>,
    _bootstrapper: NonDetachingJoinHandle<()>,
    services: SyncMutex<BTreeMap<OnionAddress, OnionService>>,
    reset: Arc<Notify>,
}
impl TorController {
    pub fn new() -> Result<Self, Error> {
        let mut config = TorClientConfig::builder();
        config
            .storage()
            .keystore()
            .primary()
            .kind(ArtiKeystoreKind::Ephemeral.into());
        let client = Watch::new((
            0,
            TorClient::with_runtime(TokioRustlsRuntime::current()?)
                .config(config.build().with_kind(ErrorKind::Tor)?)
                .local_resource_timeout(Duration::from_secs(0))
                .create_unbootstrapped()?,
        ));
        let reset = Arc::new(Notify::new());
        let bootstrapper_reset = reset.clone();
        let bootstrapper_client = client.clone();
        let bootstrapper = tokio::spawn(async move {
            loop {
                let (epoch, client): (usize, _) = bootstrapper_client.read();
                if let Err(e) = Until::new()
                    .with_async_fn(|| bootstrapper_reset.notified().map(Ok))
                    .run(async {
                        let mut events = client.bootstrap_events();
                        let bootstrap_fut =
                            client.bootstrap().map(|res| res.with_kind(ErrorKind::Tor));
                        let failure_fut = async {
                            let mut prev_frac = 0_f32;
                            let mut prev_inst = Instant::now();
                            while let Some(event) =
                                tokio::time::timeout(BOOTSTRAP_PROGRESS_TIMEOUT, events.next())
                                    .await
                                    .with_kind(ErrorKind::Tor)?
                            {
                                if event.ready_for_traffic() {
                                    return Ok::<_, Error>(());
                                }
                                let frac = event.as_frac();
                                if frac == prev_frac {
                                    if prev_inst.elapsed() > BOOTSTRAP_PROGRESS_TIMEOUT {
                                        return Err(Error::new(
                                            eyre!(
                                                "{}",
                                                t!(
                                                    "net.tor.bootstrap-no-progress",
                                                    duration = crate::util::serde::Duration::from(
                                                        BOOTSTRAP_PROGRESS_TIMEOUT
                                                    )
                                                    .to_string()
                                                )
                                            ),
                                            ErrorKind::Tor,
                                        ));
                                    }
                                } else {
                                    prev_frac = frac;
                                    prev_inst = Instant::now();
                                }
                            }
                            futures::future::pending().await
                        };
                        if let Err::<(), Error>(e) = tokio::select! {
                            res = bootstrap_fut => res,
                            res = failure_fut => res,
                        } {
                            tracing::error!(
                                "{}",
                                t!("net.tor.bootstrap-error", error = e.to_string())
                            );
                            tracing::debug!("{e:?}");
                        } else {
                            bootstrapper_client.send_modify(|_| ());

                            for _ in 0..HEALTH_CHECK_FAILURE_ALLOWANCE {
                                if let Err::<(), Error>(e) = async {
                                    loop {
                                        let (bg, mut runner) = BackgroundJobQueue::new();
                                        runner
                                            .run_while(async {
                                                const PING_BUF_LEN: usize = 8;
                                                let key = TorSecretKey::generate();
                                                let onion = key.onion_address();
                                                let (hs, stream) = client
                                                    .launch_onion_service_with_hsid(
                                                        OnionServiceConfigBuilder::default()
                                                            .nickname(
                                                                onion
                                                                    .to_string()
                                                                    .trim_end_matches(".onion")
                                                                    .parse::<HsNickname>()
                                                                    .with_kind(ErrorKind::Tor)?,
                                                            )
                                                            .build()
                                                            .with_kind(ErrorKind::Tor)?,
                                                        key.clone().0,
                                                    )
                                                    .with_kind(ErrorKind::Tor)?;
                                                bg.add_job(async move {
                                                    if let Err(e) = async {
                                                        let mut stream =
                                                            tor_hsservice::handle_rend_requests(
                                                                stream,
                                                            );
                                                        while let Some(req) = stream.next().await {
                                                            let mut stream = req
                                                                .accept(Connected::new_empty())
                                                                .await
                                                                .with_kind(ErrorKind::Tor)?;
                                                            let mut buf = [0; PING_BUF_LEN];
                                                            stream.read_exact(&mut buf).await?;
                                                            stream.write_all(&buf).await?;
                                                            stream.flush().await?;
                                                            stream.shutdown().await?;
                                                        }
                                                        Ok::<_, Error>(())
                                                    }
                                                    .await
                                                    {
                                                        tracing::error!(
                                                            "{}",
                                                            t!(
                                                                "net.tor.health-error",
                                                                error = e.to_string()
                                                            )
                                                        );
                                                        tracing::debug!("{e:?}");
                                                    }
                                                });

                                                tokio::time::timeout(HS_BOOTSTRAP_TIMEOUT, async {
                                                    let mut status = hs.status_events();
                                                    while let Some(status) = status.next().await {
                                                        if status.state().is_fully_reachable() {
                                                            return Ok(());
                                                        }
                                                    }
                                                    Err(Error::new(
                                                        eyre!(
                                                            "{}",
                                                            t!("net.tor.status-stream-ended")
                                                        ),
                                                        ErrorKind::Tor,
                                                    ))
                                                })
                                                .await
                                                .with_kind(ErrorKind::Tor)??;

                                                let mut stream = client
                                                    .connect((onion.to_string(), 8080))
                                                    .await?;
                                                let mut ping_buf = [0; PING_BUF_LEN];
                                                rand::fill(&mut ping_buf);
                                                stream.write_all(&ping_buf).await?;
                                                stream.flush().await?;
                                                let mut ping_res = [0; PING_BUF_LEN];
                                                stream.read_exact(&mut ping_res).await?;
                                                ensure_code!(
                                                    ping_buf == ping_res,
                                                    ErrorKind::Tor,
                                                    "ping buffer mismatch"
                                                );
                                                stream.shutdown().await?;

                                                Ok::<_, Error>(())
                                            })
                                            .await?;
                                        tokio::time::sleep(HEALTH_CHECK_COOLDOWN).await;
                                    }
                                }
                                .await
                                {
                                    tracing::error!(
                                        "{}",
                                        t!("net.tor.client-health-error", error = e.to_string())
                                    );
                                    tracing::debug!("{e:?}");
                                }
                            }
                            tracing::error!(
                                "{}",
                                t!(
                                    "net.tor.health-check-failed-recycling",
                                    count = HEALTH_CHECK_FAILURE_ALLOWANCE
                                )
                            );
                        }

                        Ok(())
                    })
                    .await
                {
                    tracing::error!(
                        "{}",
                        t!("net.tor.bootstrapper-error", error = e.to_string())
                    );
                    tracing::debug!("{e:?}");
                }
                if let Err::<(), Error>(e) = async {
                    tokio::time::sleep(RETRY_COOLDOWN).await;
                    bootstrapper_client.send((
                        epoch.wrapping_add(1),
                        TorClient::with_runtime(TokioRustlsRuntime::current()?)
                            .config(config.build().with_kind(ErrorKind::Tor)?)
                            .local_resource_timeout(Duration::from_secs(0))
                            .create_unbootstrapped_async()
                            .await?,
                    ));
                    tracing::debug!("TorClient recycled");
                    Ok(())
                }
                .await
                {
                    tracing::error!(
                        "{}",
                        t!("net.tor.client-creation-error", error = e.to_string())
                    );
                    tracing::debug!("{e:?}");
                }
            }
        })
        .into();
        Ok(Self(Arc::new(TorControllerInner {
            client,
            _bootstrapper: bootstrapper,
            services: SyncMutex::new(BTreeMap::new()),
            reset,
        })))
    }

    pub fn service(&self, key: TorSecretKey) -> Result<OnionService, Error> {
        self.0.services.mutate(|s| {
            use std::collections::btree_map::Entry;
            let addr = key.onion_address();
            match s.entry(addr) {
                Entry::Occupied(e) => Ok(e.get().clone()),
                Entry::Vacant(e) => Ok(e
                    .insert(OnionService::launch(self.0.client.clone(), key)?)
                    .clone()),
            }
        })
    }

    pub async fn gc(&self, addr: Option<OnionAddress>) -> Result<(), Error> {
        if let Some(addr) = addr {
            if let Some(s) = self.0.services.mutate(|s| {
                let rm = if let Some(s) = s.get(&addr) {
                    !s.gc()
                } else {
                    false
                };
                if rm { s.remove(&addr) } else { None }
            }) {
                s.shutdown().await
            } else {
                Ok(())
            }
        } else {
            for s in self.0.services.mutate(|s| {
                let mut rm = Vec::new();
                s.retain(|_, s| {
                    if s.gc() {
                        true
                    } else {
                        rm.push(s.clone());
                        false
                    }
                });
                rm
            }) {
                s.shutdown().await?;
            }
            Ok(())
        }
    }

    pub async fn reset(&self, wipe_state: bool) -> Result<(), Error> {
        self.0.reset.notify_waiters();
        Ok(())
    }

    pub async fn list_services(&self) -> Result<BTreeMap<OnionAddress, OnionServiceInfo>, Error> {
        Ok(self
            .0
            .services
            .peek(|s| s.iter().map(|(a, s)| (a.clone(), s.info())).collect()))
    }

    pub async fn connect_onion(
        &self,
        addr: &OnionAddress,
        port: u16,
    ) -> Result<Box<dyn ReadWriter + Unpin + Send + Sync + 'static>, Error> {
        if let Some(target) = self.0.services.peek(|s| {
            s.get(addr).and_then(|s| {
                s.0.bindings.peek(|b| {
                    b.get(&port).and_then(|b| {
                        b.iter()
                            .find(|(_, rc)| rc.strong_count() > 0)
                            .map(|(a, _)| *a)
                    })
                })
            })
        }) {
            let tcp_stream = TcpStream::connect(target)
                .await
                .with_kind(ErrorKind::Network)?;
            if let Err(e) = socket2::SockRef::from(&tcp_stream).set_keepalive(true) {
                tracing::error!(
                    "{}",
                    t!("net.tor.failed-to-set-tcp-keepalive", error = e.to_string())
                );
                tracing::debug!("{e:?}");
            }
            Ok(Box::new(tcp_stream))
        } else {
            let mut client = self.0.client.clone();
            client
                .wait_for(|(_, c)| c.bootstrap_status().ready_for_traffic())
                .await;
            let stream = client
                .read()
                .1
                .connect((addr.to_string(), port))
                .await
                .with_kind(ErrorKind::Tor)?;
            Ok(Box::new(stream))
        }
    }
}

#[derive(Clone)]
pub struct OnionService(Arc<OnionServiceData>);
struct OnionServiceData {
    service: Arc<SyncMutex<Option<Arc<RunningOnionService>>>>,
    bindings: Arc<SyncRwLock<BTreeMap<u16, BTreeMap<SocketAddr, Weak<()>>>>>,
    _thread: NonDetachingJoinHandle<()>,
}
impl OnionService {
    fn launch(
        mut client: Watch<(usize, TorClient<TokioRustlsRuntime>)>,
        key: TorSecretKey,
    ) -> Result<Self, Error> {
        let service = Arc::new(SyncMutex::new(None));
        let bindings = Arc::new(SyncRwLock::new(BTreeMap::<
            u16,
            BTreeMap<SocketAddr, Weak<()>>,
        >::new()));
        Ok(Self(Arc::new(OnionServiceData {
            service: service.clone(),
            bindings: bindings.clone(),
            _thread: tokio::spawn(async move {
                let (bg, mut runner) = BackgroundJobQueue::new();
                runner
                    .run_while(async {
                        loop {
                            if let Err(e) = async {
                                client.wait_for(|(_,c)| c.bootstrap_status().ready_for_traffic()).await;
                                let epoch = client.peek(|(e, c)| {
                                    ensure_code!(c.bootstrap_status().ready_for_traffic(), ErrorKind::Tor, "TorClient recycled");
                                    Ok::<_, Error>(*e)
                                })?;
                                let addr = key.onion_address();
                                let (new_service, stream) = client.peek(|(_, c)| {
                                    c.launch_onion_service_with_hsid(
                                        OnionServiceConfigBuilder::default()
                                            .nickname(
                                                addr
                                                    .to_string()
                                                    .trim_end_matches(".onion")
                                                    .parse::<HsNickname>()
                                                    .with_kind(ErrorKind::Tor)?,
                                            )
                                            .build()
                                            .with_kind(ErrorKind::Tor)?,
                                        key.clone().0,
                                    )
                                    .with_kind(ErrorKind::Tor)
                                })?;
                                let mut status_stream = new_service.status_events();
                                let mut status = new_service.status();
                                if status.state().is_fully_reachable() {
                                    tracing::debug!("{addr} is fully reachable");
                                } else {
                                    tracing::debug!("{addr} is not fully reachable");
                                }
                                bg.add_job(async move {
                                    while let Some(new_status) = status_stream.next().await {
                                        if status.state().is_fully_reachable() && !new_status.state().is_fully_reachable() {
                                            tracing::debug!("{addr} is no longer fully reachable");
                                        } else if !status.state().is_fully_reachable() && new_status.state().is_fully_reachable() {
                                            tracing::debug!("{addr} is now fully reachable");
                                        }
                                        status = new_status;
                                        // TODO: health daemon?
                                    }
                                });
                                service.replace(Some(new_service));
                                let mut stream = tor_hsservice::handle_rend_requests(stream);
                                while let Some(req) = tokio::select! {
                                    req = stream.next() => req,
                                    _ = client.wait_for(|(e, _)| *e != epoch) => None
                                } {
                                    bg.add_job({
                                        let bg = bg.clone();
                                        let bindings = bindings.clone();
                                        async move {
                                            if let Err(e) = async {
                                                let IncomingStreamRequest::Begin(begin) =
                                                    req.request()
                                                else {
                                                    return req
                                                        .reject(tor_cell::relaycell::msg::End::new_with_reason(
                                                            tor_cell::relaycell::msg::EndReason::DONE,
                                                        ))
                                                        .await
                                                        .with_kind(ErrorKind::Tor);
                                                };
                                                let Some(target) = bindings.peek(|b| {
                                                    b.get(&begin.port()).and_then(|a| {
                                                        a.iter()
                                                            .find(|(_, rc)| rc.strong_count() > 0)
                                                            .map(|(addr, _)| *addr)
                                                    })
                                                }) else {
                                                    return req
                                                        .reject(tor_cell::relaycell::msg::End::new_with_reason(
                                                            tor_cell::relaycell::msg::EndReason::DONE,
                                                        ))
                                                        .await
                                                        .with_kind(ErrorKind::Tor);
                                                };
                                                bg.add_job(async move {
                                                    if let Err(e) = async {
                                                        let mut outgoing =
                                                            TcpStream::connect(target)
                                                                .await
                                                                .with_kind(ErrorKind::Network)?;
                                                        if let Err(e) = socket2::SockRef::from(&outgoing).set_keepalive(true) {
                                                            tracing::error!("{}", t!("net.tor.failed-to-set-tcp-keepalive", error = e.to_string()));
                                                            tracing::debug!("{e:?}");
                                                        }
                                                        let mut incoming = req
                                                            .accept(Connected::new_empty())
                                                            .await
                                                            .with_kind(ErrorKind::Tor)?;
                                                        if let Err(e) =
                                                            tokio::io::copy_bidirectional(
                                                                &mut outgoing,
                                                                &mut incoming,
                                                            )
                                                            .await
                                                        {
                                                            tracing::trace!("Tor Stream Error: {e}");
                                                            tracing::trace!("{e:?}");
                                                        }

                                                        Ok::<_, Error>(())
                                                    }
                                                    .await
                                                    {
                                                        tracing::trace!("Tor Stream Error: {e}");
                                                        tracing::trace!("{e:?}");
                                                    }
                                                });
                                                Ok::<_, Error>(())
                                            }
                                            .await
                                            {
                                                tracing::trace!("Tor Request Error: {e}");
                                                tracing::trace!("{e:?}");
                                            }
                                        }
                                    });
                                }
                                Ok::<_, Error>(())
                            }
                            .await
                            {
                                tracing::error!("{}", t!("net.tor.client-error", error = e.to_string()));
                                tracing::debug!("{e:?}");
                            }
                        }
                    })
                    .await
            })
            .into(),
        })))
    }

    pub async fn proxy_all<Rcs: FromIterator<Arc<()>>>(
        &self,
        bindings: impl IntoIterator<Item = (u16, SocketAddr)>,
    ) -> Result<Rcs, Error> {
        Ok(self.0.bindings.mutate(|b| {
            bindings
                .into_iter()
                .map(|(port, target)| {
                    let entry = b.entry(port).or_default().entry(target).or_default();
                    if let Some(rc) = entry.upgrade() {
                        rc
                    } else {
                        let rc = Arc::new(());
                        *entry = Arc::downgrade(&rc);
                        rc
                    }
                })
                .collect()
        }))
    }

    pub fn gc(&self) -> bool {
        self.0.bindings.mutate(|b| {
            b.retain(|_, targets| {
                targets.retain(|_, rc| rc.strong_count() > 0);
                !targets.is_empty()
            });
            !b.is_empty()
        })
    }

    pub async fn shutdown(self) -> Result<(), Error> {
        self.0.service.replace(None);
        self.0._thread.abort();
        Ok(())
    }

    pub fn state(&self) -> OnionServiceState {
        self.0
            .service
            .peek(|s| s.as_ref().map(|s| s.status().state().into()))
            .unwrap_or(OnionServiceState::Bootstrapping)
    }

    pub fn info(&self) -> OnionServiceInfo {
        OnionServiceInfo {
            state: self.state(),
            bindings: self.0.bindings.peek(|b| {
                b.iter()
                    .filter_map(|(port, b)| {
                        b.iter()
                            .find(|(_, rc)| rc.strong_count() > 0)
                            .map(|(addr, _)| (*port, *addr))
                    })
                    .collect()
            }),
        }
    }
}
