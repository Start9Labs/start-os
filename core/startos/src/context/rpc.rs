use std::collections::{BTreeMap, BTreeSet};
use std::future::Future;
use std::net::{Ipv4Addr, SocketAddr, SocketAddrV4};
use std::ops::Deref;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

use chrono::{TimeDelta, Utc};
use helpers::NonDetachingJoinHandle;
use imbl::OrdMap;
use imbl_value::InternedString;
use itertools::Itertools;
use josekit::jwk::Jwk;
use models::{ActionId, PackageId};
use reqwest::{Client, Proxy};
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{CallRemote, Context, Empty};
use tokio::sync::{broadcast, watch, Mutex, RwLock};
use tokio::time::Instant;
use tracing::instrument;

use super::setup::CURRENT_SECRET;
use crate::account::AccountInfo;
use crate::auth::Sessions;
use crate::context::config::ServerConfig;
use crate::db::model::Database;
use crate::disk::OsPartitionInfo;
use crate::init::{check_time_is_synchronized, InitResult};
use crate::lxc::{ContainerId, LxcContainer, LxcManager};
use crate::net::net_controller::{NetController, NetService};
use crate::net::utils::{find_eth_iface, find_wifi_iface};
use crate::net::web_server::{UpgradableListener, WebServerAcceptorSetter};
use crate::net::wifi::WpaCli;
use crate::prelude::*;
use crate::progress::{FullProgressTracker, PhaseProgressTrackerHandle};
use crate::rpc_continuations::{Guid, OpenAuthedContinuations, RpcContinuations};
use crate::service::action::update_requested_actions;
use crate::service::effects::callbacks::ServiceCallbacks;
use crate::service::ServiceMap;
use crate::shutdown::Shutdown;
use crate::util::lshw::LshwDevice;
use crate::util::sync::{SyncMutex, Watch};

pub struct RpcContextSeed {
    is_closed: AtomicBool,
    pub os_partitions: OsPartitionInfo,
    pub wifi_interface: Option<String>,
    pub ethernet_interface: String,
    pub disk_guid: Arc<String>,
    pub ephemeral_sessions: SyncMutex<Sessions>,
    pub db: TypedPatchDb<Database>,
    pub sync_db: watch::Sender<u64>,
    pub account: RwLock<AccountInfo>,
    pub net_controller: Arc<NetController>,
    pub os_net_service: NetService,
    pub s9pk_arch: Option<&'static str>,
    pub services: ServiceMap,
    pub metrics_cache: Watch<Option<crate::system::Metrics>>,
    pub shutdown: broadcast::Sender<Option<Shutdown>>,
    pub tor_socks: SocketAddr,
    pub lxc_manager: Arc<LxcManager>,
    pub open_authed_continuations: OpenAuthedContinuations<Option<InternedString>>,
    pub rpc_continuations: RpcContinuations,
    pub callbacks: ServiceCallbacks,
    pub wifi_manager: Arc<RwLock<Option<WpaCli>>>,
    pub current_secret: Arc<Jwk>,
    pub client: Client,
    pub start_time: Instant,
    pub crons: SyncMutex<BTreeMap<Guid, NonDetachingJoinHandle<()>>>,
    // #[cfg(feature = "dev")]
    pub dev: Dev,
}

pub struct Dev {
    pub lxc: Mutex<BTreeMap<ContainerId, LxcContainer>>,
}

pub struct Hardware {
    pub devices: Vec<LshwDevice>,
    pub ram: u64,
}

pub struct InitRpcContextPhases {
    load_db: PhaseProgressTrackerHandle,
    init_net_ctrl: PhaseProgressTrackerHandle,
    cleanup_init: CleanupInitPhases,
    run_migrations: PhaseProgressTrackerHandle,
}
impl InitRpcContextPhases {
    pub fn new(handle: &FullProgressTracker) -> Self {
        Self {
            load_db: handle.add_phase("Loading database".into(), Some(5)),
            init_net_ctrl: handle.add_phase("Initializing network".into(), Some(1)),
            cleanup_init: CleanupInitPhases::new(handle),
            run_migrations: handle.add_phase("Running migrations".into(), Some(10)),
        }
    }
}

pub struct CleanupInitPhases {
    cleanup_sessions: PhaseProgressTrackerHandle,
    init_services: PhaseProgressTrackerHandle,
    check_requested_actions: PhaseProgressTrackerHandle,
}
impl CleanupInitPhases {
    pub fn new(handle: &FullProgressTracker) -> Self {
        Self {
            cleanup_sessions: handle.add_phase("Cleaning up sessions".into(), Some(1)),
            init_services: handle.add_phase("Initializing services".into(), Some(10)),
            check_requested_actions: handle.add_phase("Checking action requests".into(), Some(1)),
        }
    }
}

#[derive(Clone)]
pub struct RpcContext(Arc<RpcContextSeed>);
impl RpcContext {
    #[instrument(skip_all)]
    pub async fn init(
        webserver: &WebServerAcceptorSetter<UpgradableListener>,
        config: &ServerConfig,
        disk_guid: Arc<String>,
        init_result: Option<InitResult>,
        InitRpcContextPhases {
            mut load_db,
            mut init_net_ctrl,
            cleanup_init,
            run_migrations,
        }: InitRpcContextPhases,
    ) -> Result<Self, Error> {
        let tor_proxy = config.tor_socks.unwrap_or(SocketAddr::V4(SocketAddrV4::new(
            Ipv4Addr::new(127, 0, 0, 1),
            9050,
        )));
        let (shutdown, _) = tokio::sync::broadcast::channel(1);

        load_db.start();
        let db = if let Some(InitResult { net_ctrl, .. }) = &init_result {
            net_ctrl.db.clone()
        } else {
            TypedPatchDb::<Database>::load(config.db().await?).await?
        };
        let peek = db.peek().await;
        let account = AccountInfo::load(&peek)?;
        load_db.complete();
        tracing::info!("Opened PatchDB");

        init_net_ctrl.start();
        let (net_controller, os_net_service) = if let Some(InitResult {
            net_ctrl,
            os_net_service,
        }) = init_result
        {
            (net_ctrl, os_net_service)
        } else {
            let net_ctrl = Arc::new(
                NetController::init(
                    db.clone(),
                    config
                        .tor_control
                        .unwrap_or(SocketAddr::from(([127, 0, 0, 1], 9051))),
                    tor_proxy,
                    &account.hostname,
                )
                .await?,
            );
            webserver.try_upgrade(|a| net_ctrl.net_iface.upgrade_listener(a))?;
            let os_net_service = net_ctrl.os_bindings().await?;
            (net_ctrl, os_net_service)
        };
        init_net_ctrl.complete();
        tracing::info!("Initialized Net Controller");

        let services = ServiceMap::default();
        let metrics_cache = Watch::<Option<crate::system::Metrics>>::new(None);
        let tor_proxy_url = format!("socks5h://{tor_proxy}");

        let crons = SyncMutex::new(BTreeMap::new());

        if !db
            .peek()
            .await
            .as_public()
            .as_server_info()
            .as_ntp_synced()
            .de()?
        {
            let db = db.clone();
            crons.mutate(|c| {
                c.insert(
                    Guid::new(),
                    tokio::spawn(async move {
                        while !check_time_is_synchronized().await.unwrap() {
                            tokio::time::sleep(Duration::from_secs(30)).await;
                        }
                        db.mutate(|v| {
                            v.as_public_mut()
                                .as_server_info_mut()
                                .as_ntp_synced_mut()
                                .ser(&true)
                        })
                        .await
                        .unwrap()
                    })
                    .into(),
                )
            });
        }

        let wifi_interface = find_wifi_iface().await?;

        let seed = Arc::new(RpcContextSeed {
            is_closed: AtomicBool::new(false),
            os_partitions: config.os_partitions.clone().ok_or_else(|| {
                Error::new(
                    eyre!("OS Partition Information Missing"),
                    ErrorKind::Filesystem,
                )
            })?,
            wifi_interface: wifi_interface.clone(),
            ethernet_interface: if let Some(eth) = config.ethernet_interface.clone() {
                eth
            } else {
                find_eth_iface().await?
            },
            disk_guid,
            ephemeral_sessions: SyncMutex::new(Sessions::new()),
            sync_db: watch::Sender::new(db.sequence().await),
            db,
            account: RwLock::new(account),
            net_controller,
            os_net_service,
            s9pk_arch: if config.multi_arch_s9pks.unwrap_or(false) {
                None
            } else {
                Some(crate::ARCH)
            },
            services,
            metrics_cache,
            shutdown,
            tor_socks: tor_proxy,
            lxc_manager: Arc::new(LxcManager::new()),
            open_authed_continuations: OpenAuthedContinuations::new(),
            rpc_continuations: RpcContinuations::new(),
            callbacks: Default::default(),
            wifi_manager: Arc::new(RwLock::new(wifi_interface.clone().map(|i| WpaCli::init(i)))),
            current_secret: Arc::new(
                Jwk::generate_ec_key(josekit::jwk::alg::ec::EcCurve::P256).map_err(|e| {
                    tracing::debug!("{:?}", e);
                    tracing::error!("Couldn't generate ec key");
                    Error::new(
                        color_eyre::eyre::eyre!("Couldn't generate ec key"),
                        crate::ErrorKind::Unknown,
                    )
                })?,
            ),
            client: Client::builder()
                .proxy(Proxy::custom(move |url| {
                    if url.host_str().map_or(false, |h| h.ends_with(".onion")) {
                        Some(tor_proxy_url.clone())
                    } else {
                        None
                    }
                }))
                .build()
                .with_kind(crate::ErrorKind::ParseUrl)?,
            start_time: Instant::now(),
            crons,
            // #[cfg(feature = "dev")]
            dev: Dev {
                lxc: Mutex::new(BTreeMap::new()),
            },
        });

        let res = Self(seed.clone());
        res.cleanup_and_initialize(cleanup_init).await?;
        tracing::info!("Cleaned up transient states");

        crate::version::post_init(&res, run_migrations).await?;
        tracing::info!("Completed migrations");
        Ok(res)
    }

    #[instrument(skip_all)]
    pub async fn shutdown(self) -> Result<(), Error> {
        self.crons.mutate(|c| std::mem::take(c));
        self.services.shutdown_all().await?;
        self.is_closed.store(true, Ordering::SeqCst);
        tracing::info!("RPC Context is shutdown");
        Ok(())
    }

    pub fn add_cron<F: Future<Output = ()> + Send + 'static>(&self, fut: F) -> Guid {
        let guid = Guid::new();
        self.crons
            .mutate(|c| c.insert(guid.clone(), tokio::spawn(fut).into()));
        guid
    }

    #[instrument(skip_all)]
    pub async fn cleanup_and_initialize(
        &self,
        CleanupInitPhases {
            mut cleanup_sessions,
            init_services,
            mut check_requested_actions,
        }: CleanupInitPhases,
    ) -> Result<(), Error> {
        cleanup_sessions.start();
        self.db
            .mutate(|db| {
                if db.as_public().as_server_info().as_ntp_synced().de()? {
                    for id in db.as_private().as_sessions().keys()? {
                        if Utc::now()
                            - db.as_private()
                                .as_sessions()
                                .as_idx(&id)
                                .unwrap()
                                .de()?
                                .last_active
                            > TimeDelta::days(30)
                        {
                            db.as_private_mut().as_sessions_mut().remove(&id)?;
                        }
                    }
                }
                Ok(())
            })
            .await?;
        let db = self.db.clone();
        self.add_cron(async move {
            loop {
                tokio::time::sleep(Duration::from_secs(86400)).await;
                if let Err(e) = db
                    .mutate(|db| {
                        if db.as_public().as_server_info().as_ntp_synced().de()? {
                            for id in db.as_private().as_sessions().keys()? {
                                if Utc::now()
                                    - db.as_private()
                                        .as_sessions()
                                        .as_idx(&id)
                                        .unwrap()
                                        .de()?
                                        .last_active
                                    > TimeDelta::days(30)
                                {
                                    db.as_private_mut().as_sessions_mut().remove(&id)?;
                                }
                            }
                        }
                        Ok(())
                    })
                    .await
                {
                    tracing::error!("Error in session cleanup cron: {e}");
                    tracing::debug!("{e:?}");
                }
            }
        });
        cleanup_sessions.complete();

        self.services.init(&self, init_services).await?;
        tracing::info!("Initialized Services");

        // TODO
        check_requested_actions.start();
        let peek = self.db.peek().await;
        let mut action_input: OrdMap<PackageId, BTreeMap<ActionId, Value>> = OrdMap::new();
        let requested_actions: BTreeSet<_> = peek
            .as_public()
            .as_package_data()
            .as_entries()?
            .into_iter()
            .map(|(_, pde)| {
                Ok(pde
                    .as_requested_actions()
                    .as_entries()?
                    .into_iter()
                    .map(|(_, r)| {
                        Ok::<_, Error>((
                            r.as_request().as_package_id().de()?,
                            r.as_request().as_action_id().de()?,
                        ))
                    }))
            })
            .flatten_ok()
            .map(|a| a.and_then(|a| a))
            .try_collect()?;
        let procedure_id = Guid::new();
        for (package_id, action_id) in requested_actions {
            if let Some(service) = self.services.get(&package_id).await.as_ref() {
                if let Some(input) = service
                    .get_action_input(procedure_id.clone(), action_id.clone())
                    .await?
                    .and_then(|i| i.value)
                {
                    action_input
                        .entry(package_id)
                        .or_default()
                        .insert(action_id, input);
                }
            }
        }
        self.db
            .mutate(|db| {
                for (package_id, action_input) in &action_input {
                    for (action_id, input) in action_input {
                        for (_, pde) in db.as_public_mut().as_package_data_mut().as_entries_mut()? {
                            pde.as_requested_actions_mut().mutate(|requested_actions| {
                                Ok(update_requested_actions(
                                    requested_actions,
                                    package_id,
                                    action_id,
                                    input,
                                    false,
                                ))
                            })?;
                        }
                    }
                }
                Ok(())
            })
            .await?;
        check_requested_actions.complete();

        Ok(())
    }
    pub async fn call_remote<RemoteContext>(
        &self,
        method: &str,
        params: Value,
    ) -> Result<Value, RpcError>
    where
        Self: CallRemote<RemoteContext>,
    {
        <Self as CallRemote<RemoteContext, Empty>>::call_remote(&self, method, params, Empty {})
            .await
    }
    pub async fn call_remote_with<RemoteContext, T>(
        &self,
        method: &str,
        params: Value,
        extra: T,
    ) -> Result<Value, RpcError>
    where
        Self: CallRemote<RemoteContext, T>,
    {
        <Self as CallRemote<RemoteContext, T>>::call_remote(&self, method, params, extra).await
    }
}
impl AsRef<Jwk> for RpcContext {
    fn as_ref(&self) -> &Jwk {
        &CURRENT_SECRET
    }
}
impl AsRef<RpcContinuations> for RpcContext {
    fn as_ref(&self) -> &RpcContinuations {
        &self.rpc_continuations
    }
}
impl AsRef<OpenAuthedContinuations<Option<InternedString>>> for RpcContext {
    fn as_ref(&self) -> &OpenAuthedContinuations<Option<InternedString>> {
        &self.open_authed_continuations
    }
}
impl Context for RpcContext {}
impl Deref for RpcContext {
    type Target = RpcContextSeed;
    fn deref(&self) -> &Self::Target {
        #[cfg(feature = "unstable")]
        if self.0.is_closed.load(Ordering::SeqCst) {
            panic!(
                "RpcContext used after shutdown! {}",
                tracing_error::SpanTrace::capture()
            );
        }
        &self.0
    }
}
impl Drop for RpcContext {
    fn drop(&mut self) {
        #[cfg(feature = "unstable")]
        if self.0.is_closed.load(Ordering::SeqCst) {
            let count = Arc::strong_count(&self.0) - 1;
            tracing::info!("RpcContext dropped. {} left.", count);
            if count > 0 {
                tracing::debug!("{}", std::backtrace::Backtrace::force_capture());
                tracing::debug!("{:?}", eyre!(""))
            }
        }
    }
}
