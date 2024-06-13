use std::collections::BTreeMap;
use std::net::{Ipv4Addr, SocketAddr, SocketAddrV4};
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

use imbl_value::InternedString;
use josekit::jwk::Jwk;
use reqwest::{Client, Proxy};
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{CallRemote, Context, Empty};
use tokio::sync::{broadcast, Mutex, RwLock};
use tokio::time::Instant;
use tracing::instrument;

use super::setup::CURRENT_SECRET;
use crate::account::AccountInfo;
use crate::context::config::ServerConfig;
use crate::db::model::Database;
use crate::dependencies::compute_dependency_config_errs;
use crate::disk::OsPartitionInfo;
use crate::init::check_time_is_synchronized;
use crate::lxc::{ContainerId, LxcContainer, LxcManager};
use crate::net::net_controller::NetController;
use crate::net::utils::{find_eth_iface, find_wifi_iface};
use crate::net::wifi::WpaCli;
use crate::prelude::*;
use crate::progress::{FullProgressTracker, PhaseProgressTrackerHandle};
use crate::rpc_continuations::{OpenAuthedContinuations, RpcContinuations};
use crate::service::ServiceMap;
use crate::shutdown::Shutdown;
use crate::system::get_mem_info;
use crate::util::lshw::{lshw, LshwDevice};

pub struct RpcContextSeed {
    is_closed: AtomicBool,
    pub os_partitions: OsPartitionInfo,
    pub wifi_interface: Option<String>,
    pub ethernet_interface: String,
    pub datadir: PathBuf,
    pub disk_guid: Arc<String>,
    pub db: TypedPatchDb<Database>,
    pub account: RwLock<AccountInfo>,
    pub net_controller: Arc<NetController>,
    pub s9pk_arch: Option<&'static str>,
    pub services: ServiceMap,
    pub metrics_cache: RwLock<Option<crate::system::Metrics>>,
    pub shutdown: broadcast::Sender<Option<Shutdown>>,
    pub tor_socks: SocketAddr,
    pub lxc_manager: Arc<LxcManager>,
    pub open_authed_continuations: OpenAuthedContinuations<InternedString>,
    pub rpc_continuations: RpcContinuations,
    pub wifi_manager: Option<Arc<RwLock<WpaCli>>>,
    pub current_secret: Arc<Jwk>,
    pub client: Client,
    pub hardware: Hardware,
    pub start_time: Instant,
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
    read_device_info: PhaseProgressTrackerHandle,
    cleanup_init: CleanupInitPhases,
}
impl InitRpcContextPhases {
    pub fn new(handle: &FullProgressTracker) -> Self {
        Self {
            load_db: handle.add_phase("Loading database".into(), Some(5)),
            init_net_ctrl: handle.add_phase("Initializing network".into(), Some(1)),
            read_device_info: handle.add_phase("Reading device information".into(), Some(1)),
            cleanup_init: CleanupInitPhases::new(handle),
        }
    }
}

pub struct CleanupInitPhases {
    init_services: PhaseProgressTrackerHandle,
    check_dependencies: PhaseProgressTrackerHandle,
}
impl CleanupInitPhases {
    pub fn new(handle: &FullProgressTracker) -> Self {
        Self {
            init_services: handle.add_phase("Initializing services".into(), Some(10)),
            check_dependencies: handle.add_phase("Checking dependencies".into(), Some(1)),
        }
    }
}

#[derive(Clone)]
pub struct RpcContext(Arc<RpcContextSeed>);
impl RpcContext {
    #[instrument(skip_all)]
    pub async fn init(
        config: &ServerConfig,
        disk_guid: Arc<String>,
        InitRpcContextPhases {
            mut load_db,
            mut init_net_ctrl,
            mut read_device_info,
            cleanup_init,
        }: InitRpcContextPhases,
    ) -> Result<Self, Error> {
        let tor_proxy = config.tor_socks.unwrap_or(SocketAddr::V4(SocketAddrV4::new(
            Ipv4Addr::new(127, 0, 0, 1),
            9050,
        )));
        let (shutdown, _) = tokio::sync::broadcast::channel(1);

        load_db.start();
        let db = TypedPatchDb::<Database>::load(config.db().await?).await?;
        let peek = db.peek().await;
        let account = AccountInfo::load(&peek)?;
        load_db.complete();
        tracing::info!("Opened PatchDB");

        init_net_ctrl.start();
        let net_controller = Arc::new(
            NetController::init(
                db.clone(),
                config
                    .tor_control
                    .unwrap_or(SocketAddr::from(([127, 0, 0, 1], 9051))),
                tor_proxy,
                config
                    .dns_bind
                    .as_deref()
                    .unwrap_or(&[SocketAddr::from(([127, 0, 0, 1], 53))]),
                &account.hostname,
                account.tor_key.clone(),
            )
            .await?,
        );
        init_net_ctrl.complete();
        tracing::info!("Initialized Net Controller");

        let services = ServiceMap::default();
        let metrics_cache = RwLock::<Option<crate::system::Metrics>>::new(None);
        let tor_proxy_url = format!("socks5h://{tor_proxy}");

        read_device_info.start();
        let devices = lshw().await?;
        let ram = get_mem_info().await?.total.0 as u64 * 1024 * 1024;
        read_device_info.complete();

        if !db
            .peek()
            .await
            .as_public()
            .as_server_info()
            .as_ntp_synced()
            .de()?
        {
            let db = db.clone();
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
            });
        }

        let wifi_interface = find_wifi_iface().await?;

        let seed = Arc::new(RpcContextSeed {
            is_closed: AtomicBool::new(false),
            datadir: config.datadir().to_path_buf(),
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
            db,
            account: RwLock::new(account),
            net_controller,
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
            wifi_manager: wifi_interface
                .clone()
                .map(|i| Arc::new(RwLock::new(WpaCli::init(i)))),
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
            hardware: Hardware { devices, ram },
            start_time: Instant::now(),
            dev: Dev {
                lxc: Mutex::new(BTreeMap::new()),
            },
        });

        let res = Self(seed.clone());
        res.cleanup_and_initialize(cleanup_init).await?;
        tracing::info!("Cleaned up transient states");
        Ok(res)
    }

    #[instrument(skip_all)]
    pub async fn shutdown(self) -> Result<(), Error> {
        self.services.shutdown_all().await?;
        self.is_closed.store(true, Ordering::SeqCst);
        tracing::info!("RPC Context is shutdown");
        // TODO: shutdown http servers
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn cleanup_and_initialize(
        &self,
        CleanupInitPhases {
            init_services,
            mut check_dependencies,
        }: CleanupInitPhases,
    ) -> Result<(), Error> {
        self.services.init(&self, init_services).await?;
        tracing::info!("Initialized Package Managers");

        check_dependencies.start();
        let mut updated_current_dependents = BTreeMap::new();
        let peek = self.db.peek().await;
        for (package_id, package) in peek.as_public().as_package_data().as_entries()?.into_iter() {
            let package = package.clone();
            let mut current_dependencies = package.as_current_dependencies().de()?;
            compute_dependency_config_errs(self, &package_id, &mut current_dependencies).await?;
            updated_current_dependents.insert(package_id.clone(), current_dependencies);
        }
        self.db
            .mutate(|v| {
                for (package_id, deps) in updated_current_dependents {
                    if let Some(model) = v
                        .as_public_mut()
                        .as_package_data_mut()
                        .as_idx_mut(&package_id)
                        .map(|i| i.as_current_dependencies_mut())
                    {
                        model.ser(&deps)?;
                    }
                }
                Ok(())
            })
            .await?;
        check_dependencies.complete();

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
impl AsRef<OpenAuthedContinuations<InternedString>> for RpcContext {
    fn as_ref(&self) -> &OpenAuthedContinuations<InternedString> {
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
            tracing::info!(
                "RpcContext dropped. {} left.",
                Arc::strong_count(&self.0) - 1
            );
        }
    }
}
