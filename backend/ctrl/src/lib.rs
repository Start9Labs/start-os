pub mod auth;
pub mod captive;
pub mod devices;
pub mod emmc;
pub mod error;
pub mod ethernet;
pub mod exec;
pub mod files;
pub mod flash;
pub mod init;
pub mod lan;
pub mod logs;
pub mod middleware;
pub mod profiles;
pub mod setup;
pub mod system;
pub mod uci;
pub mod utils;
pub mod wan;
pub mod wifi;
pub mod embedded_web;
pub mod bins;

use std::fs::{File, OpenOptions};
use std::io::BufReader;
use std::ops::Deref;
use std::os::unix::fs::OpenOptionsExt;
use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock};

use clap::Parser;
use cookie_store::CookieStore;
use reqwest_cookie_store::CookieStoreMutex;
use tokio::runtime::Runtime;
use tracing::subscriber::DefaultGuard;

pub use error::{Error, ErrorKind};
use imbl_value::imbl::OrdMap;
use imbl_value::Value;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{
    call_remote_http, from_fn_async,
    reqwest::{Client, Url},
    CallRemote, Context, Empty, HandlerExt, ParentHandler,
};

pub trait CtrlContext: Context + Clone {
    fn uci_root(&self) -> PathBuf;
    fn effectful(&self) -> bool;
}

fn cookies_path() -> PathBuf {
    std::env::var("HOME")
        .map(|h| PathBuf::from(h).join(".startwrt").join(".cookies.json"))
        .unwrap_or_else(|_| PathBuf::from("/tmp/.startwrt-cookies.json"))
}

/// CLI arguments parsed by clap
#[derive(Clone, Parser)]
pub struct CliArgs {
    #[clap(long, default_value = "/etc/config")]
    pub config_root: PathBuf,
    #[clap(long)]
    pub configs_only: bool,
    #[clap(long, default_value = "http://127.0.0.1/rpc/v1")]
    pub host: Url,
}

/// Inner context with cookie persistence on Drop
pub struct CliContextSeed {
    pub config_root: PathBuf,
    pub configs_only: bool,
    pub host: Url,
    pub client: Client,
    pub cookie_store: Arc<CookieStoreMutex>,
    pub cookie_path: PathBuf,
    runtime: OnceLock<Arc<Runtime>>,
}

impl Drop for CliContextSeed {
    fn drop(&mut self) {
        let tmp = format!("{}.tmp", self.cookie_path.display());
        let parent_dir = self.cookie_path.parent().unwrap_or(Path::new("/"));
        if !parent_dir.exists() {
            if let Err(e) = std::fs::create_dir_all(parent_dir) {
                tracing::warn!("Failed to create cookie directory: {}", e);
                return;
            }
        }
        let file = match OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .mode(0o600)
            .open(&tmp)
        {
            Ok(f) => f,
            Err(e) => {
                tracing::warn!("Failed to create cookie temp file: {}", e);
                return;
            }
        };
        let mut writer = match fd_lock_rs::FdLock::lock(file, fd_lock_rs::LockType::Exclusive, true)
        {
            Ok(w) => w,
            Err(e) => {
                tracing::warn!("Failed to lock cookie file: {}", e);
                return;
            }
        };
        let store = self.cookie_store.lock().unwrap();
        if let Err(e) = cookie_store::serde::json::save(&store, &mut *writer) {
            tracing::warn!("Failed to save cookies: {}", e);
            return;
        }
        if let Err(e) = writer.sync_all() {
            tracing::warn!("Failed to sync cookie file: {}", e);
            return;
        }
        if let Err(e) = std::fs::rename(&tmp, &self.cookie_path) {
            tracing::warn!("Failed to rename cookie file: {}", e);
        }
    }
}

/// CLI context with session persistence
#[derive(Clone)]
pub struct CliContext(Arc<CliContextSeed>);

impl Deref for CliContext {
    type Target = CliContextSeed;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl CliContext {
    pub fn init(args: CliArgs) -> Result<Self, Error> {
        let cookie_path = cookies_path();
        let cookie_store = Arc::new(CookieStoreMutex::new(if cookie_path.exists() {
            cookie_store::serde::json::load(BufReader::new(
                File::open(&cookie_path).map_err(|e| {
                    Error::other(format!("Failed to open cookie file: {}", e))
                })?,
            ))
            .unwrap_or_default()
        } else {
            CookieStore::default()
        }));

        let client = Client::builder()
            .cookie_provider(cookie_store.clone())
            .build()
            .map_err(|e| Error::other(format!("Failed to build HTTP client: {}", e)))?;

        Ok(Self(Arc::new(CliContextSeed {
            config_root: args.config_root,
            configs_only: args.configs_only,
            host: args.host,
            client,
            cookie_store,
            cookie_path,
            runtime: OnceLock::new(),
        })))
    }

    pub fn client(&self) -> &Client {
        &self.client
    }
}

impl Context for CliContext {
    fn runtime(&self) -> Option<Arc<Runtime>> {
        Some(
            self.runtime
                .get_or_init(|| {
                    Arc::new(
                        tokio::runtime::Builder::new_current_thread()
                            .enable_all()
                            .build()
                            .unwrap(),
                    )
                })
                .clone(),
        )
    }
}

impl CtrlContext for CliContext {
    fn uci_root(&self) -> PathBuf {
        self.config_root.clone()
    }

    fn effectful(&self) -> bool {
        !self.configs_only
    }
}

impl CallRemote<ServerContext> for CliContext {
    async fn call_remote(
        &self,
        method: &str,
        _metadata: OrdMap<&'static str, Value>,
        params: Value,
        _extra: Empty,
    ) -> Result<Value, RpcError> {
        call_remote_http(self.client(), self.host.clone(), method, params).await
    }
}

#[derive(Clone)]
pub struct ServerContext;
impl Context for ServerContext {}
impl CtrlContext for ServerContext {
    fn uci_root(&self) -> PathBuf {
        "/etc/config/".into()
    }

    fn effectful(&self) -> bool {
        true
    }
}

pub fn main_api<C: CtrlContext + Clone>() -> ParentHandler<C> {
    use utils::HandlerExtSerde;

    ParentHandler::new()
        .subcommand("auth", auth::auth::<C>())
        .subcommand("profiles", profiles::profiles::<C>())
        .subcommand("ethernet", ethernet::ethernet::<C>())
        .subcommand("wifi", wifi::wifi::<C>())
        .subcommand("uci", uci::uci::<C>())
        .subcommand("file", files::file::<C>())
        .subcommand("dir", files::dir::<C>())
        .subcommand(
            "exec",
            from_fn_async(exec::exec_command)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand("setup", setup::setup::<C>())
        .subcommand("system", system::system::<C>())
        .subcommand("devices", devices::devices::<C>())
        .subcommand("wan", wan::wan::<C>())
        .subcommand("lan", lan::lan::<C>())
}

pub fn init_logging(name: &str) -> DefaultGuard {
    use tracing_rfc_5424::{
        rfc3164::Rfc3164, tracing::TrivialTracingFormatter, transport::UnixSocket,
    };
    use tracing_subscriber::Registry;
    use tracing_subscriber::{
        layer::SubscriberExt, // Needed to get `with()`
    };

    // Setup the subsriber...
    let subscriber = Registry::default().with(
        tracing_rfc_5424::layer::Layer::<
            tracing_subscriber::Registry,
            Rfc3164,
            TrivialTracingFormatter,
            UnixSocket,
        >::try_default()
        .unwrap(),
    );
    // and install it.
    tracing::subscriber::set_default(subscriber)
}
