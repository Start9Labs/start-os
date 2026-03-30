pub mod activity;
pub mod auth;
pub mod backup;
pub mod captive;
pub mod continuations;
pub mod devices;
pub mod diagnostics;
pub mod dns;
pub mod emmc;
pub mod error;
pub mod ethernet;
pub mod exec;
pub mod files;
pub mod flash;
pub mod init;
pub mod lan;
pub mod logs;
pub mod luci_proxy;
pub mod middleware;
pub mod profiles;
pub mod published_ports;
pub mod setup;
pub mod ssh_keys;
pub mod ssl;
pub mod system;
pub mod uci;
pub mod utils;
pub mod verify;
pub mod wan;
pub mod vpn_client;
pub mod vpn_server;
pub mod wg;
pub mod wifi;
pub mod embedded_web;
pub mod bins;

use std::fs::{File, OpenOptions};
use std::io::BufReader;
use std::ops::Deref;
use std::os::unix::fs::OpenOptionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::{Arc, OnceLock};

use clap::Parser;
use cookie_store::CookieStore;
use reqwest_cookie_store::CookieStoreMutex;
use tokio::runtime::Runtime;

pub use error::{Error, ErrorKind};

/// Non-ambiguous character set for generated passwords.
///
/// 67 chars: A-Z minus I,O (24) + a-z minus l (25) + 2-9 (8) + !@#$%^&*=+? (10)
pub const PASSWORD_CHARS: &str =
    "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz23456789!@#$%^&*=+?";

/// Non-ambiguous alphanumeric subset (no special characters).
///
/// 57 chars: A-Z minus I,O (24) + a-z minus l (25) + 2-9 (8)
pub const PASSWORD_CHARS_ALNUM: &str =
    "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz23456789";

/// Generate a random password from the given charset using rejection sampling.
pub fn generate_password(charset: &[u8], len: usize) -> String {
    let limit = 256 - (256 % charset.len());
    let mut password = String::with_capacity(len);
    while password.len() < len {
        let mut buf = [0u8; 32];
        rand::RngCore::fill_bytes(&mut rand::rng(), &mut buf);
        for &b in &buf {
            if (b as usize) < limit {
                password.push(charset[b as usize % charset.len()] as char);
                if password.len() == len {
                    break;
                }
            }
        }
    }
    password
}
use imbl_value::imbl::OrdMap;
use imbl_value::Value;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{
    call_remote_http, from_fn_async,
    reqwest::{self, Client, Url},
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
    #[clap(long, default_value = "http://router.lan/rpc/v1")]
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

        // If the local auth cookie exists (running on the router), inject it
        // so the server's auth middleware trusts us without a session.
        if let Ok(local_token) = std::fs::read_to_string(crate::auth::LOCAL_AUTH_COOKIE_PATH) {
            let local_token = local_token.trim();
            let domain = args.host.host_str().unwrap_or("localhost");
            let cookie_value = format!("local={local_token}; Domain={domain}; Path=/; SameSite=Strict");
            if let Ok(cookie) = cookie_store::RawCookie::parse(cookie_value) {
                cookie_store.lock().unwrap().insert_raw(&cookie, &args.host).ok();
            }
        }

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

    /// Build URL for a continuation endpoint.
    pub fn rest_url(&self, guid: &str) -> Url {
        let mut url = self.host.clone();
        url.set_path(&format!("/rest/rpc/{guid}"));
        url
    }

    /// GET a continuation, returning (bytes, filename).
    pub async fn rest_download(&self, guid: &str) -> Result<(Vec<u8>, String), Error> {
        let url = self.rest_url(guid);
        let res = self.client.get(url).send().await
            .map_err(|e| Error::other(format!("Download request failed: {e}")))?;
        if !res.status().is_success() {
            return Err(Error::other(format!("Download failed: {}", res.status())));
        }
        let filename = res.headers()
            .get(reqwest::header::CONTENT_DISPOSITION)
            .and_then(|v| v.to_str().ok())
            .and_then(|v| v.split("filename=\"").nth(1))
            .and_then(|v| v.strip_suffix('"'))
            .unwrap_or("download")
            .to_string();
        let bytes = res.bytes().await
            .map_err(|e| Error::other(format!("Failed to read response: {e}")))?
            .to_vec();
        Ok((bytes, filename))
    }

    /// POST data to a continuation.
    pub async fn rest_upload(&self, guid: &str, data: Vec<u8>) -> Result<(), Error> {
        let url = self.rest_url(guid);
        let res = self.client.post(url).body(data).send().await
            .map_err(|e| Error::other(format!("Upload request failed: {e}")))?;
        if !res.status().is_success() {
            let text = res.text().await.unwrap_or_default();
            return Err(Error::other(format!("Upload failed: {text}")));
        }
        Ok(())
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
pub struct ServerContext {
    pub continuations: continuations::RpcContinuations,
    pub open_authed_continuations: continuations::OpenAuthedContinuations,
}
impl Default for ServerContext {
    fn default() -> Self {
        Self {
            continuations: continuations::RpcContinuations::new(),
            open_authed_continuations: continuations::OpenAuthedContinuations::new(),
        }
    }
}
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
        .subcommand("vpn-server", vpn_server::vpn_server::<C>())
        .subcommand("vpn-client", vpn_client::vpn_client::<C>())
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
        .subcommand("published-ports", published_ports::published_ports::<C>())
        .subcommand("ssh-keys", ssh_keys::ssh_keys::<C>())
        .subcommand("activity", activity::activity::<C>())
        .subcommand("backup", backup::backup::<C>())
        .subcommand("diagnostics", diagnostics::diagnostics::<C>())
}

/// Run a command with stdout/stderr redirected to /dev/null.
///
/// Child processes inherit the daemon's file descriptors, and procd logs
/// anything on stderr as `daemon.err`. Service reload scripts write
/// informational output to stderr (firewall rules, udhcpc status, etc.),
/// which floods the syslog with false errors. Silencing child output
/// keeps the syslog clean.
pub fn run_quiet(cmd: &mut Command) -> std::io::Result<std::process::ExitStatus> {
    cmd.stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .and_then(|mut c| c.wait())
}

pub fn init_logging(name: &str) {
    use tracing_rfc_5424::{
        rfc3164::Rfc3164, tracing::TrivialTracingFormatter, transport::UnixSocket,
    };
    use tracing_subscriber::Registry;
    use tracing_subscriber::{
        layer::SubscriberExt,
        EnvFilter,
    };

    let filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new("warn,activity=info"));

    let syslog = tracing_rfc_5424::layer::Layer::<
        tracing_subscriber::Registry,
        Rfc3164,
        TrivialTracingFormatter,
        UnixSocket,
    >::try_default()
    .unwrap();

    let subscriber = Registry::default()
        .with(syslog)
        .with(filter);
    tracing::subscriber::set_global_default(subscriber)
        .expect("failed to set global tracing subscriber");
}
