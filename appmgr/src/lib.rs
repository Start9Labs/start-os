pub const CONFIG_PATH: &'static str = "/etc/embassy/config.toml";
pub const TOR_RC: &'static str = "/root/appmgr/tor/torrc";
pub const SERVICES_YAML: &'static str = "tor/services.yaml";
pub const VOLUMES: &'static str = "/root/volumes";
pub const PERSISTENCE_DIR: &'static str = "/root/appmgr";
pub const TMP_DIR: &'static str = "/root/tmp/appmgr";
pub const BACKUP_MOUNT_POINT: &'static str = "/mnt/backup_drive";
pub const BACKUP_DIR: &'static str = "Embassy Backups";
pub const BUFFER_SIZE: usize = 1024;
pub const HOST_IP: [u8; 4] = [172, 18, 0, 1];

lazy_static::lazy_static! {
    pub static ref REGISTRY_URL: String = std::env::var("REGISTRY_URL").unwrap_or_else(|_| "https://registry.start9labs.com".to_owned());
    pub static ref SYS_REGISTRY_URL: String = format!("{}/sys", *REGISTRY_URL);
    pub static ref APP_REGISTRY_URL: String = format!("{}/apps", *REGISTRY_URL);
    pub static ref QUIET: tokio::sync::RwLock<bool> = tokio::sync::RwLock::new(!std::env::var("APPMGR_QUIET").map(|a| a == "0").unwrap_or(true));
}

pub mod action;
pub mod auth;
pub mod backup;
pub mod config;
pub mod context;
pub mod control;
pub mod daemon;
pub mod db;
pub mod dependencies;
pub mod developer;
pub mod error;
pub mod hostname;
pub mod id;
pub mod inspect;
pub mod install;
pub mod manager;
pub mod middleware;
pub mod migration;
pub mod net;
pub mod registry;
pub mod s9pk;
pub mod sound;
pub mod ssh;
pub mod status;
pub mod system;
pub mod util;
pub mod version;
pub mod volume;

pub use config::Config;
use context::{CliContext, EitherContext};
pub use error::{Error, ErrorKind, ResultExt};
use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;
pub use version::{init, self_update};

#[command(metadata(authenticated = false))]
pub fn echo(#[context] _ctx: EitherContext, #[arg] message: String) -> Result<String, RpcError> {
    Ok(message)
}

#[command(subcommands(
    version::git_info,
    echo,
    s9pk::pack,
    s9pk::verify,
    developer::init,
    inspect::inspect,
    package,
    auth::auth,
    db::db,
))]
pub fn main_api(#[context] ctx: EitherContext) -> Result<EitherContext, RpcError> {
    Ok(ctx)
}

#[command(subcommands(
    install::install,
    install::uninstall,
    config::config,
    control::start,
    control::stop
))]
pub fn package(#[context] ctx: EitherContext) -> Result<EitherContext, RpcError> {
    Ok(ctx)
}

#[command(subcommands(
    version::git_info,
    s9pk::pack,
    s9pk::verify,
    developer::init,
    inspect::inspect
))]
pub fn portable_api(#[context] ctx: EitherContext) -> Result<EitherContext, RpcError> {
    Ok(ctx)
}
