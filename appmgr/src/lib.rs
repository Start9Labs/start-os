pub const CONFIG_PATH: &'static str = "/etc/embassy/config.toml";
pub const SERVICES_YAML: &'static str = "tor/services.yaml";
pub const VOLUMES: &'static str = "/root/volumes";
pub const BACKUP_MOUNT_POINT: &'static str = "/mnt/backup_drive";
pub const BACKUP_DIR: &'static str = "Embassy Backups";
pub const BUFFER_SIZE: usize = 1024;
pub const HOST_IP: [u8; 4] = [172, 18, 0, 1];

pub mod action;
pub mod auth;
pub mod backup;
pub mod config;
pub mod context;
pub mod control;
pub mod db;
pub mod dependencies;
pub mod developer;
pub mod disk;
pub mod error;
pub mod hostname;
pub mod id;
pub mod inspect;
pub mod install;
pub mod logs;
pub mod manager;
pub mod middleware;
pub mod migration;
pub mod net;
pub mod notifications;
pub mod properties;
pub mod recovery;
pub mod s9pk;
pub mod setup;
pub mod shutdown;
pub mod sound;
pub mod ssh;
pub mod status;
pub mod system;
pub mod util;
pub mod version;
pub mod volume;

pub use config::Config;
pub use error::{Error, ErrorKind, ResultExt};
use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;
pub use version::{init, self_update};

#[command(metadata(authenticated = false))]
pub fn echo(#[arg] message: String) -> Result<String, RpcError> {
    Ok(message)
}

#[command(subcommands(
    version::git_info,
    echo,
    developer::init,
    s9pk::pack,
    s9pk::verify,
    inspect::inspect,
    server,
    package,
    net::net,
    auth::auth,
    db::db,
    ssh::ssh,
    net::wifi::wifi,
    disk::disk,
))]
pub fn main_api() -> Result<(), RpcError> {
    Ok(())
}

#[command(subcommands(
    system::config,
    system::logs,
    system::metrics,
    shutdown::shutdown,
    shutdown::restart
))]
pub fn server() -> Result<(), RpcError> {
    Ok(())
}

#[command(subcommands(
    install::install,
    install::uninstall,
    config::config,
    control::start,
    control::stop,
    logs::logs,
    properties::properties,
))]
pub fn package() -> Result<(), RpcError> {
    Ok(())
}

#[command(subcommands(
    version::git_info,
    s9pk::pack,
    s9pk::verify,
    developer::init,
    inspect::inspect
))]
pub fn portable_api() -> Result<(), RpcError> {
    Ok(())
}

#[command(subcommands(version::git_info, echo, recovery::recovery))]
pub fn recovery_api() -> Result<(), RpcError> {
    Ok(())
}

#[command(subcommands(version::git_info, echo, setup::setup))]
pub fn setup_api() -> Result<(), RpcError> {
    Ok(())
}
