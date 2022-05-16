pub const CONFIG_PATH: &str = "/etc/embassy/config.yaml";
#[cfg(not(feature = "beta"))]
pub const DEFAULT_MARKETPLACE: &str = "https://marketplace.start9.com";
#[cfg(feature = "beta")]
pub const DEFAULT_MARKETPLACE: &str = "https://beta-registry-0-3.start9labs.com";
pub const BUFFER_SIZE: usize = 1024;
pub const HOST_IP: [u8; 4] = [172, 18, 0, 1];

pub mod action;
pub mod auth;
pub mod backup;
pub mod config;
pub mod context;
pub mod control;
pub mod core;
pub mod db;
pub mod dependencies;
pub mod developer;
pub mod diagnostic;
pub mod disk;
pub mod error;
pub mod hostname;
pub mod id;
pub mod init;
pub mod inspect;
pub mod install;
pub mod logs;
pub mod manager;
pub mod marketplace;
pub mod middleware;
pub mod migration;
pub mod net;
pub mod notifications;
pub mod procedure;
pub mod properties;
pub mod s9pk;
pub mod setup;
pub mod shutdown;
pub mod sound;
pub mod ssh;
pub mod static_server;
pub mod status;
pub mod system;
pub mod update;
pub mod util;
pub mod version;
pub mod volume;

pub use config::Config;
pub use error::{Error, ErrorKind, ResultExt};
use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;

#[command(metadata(authenticated = false))]
pub fn echo(#[arg] message: String) -> Result<String, RpcError> {
    Ok(message)
}

#[command(subcommands(
    version::git_info,
    echo,
    inspect::inspect,
    server,
    package,
    net::net,
    auth::auth,
    db::db,
    ssh::ssh,
    net::wifi::wifi,
    disk::disk,
    notifications::notification,
    backup::backup,
    marketplace::marketplace,
))]
pub fn main_api() -> Result<(), RpcError> {
    Ok(())
}

#[command(subcommands(
    system::logs,
    system::kernel_logs,
    system::metrics,
    shutdown::shutdown,
    shutdown::restart,
    shutdown::rebuild,
    update::update_system,
))]
pub fn server() -> Result<(), RpcError> {
    Ok(())
}

#[command(subcommands(
    action::action,
    install::install,
    install::sideload,
    install::uninstall,
    install::delete_recovered,
    install::list,
    install::update::update,
    config::config,
    control::start,
    control::stop,
    logs::logs,
    properties::properties,
    dependencies::dependency,
    backup::package_backup,
))]
pub fn package() -> Result<(), RpcError> {
    Ok(())
}

#[command(subcommands(
    version::git_info,
    s9pk::pack,
    developer::verify,
    developer::init,
    inspect::inspect
))]
pub fn portable_api() -> Result<(), RpcError> {
    Ok(())
}

#[command(subcommands(version::git_info, echo, diagnostic::diagnostic))]
pub fn diagnostic_api() -> Result<(), RpcError> {
    Ok(())
}

#[command(subcommands(version::git_info, echo, setup::setup))]
pub fn setup_api() -> Result<(), RpcError> {
    Ok(())
}
