pub const DEFAULT_MARKETPLACE: &str = "https://registry.start9.com";
// pub const COMMUNITY_MARKETPLACE: &str = "https://community-registry.start9.com";
pub const BUFFER_SIZE: usize = 1024;
pub const HOST_IP: [u8; 4] = [172, 18, 0, 1];
pub const TARGET: &str = current_platform::CURRENT_PLATFORM;
lazy_static::lazy_static! {
    pub static ref ARCH: &'static str = {
        let (arch, _) = TARGET.split_once("-").unwrap();
        arch
    };
    pub static ref PLATFORM: String = {
        if let Ok(platform) = std::fs::read_to_string("/usr/lib/startos/PLATFORM.txt") {
            platform
        } else {
            ARCH.to_string()
        }
    };
    pub static ref SOURCE_DATE: SystemTime = {
        std::fs::metadata(std::env::current_exe().unwrap()).unwrap().modified().unwrap()
    };
}

pub mod account;
pub mod action;
pub mod auth;
pub mod backup;
pub mod bins;
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
pub mod firmware;
pub mod hostname;
pub mod init;
pub mod inspect;
pub mod install;
pub mod logs;
pub mod lxc;
pub mod manager;
pub mod middleware;
pub mod net;
pub mod notifications;
pub mod os_install;
pub mod prelude;
pub mod properties;
pub mod registry;
pub mod s9pk;
pub mod setup;
pub mod shutdown;
pub mod sound;
pub mod ssh;
pub mod status;
pub mod system;
pub mod update;
pub mod util;
pub mod version;
pub mod volume;

use std::time::SystemTime;

use clap::Parser;
pub use config::Config;
pub use error::{Error, ErrorKind, ResultExt};
use imbl_value::Value;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{command, from_fn, from_fn_async, from_fn_blocking, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct EchoParams {
    message: String,
}

pub fn echo(ctx: (), EchoParams { message }: EchoParams) -> Result<String, RpcError> {
    Ok(message)
}

pub fn main_api() -> ParentHandler {
    ParentHandler::new()
        .subsommand("git-info", from_fn_async(version::git_info))
        .subsommand(
            "echo",
            from_fn_async(echo).metadata("authenticated", Value::Boolean(false)),
        )
        .subsommand("inspect", inspect::inspect)
        .subsommand("server", server)
        .subsommand("package", package)
        .subsommand("net", net::net)
        .subsommand("auth", auth::auth)
        .subsommand("db", db::db)
        .subsommand("ssh", ssh::ssh)
        .subsommand("wifi", net::wifi::wifi())
        .subsommand("disk", disk::disk())
        .subsommand("notification", notifications::notification)
        .subsommand("backup", backup::backup)
        .subsommand("marketplace", registry::markeplace::marketplace())
}

pub fn server() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "time",
            from_fn_async(system::time).with_custom_display_fn(|handle, result| {
                Ok(system::display_time(handle.params, result))
            }),
        )
        .subcommand("experimental", system::experimental)
        .subcommand("logs", system::logs())
        .subcommand("kernel-logs", system::kernel_logs())
        .subcommand("metrics", system::metrics)
        .subcommand("shutdown", from_fn_async(shutdown::shutdown).no_display())
        .subcommand("restart", from_fn_async(shutdown::restart).no_display())
        .subcommand("rebuild", from_fn_async(shutdown::rebuild).no_display())
        .subcommand(
            "update",
            from_fn_async(update::update_system)
                .with_custom_display_fn(|handle, result| {
                    Ok(update::display_update_result(handle.params, result))
                })
                .metadata("sync_db", Value::Boolean(true)),
        )
        .subcommand(
            "update-firmware",
            from_fn_async(firmware::update_firmware).with_custom_display_fn(|_handle, result| {
                Ok(firmware::display_firmware_update_result(result))
            }),
        )
}

pub fn package() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "action",
            from_fn_async(action::action).with_custom_display_fn(|handle, result| {
                Ok(action::display_action_result(handle.params, result))
            }),
        )
        .subcommand(
            "install",
            from_fn_async(install::install)
                .no_display()
                .metadata("sync_db", Value::Boolean(true)),
        )
        .subcommand("sideload", from_fn_async(install::sideload).no_cli())
        .subcommand(
            "uninstall",
            from_fn_async(install::uninstall)
                .no_display()
                .metadata("sync_db", Value::Boolean(true)),
        )
        .subcommand("list", from_fn_async(install::list))
        .subcommand("config", config::config)
        .subcommand(
            "start",
            from_fn_async(control::start)
                .no_display()
                .metadata("sync_db", Value::Boolean(true)),
        )
        .subcommand(
            "stop",
            from_fn_async(control::stop)
                .no_display()
                .metadata("sync_db", Value::Boolean(true)),
        )
        .subcommand(
            "restart",
            from_fn_async(control::restart)
                .no_display()
                .metadata("sync_db", Value::Boolean(true)),
        )
        .subcommand("logs", logs::logs)
        .subcommand(
            "properties",
            from_fn_async(properties::properties).with_custom_display_fn(|handle, result| {
                Ok(properties::display_properties(result))
            }),
        )
        .subcommand("dependency", dependencies::dependency)
        .subcommand("package-backup", backup::package_backup)
}

pub fn portable_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "git-info",
            from_fn(version::git_info).metadata("authenticated", Value::Boolean(false)),
        )
        // s9pk::pack,
        .subcommand("s9pk", developer::verify())
        .subcommand("init", from_fn_blocking(developer::init).no_display())
        .subcommand("inspect", inspect::inspect())
        .subcommand(
            "publish",
            from_fn_async(registry::admin::publish).no_display(),
        )
}

pub fn diagnostic_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "git-info",
            from_fn(version::git_info).metadata("authenticated", Value::Boolean(false)),
        )
        .subcommand("echo", from_fn_async(echo))
        .subcommand("diagnostic", diagnostic::diagnostic())
}

pub fn setup_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "git-info",
            from_fn(version::git_info).metadata("authenticated", Value::Boolean(false)),
        )
        .subcommand("echo", from_fn_async(echo))
        .subcommand("setup", setup::setup())
}

pub fn install_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "git-info",
            from_fn(version::git_info).metadata("authenticated", Value::Boolean(false)),
        )
        .subcommand("echo", from_fn_async(echo))
        .subcommand("install", os_install::install())
}
