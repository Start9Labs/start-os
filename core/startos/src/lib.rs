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
pub mod db;
pub mod dependencies;
pub mod developer;
pub mod diagnostic;
pub mod disk;
pub mod error;
pub mod firmware;
pub mod hostname;
pub mod init;
pub mod install;
pub mod logs;
pub mod lxc;
pub mod middleware;
pub mod net;
pub mod notifications;
pub mod os_install;
pub mod prelude;
pub mod progress;
pub mod properties;
pub mod registry;
pub mod rpc_continuations;
pub mod s9pk;
pub mod service;
pub mod setup;
pub mod shutdown;
pub mod sound;
pub mod ssh;
pub mod status;
pub mod system;
pub mod update;
pub mod upload;
pub mod util;
pub mod version;
pub mod volume;

use std::time::SystemTime;

use clap::Parser;
pub use config::Config;
pub use error::{Error, ErrorKind, ResultExt};
use imbl_value::Value;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{
    command, from_fn, from_fn_async, from_fn_blocking, AnyContext, HandlerExt, ParentHandler,
};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::CliContext;
use crate::util::serde::HandlerExtSerde;

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct EchoParams {
    message: String,
}

pub fn echo(_: AnyContext, EchoParams { message }: EchoParams) -> Result<String, RpcError> {
    Ok(message)
}

pub fn main_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand("git-info", from_fn(version::git_info))
        .subcommand(
            "echo",
            from_fn(echo)
                .with_metadata("authenticated", Value::Bool(false))
                .with_call_remote::<CliContext>(),
        )
        .subcommand("init", from_fn_blocking(developer::init).no_display())
        .subcommand("server", server())
        .subcommand("package", package())
        .subcommand("net", net::net())
        .subcommand("auth", auth::auth())
        .subcommand("db", db::db())
        .subcommand("ssh", ssh::ssh())
        .subcommand("wifi", net::wifi::wifi())
        .subcommand("disk", disk::disk())
        .subcommand("notification", notifications::notification())
        .subcommand("backup", backup::backup())
        .subcommand("marketplace", registry::client::marketplace::marketplace())
        .subcommand("lxc", lxc::lxc())
        .subcommand("s9pk", s9pk::rpc::s9pk())
        .subcommand("util", util::rpc::util())
}

pub fn server() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "time",
            from_fn_async(system::time)
                .with_display_serializable()
                .with_custom_display_fn::<AnyContext, _>(|handle, result| {
                    Ok(system::display_time(handle.params, result))
                })
                .with_call_remote::<CliContext>(),
        )
        .subcommand("experimental", system::experimental())
        .subcommand("logs", system::logs())
        .subcommand("kernel-logs", system::kernel_logs())
        .subcommand(
            "metrics",
            from_fn_async(system::metrics)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "shutdown",
            from_fn_async(shutdown::shutdown)
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "restart",
            from_fn_async(shutdown::restart)
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "rebuild",
            from_fn_async(shutdown::rebuild)
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "update",
            from_fn_async(update::update_system)
                .with_metadata("sync_db", Value::Bool(true))
                .with_custom_display_fn::<AnyContext, _>(|handle, result| {
                    Ok(update::display_update_result(handle.params, result))
                })
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "update-firmware",
            from_fn_async(firmware::update_firmware)
                .with_custom_display_fn::<AnyContext, _>(|_handle, result| {
                    Ok(firmware::display_firmware_update_result(result))
                })
                .with_call_remote::<CliContext>(),
        )
}

pub fn package() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "action",
            from_fn_async(action::action)
                .with_display_serializable()
                .with_custom_display_fn::<AnyContext, _>(|handle, result| {
                    Ok(action::display_action_result(handle.params, result))
                })
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "install",
            from_fn_async(install::install)
                .with_metadata("sync_db", Value::Bool(true))
                .no_cli(),
        )
        .subcommand("sideload", from_fn_async(install::sideload).no_cli())
        .subcommand("install", from_fn_async(install::cli_install).no_display())
        .subcommand(
            "uninstall",
            from_fn_async(install::uninstall)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "list",
            from_fn_async(install::list)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand("config", config::config())
        .subcommand(
            "start",
            from_fn_async(control::start)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "stop",
            from_fn_async(control::stop)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "restart",
            from_fn_async(control::restart)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand("logs", logs::logs())
        .subcommand(
            "properties",
            from_fn_async(properties::properties)
                .with_custom_display_fn::<AnyContext, _>(|_handle, result| {
                    Ok(properties::display_properties(result))
                })
                .with_call_remote::<CliContext>(),
        )
        .subcommand("dependency", dependencies::dependency())
        .subcommand("backup", backup::package_backup())
        .subcommand("connect", from_fn_async(service::connect_rpc).no_cli())
        .subcommand(
            "connect",
            from_fn_async(service::connect_rpc_cli).no_display(),
        )
}

pub fn diagnostic_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "git-info",
            from_fn(version::git_info).with_metadata("authenticated", Value::Bool(false)),
        )
        .subcommand("echo", from_fn(echo).with_call_remote::<CliContext>())
        .subcommand("diagnostic", diagnostic::diagnostic())
}

pub fn setup_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "git-info",
            from_fn(version::git_info).with_metadata("authenticated", Value::Bool(false)),
        )
        .subcommand("echo", from_fn(echo).with_call_remote::<CliContext>())
        .subcommand("setup", setup::setup())
}

pub fn install_api() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "git-info",
            from_fn(version::git_info).with_metadata("authenticated", Value::Bool(false)),
        )
        .subcommand("echo", from_fn(echo).with_call_remote::<CliContext>())
        .subcommand("install", os_install::install())
}

pub fn expanded_api() -> ParentHandler {
    main_api()
        .subcommand("diagnostic", diagnostic::diagnostic())
        .subcommand("setup", setup::setup())
        .subcommand("install", os_install::install())
}
