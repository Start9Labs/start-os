pub const DEFAULT_REGISTRY: &str = "https://registry.start9.com";
// pub const COMMUNITY_MARKETPLACE: &str = "https://community-registry.start9.com";
pub const HOST_IP: [u8; 4] = [172, 18, 0, 1];
pub use std::env::consts::ARCH;
lazy_static::lazy_static! {
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

mod cap {
    #![allow(non_upper_case_globals)]

    pub const CAP_1_KiB: usize = 1024;
    pub const CAP_1_MiB: usize = CAP_1_KiB * CAP_1_KiB;
    pub const CAP_10_MiB: usize = 10 * CAP_1_MiB;
}
pub use cap::*;

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
    from_fn, from_fn_async, from_fn_blocking, CallRemoteHandler, Context, Empty, HandlerExt,
    ParentHandler,
};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::{
    CliContext, DiagnosticContext, InitContext, InstallContext, RpcContext, SetupContext,
};
use crate::disk::fsck::RequiresReboot;
use crate::registry::context::{RegistryContext, RegistryUrlParams};
use crate::util::serde::HandlerExtSerde;

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
#[ts(export)]
pub struct EchoParams {
    message: String,
}

pub fn echo<C: Context>(_: C, EchoParams { message }: EchoParams) -> Result<String, RpcError> {
    Ok(message)
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum ApiState {
    Error,
    Initializing,
    Running,
}
impl std::fmt::Display for ApiState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self, f)
    }
}

pub fn main_api<C: Context>() -> ParentHandler<C> {
    let api = ParentHandler::new()
        .subcommand::<C, _>("git-info", from_fn(version::git_info))
        .subcommand(
            "echo",
            from_fn(echo::<RpcContext>)
                .with_metadata("authenticated", Value::Bool(false))
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "state",
            from_fn(|_: RpcContext| Ok::<_, Error>(ApiState::Running))
                .with_metadata("authenticated", Value::Bool(false))
                .with_call_remote::<CliContext>(),
        )
        .subcommand("server", server::<C>())
        .subcommand("package", package::<C>())
        .subcommand("net", net::net::<C>())
        .subcommand("auth", auth::auth::<C>())
        .subcommand("db", db::db::<C>())
        .subcommand("ssh", ssh::ssh::<C>())
        .subcommand("wifi", net::wifi::wifi::<C>())
        .subcommand("disk", disk::disk::<C>())
        .subcommand("notification", notifications::notification::<C>())
        .subcommand("backup", backup::backup::<C>())
        .subcommand(
            "registry",
            CallRemoteHandler::<RpcContext, _, _, RegistryUrlParams>::new(
                registry::registry_api::<RegistryContext>(),
            )
            .no_cli(),
        )
        .subcommand("s9pk", s9pk::rpc::s9pk())
        .subcommand("util", util::rpc::util::<C>());
    #[cfg(feature = "dev")]
    let api = api.subcommand("lxc", lxc::dev::lxc::<C>());
    api
}

pub fn server<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "time",
            from_fn_async(system::time)
                .with_display_serializable()
                .with_custom_display_fn(|handle, result| {
                    Ok(system::display_time(handle.params, result))
                })
                .with_call_remote::<CliContext>(),
        )
        .subcommand("experimental", system::experimental::<C>())
        .subcommand("logs", system::logs::<RpcContext>())
        .subcommand(
            "logs",
            from_fn_async(logs::cli_logs::<RpcContext, Empty>).no_display(),
        )
        .subcommand("kernel-logs", system::kernel_logs::<RpcContext>())
        .subcommand(
            "kernel-logs",
            from_fn_async(logs::cli_logs::<RpcContext, Empty>).no_display(),
        )
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
                .no_cli(),
        )
        .subcommand(
            "update",
            from_fn_async(update::cli_update_system).no_display(),
        )
        .subcommand(
            "update-firmware",
            from_fn_async(|_: RpcContext| async {
                if let Some(firmware) = firmware::check_for_firmware_update().await? {
                    firmware::update_firmware(firmware).await?;
                    Ok::<_, Error>(RequiresReboot(true))
                } else {
                    Ok(RequiresReboot(false))
                }
            })
            .with_custom_display_fn(|_handle, result| {
                Ok(firmware::display_firmware_update_result(result))
            })
            .with_call_remote::<CliContext>(),
        )
}

pub fn package<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "action",
            from_fn_async(action::action)
                .with_display_serializable()
                .with_custom_display_fn(|handle, result| {
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
        .subcommand(
            "sideload",
            from_fn_async(install::sideload)
                .with_metadata("get_session", Value::Bool(true))
                .no_cli(),
        )
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
        .subcommand(
            "installed-version",
            from_fn_async(install::installed_version)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand("config", config::config::<C>())
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
        .subcommand("logs", logs::package_logs())
        .subcommand(
            "logs",
            from_fn_async(logs::cli_logs::<RpcContext, logs::PackageIdParams>).no_display(),
        )
        .subcommand(
            "properties",
            from_fn_async(properties::properties)
                .with_custom_display_fn(|_handle, result| {
                    Ok(properties::display_properties(result))
                })
                .with_call_remote::<CliContext>(),
        )
        .subcommand("dependency", dependencies::dependency::<C>())
        .subcommand("backup", backup::package_backup::<C>())
        .subcommand("connect", from_fn_async(service::connect_rpc).no_cli())
        .subcommand(
            "connect",
            from_fn_async(service::connect_rpc_cli).no_display(),
        )
}

pub fn diagnostic_api() -> ParentHandler<DiagnosticContext> {
    ParentHandler::new()
        .subcommand::<DiagnosticContext, _>(
            "git-info",
            from_fn(version::git_info).with_metadata("authenticated", Value::Bool(false)),
        )
        .subcommand(
            "echo",
            from_fn(echo::<DiagnosticContext>).with_call_remote::<CliContext>(),
        )
        .subcommand(
            "state",
            from_fn(|_: DiagnosticContext| Ok::<_, Error>(ApiState::Error))
                .with_metadata("authenticated", Value::Bool(false))
                .with_call_remote::<CliContext>(),
        )
        .subcommand("diagnostic", diagnostic::diagnostic::<DiagnosticContext>())
}

pub fn init_api() -> ParentHandler<InitContext> {
    ParentHandler::new()
        .subcommand::<InitContext, _>(
            "git-info",
            from_fn(version::git_info).with_metadata("authenticated", Value::Bool(false)),
        )
        .subcommand(
            "echo",
            from_fn(echo::<InitContext>).with_call_remote::<CliContext>(),
        )
        .subcommand(
            "state",
            from_fn(|_: InitContext| Ok::<_, Error>(ApiState::Initializing))
                .with_metadata("authenticated", Value::Bool(false))
                .with_call_remote::<CliContext>(),
        )
        .subcommand("init", init::init_api::<InitContext>())
}

pub fn setup_api() -> ParentHandler<SetupContext> {
    ParentHandler::new()
        .subcommand::<SetupContext, _>(
            "git-info",
            from_fn(version::git_info).with_metadata("authenticated", Value::Bool(false)),
        )
        .subcommand(
            "echo",
            from_fn(echo::<SetupContext>).with_call_remote::<CliContext>(),
        )
        .subcommand("setup", setup::setup::<SetupContext>())
}

pub fn install_api() -> ParentHandler<InstallContext> {
    ParentHandler::new()
        .subcommand::<InstallContext, _>(
            "git-info",
            from_fn(version::git_info).with_metadata("authenticated", Value::Bool(false)),
        )
        .subcommand(
            "echo",
            from_fn(echo::<InstallContext>).with_call_remote::<CliContext>(),
        )
        .subcommand("install", os_install::install::<InstallContext>())
}

pub fn expanded_api() -> ParentHandler<CliContext> {
    main_api()
        .subcommand("init", from_fn_blocking(developer::init).no_display())
        .subcommand("pubkey", from_fn_blocking(developer::pubkey))
        .subcommand("diagnostic", diagnostic::diagnostic::<CliContext>())
        .subcommand("setup", setup::setup::<CliContext>())
        .subcommand("install", os_install::install::<CliContext>())
        .subcommand("registry", registry::registry_api::<CliContext>())
}
