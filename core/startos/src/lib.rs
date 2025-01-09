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
        .subcommand(
            "git-info",
            from_fn(|_: C| version::git_info()).with_about("Display the githash of StartOS CLI"),
        )
        .subcommand(
            "echo",
            from_fn(echo::<RpcContext>)
                .with_metadata("authenticated", Value::Bool(false))
                .with_about("Echo a message")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "state",
            from_fn(|_: RpcContext| Ok::<_, Error>(ApiState::Running))
                .with_metadata("authenticated", Value::Bool(false))
                .with_about("Display the API that is currently serving")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "server",
            server::<C>()
                .with_about("Commands related to the server i.e. restart, update, and shutdown"),
        )
        .subcommand(
            "package",
            package::<C>().with_about("Commands related to packages"),
        )
        .subcommand(
            "net",
            net::net::<C>().with_about("Network commands related to tor and dhcp"),
        )
        .subcommand(
            "auth",
            auth::auth::<C>().with_about(
                "Commands related to Authentication i.e. login, logout, reset-password",
            ),
        )
        .subcommand(
            "db",
            db::db::<C>().with_about("Commands to interact with the db i.e. dump, put, apply"),
        )
        .subcommand(
            "ssh",
            ssh::ssh::<C>()
                .with_about("Commands for interacting with ssh keys i.e. add, delete, list"),
        )
        .subcommand(
            "wifi",
            net::wifi::wifi::<C>()
                .with_about("Commands related to wifi networks i.e. add, connect, delete"),
        )
        .subcommand(
            "disk",
            disk::disk::<C>().with_about("Commands for listing disk info and repairing"),
        )
        .subcommand(
            "notification",
            notifications::notification::<C>().with_about("Create, delete, or list notifications"),
        )
        .subcommand(
            "backup",
            backup::backup::<C>()
                .with_about("Commands related to backup creation and backup targets"),
        )
        .subcommand(
            "registry",
            CallRemoteHandler::<RpcContext, _, _, RegistryUrlParams>::new(
                registry::registry_api::<RegistryContext>(),
            )
            .no_cli(),
        )
        .subcommand(
            "s9pk",
            s9pk::rpc::s9pk().with_about("Commands for interacting with s9pk files"),
        )
        .subcommand(
            "util",
            util::rpc::util::<C>().with_about("Command for calculating the blake3 hash of a file"),
        );
    #[cfg(feature = "dev")]
    let api = api.subcommand(
        "lxc",
        lxc::dev::lxc::<C>()
            .with_about("Commands related to lxc containers i.e. create, list, remove, connect"),
    );
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
                .with_about("Display current time and server uptime")
                .with_call_remote::<CliContext>()
        )
        .subcommand(
            "experimental",
            system::experimental::<C>()
                .with_about("Commands related to configuring experimental options such as zram and cpu governor"),
        )
        .subcommand(
            "logs",
            system::logs::<RpcContext>().with_about("Display OS logs"),
        )
        .subcommand(
            "logs",
            from_fn_async(logs::cli_logs::<RpcContext, Empty>).no_display().with_about("Display OS logs"),
        )
        .subcommand(
            "kernel-logs",
            system::kernel_logs::<RpcContext>().with_about("Display Kernel logs"),
        )
        .subcommand(
            "kernel-logs",
            from_fn_async(logs::cli_logs::<RpcContext, Empty>).no_display().with_about("Display Kernel logs"),
        )
        .subcommand(
            "metrics",
            from_fn_async(system::metrics)
                .with_display_serializable()
                .with_about("Display information about the server i.e. temperature, RAM, CPU, and disk usage")
                .with_call_remote::<CliContext>()
        )
        .subcommand(
            "shutdown",
            from_fn_async(shutdown::shutdown)
                .no_display()
                .with_about("Shutdown the server")
                .with_call_remote::<CliContext>()
        )
        .subcommand(
            "restart",
            from_fn_async(shutdown::restart)
                .no_display()
                .with_about("Restart the server")
                .with_call_remote::<CliContext>()
        )
        .subcommand(
            "rebuild",
            from_fn_async(shutdown::rebuild)
                .no_display()
                .with_about("Teardown and rebuild service containers")
                .with_call_remote::<CliContext>()
        )
        .subcommand(
            "update",
            from_fn_async(update::update_system)
                .with_metadata("sync_db", Value::Bool(true))
                .no_cli(),
        )
        .subcommand(
            "update",
            from_fn_async(update::cli_update_system).no_display().with_about("Check a given registry for StartOS updates and update if available"),
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
            .with_about("Update the mainboard's firmware to the latest firmware available in this version of StartOS if available. Note: This command does not reach out to the Internet")
            .with_call_remote::<CliContext>()
        )
        .subcommand(
            "set-smtp",
            from_fn_async(system::set_system_smtp)
                .no_display()
                .with_about("Set system smtp server and credentials")
                .with_call_remote::<CliContext>()
        )
        .subcommand(
            "test-smtp", 
            from_fn_async(system::test_system_smtp)
                .no_display()
                .with_about("Send test email using system smtp server and credentials")
                .with_call_remote::<CliContext>()
        )
        .subcommand(
            "clear-smtp",
            from_fn_async(system::clear_system_smtp)
                .no_display()
                .with_about("Remove system smtp server and credentials")
                .with_call_remote::<CliContext>()
        )
}

pub fn package<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "action",
            action::action_api::<C>().with_about("Commands to get action input or run an action"),
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
        .subcommand(
            "install",
            from_fn_async(install::cli_install)
                .no_display()
                .with_about("Install a package from a marketplace or via sideloading"),
        )
        .subcommand(
            "uninstall",
            from_fn_async(install::uninstall)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("Remove a package")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "list",
            from_fn_async(install::list)
                .with_display_serializable()
                .with_about("List installed packages")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "installed-version",
            from_fn_async(install::installed_version)
                .with_display_serializable()
                .with_about("Display installed version for a PackageId")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "start",
            from_fn_async(control::start)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("Start a service")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "stop",
            from_fn_async(control::stop)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("Stop a service")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "restart",
            from_fn_async(control::restart)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("Restart a service")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "rebuild",
            from_fn_async(service::rebuild)
                .with_metadata("sync_db", Value::Bool(true))
                .no_display()
                .with_about("Rebuild service container")
                .with_call_remote::<CliContext>(),
        )
        .subcommand("logs", logs::package_logs())
        .subcommand(
            "logs",
            logs::package_logs().with_about("Display package logs"),
        )
        .subcommand(
            "logs",
            from_fn_async(logs::cli_logs::<RpcContext, logs::PackageIdParams>)
                .no_display()
                .with_about("Display package logs"),
        )
        .subcommand(
            "backup",
            backup::package_backup::<C>()
                .with_about("Commands for restoring package(s) from backup"),
        )
        .subcommand("connect", from_fn_async(service::connect_rpc).no_cli())
        .subcommand(
            "connect",
            from_fn_async(service::connect_rpc_cli)
                .no_display()
                .with_about("Connect to a LXC container"),
        )
        .subcommand(
            "attach",
            from_fn_async(service::attach)
                .with_metadata("get_session", Value::Bool(true))
                .with_about("Execute commands within a service container")
                .no_cli(),
        )
        .subcommand("attach", from_fn_async(service::cli_attach).no_display())
        .subcommand(
            "host",
            net::host::host::<C>().with_about("Manage network hosts for a package"),
        )
}

pub fn diagnostic_api() -> ParentHandler<DiagnosticContext> {
    ParentHandler::new()
        .subcommand(
            "git-info",
            from_fn(|_: DiagnosticContext| version::git_info())
                .with_metadata("authenticated", Value::Bool(false))
                .with_about("Display the githash of StartOS CLI"),
        )
        .subcommand(
            "echo",
            from_fn(echo::<DiagnosticContext>)
                .with_about("Echo a message")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "state",
            from_fn(|_: DiagnosticContext| Ok::<_, Error>(ApiState::Error))
                .with_metadata("authenticated", Value::Bool(false))
                .with_about("Display the API that is currently serving")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "diagnostic",
            diagnostic::diagnostic::<DiagnosticContext>()
                .with_about("Diagnostic commands i.e. logs, restart, rebuild"),
        )
}

pub fn init_api() -> ParentHandler<InitContext> {
    ParentHandler::new()
        .subcommand(
            "git-info",
            from_fn(|_: InitContext| version::git_info())
                .with_metadata("authenticated", Value::Bool(false))
                .with_about("Display the githash of StartOS CLI"),
        )
        .subcommand(
            "echo",
            from_fn(echo::<InitContext>)
                .with_about("Echo a message")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "state",
            from_fn(|_: InitContext| Ok::<_, Error>(ApiState::Initializing))
                .with_metadata("authenticated", Value::Bool(false))
                .with_about("Display the API that is currently serving")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "init",
            init::init_api::<InitContext>()
                .with_about("Commands to get logs or initialization progress"),
        )
}

pub fn setup_api() -> ParentHandler<SetupContext> {
    ParentHandler::new()
        .subcommand(
            "git-info",
            from_fn(|_: SetupContext| version::git_info())
                .with_metadata("authenticated", Value::Bool(false))
                .with_about("Display the githash of StartOS CLI"),
        )
        .subcommand(
            "echo",
            from_fn(echo::<SetupContext>)
                .with_about("Echo a message")
                .with_call_remote::<CliContext>(),
        )
        .subcommand("setup", setup::setup::<SetupContext>())
}

pub fn install_api() -> ParentHandler<InstallContext> {
    ParentHandler::new()
        .subcommand(
            "git-info",
            from_fn(|_: InstallContext| version::git_info())
                .with_metadata("authenticated", Value::Bool(false))
                .with_about("Display the githash of StartOS CLI"),
        )
        .subcommand(
            "echo",
            from_fn(echo::<InstallContext>)
                .with_about("Echo a message")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "install",
            os_install::install::<InstallContext>()
                .with_about("Commands to list disk info, install StartOS, and reboot"),
        )
}

pub fn expanded_api() -> ParentHandler<CliContext> {
    main_api()
        .subcommand(
            "init",
            from_fn_blocking(developer::init)
                .no_display()
                .with_about("Create developer key if it doesn't exist"),
        )
        .subcommand(
            "pubkey",
            from_fn_blocking(developer::pubkey)
                .with_about("Get public key for developer private key"),
        )
        .subcommand(
            "diagnostic",
            diagnostic::diagnostic::<CliContext>()
                .with_about("Commands to display logs, restart the server, etc"),
        )
        .subcommand("setup", setup::setup::<CliContext>())
        .subcommand(
            "install",
            os_install::install::<CliContext>()
                .with_about("Commands to list disk info, install StartOS, and reboot"),
        )
        .subcommand(
            "registry",
            registry::registry_api::<CliContext>().with_about("Commands related to the registry"),
        )
}
