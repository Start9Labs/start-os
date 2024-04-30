use std::collections::BTreeMap;

use chrono::{DateTime, Utc};
use models::{HostId, PackageId};
use reqwest::Url;
use rpc_toolkit::{from_fn_async, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};

use crate::context::CliContext;
#[allow(unused_imports)]
use crate::prelude::*;
use crate::util::serde::{Base32, Base64};

pub mod backup_bulk;
pub mod os;
pub mod restore;
pub mod target;

#[derive(Debug, Deserialize, Serialize)]
pub struct BackupReport {
    server: ServerBackupReport,
    packages: BTreeMap<PackageId, PackageBackupReport>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ServerBackupReport {
    attempted: bool,
    error: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct PackageBackupReport {
    pub error: Option<String>,
}

// #[command(subcommands(backup_bulk::backup_all, target::target))]
pub fn backup() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "create",
            from_fn_async(backup_bulk::backup_all)
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand("target", target::target())
}

pub fn package_backup() -> ParentHandler {
    ParentHandler::new().subcommand(
        "restore",
        from_fn_async(restore::restore_packages_rpc)
            .no_display()
            .with_call_remote::<CliContext>(),
    )
}

#[derive(Deserialize, Serialize)]
struct BackupMetadata {
    pub timestamp: DateTime<Utc>,
    #[serde(default)]
    pub network_keys: BTreeMap<HostId, Base64<[u8; 32]>>,
    #[serde(default)]
    pub tor_keys: BTreeMap<HostId, Base32<[u8; 64]>>, // DEPRECATED
    pub marketplace_url: Option<Url>,
}
