use std::collections::BTreeMap;

use chrono::{DateTime, Utc};
use models::PackageId;
use reqwest::Url;
use rpc_toolkit::{from_fn_async, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};

use crate::context::CliContext;
use crate::net::interface::InterfaceId;
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
                .with_remote_cli::<CliContext>(),
        )
        .subcommand("target", target::target())
}

#[derive(Deserialize, Serialize)]
struct BackupMetadata {
    pub timestamp: DateTime<Utc>,
    #[serde(default)]
    pub network_keys: BTreeMap<InterfaceId, Base64<[u8; 32]>>,
    #[serde(default)]
    pub tor_keys: BTreeMap<InterfaceId, Base32<[u8; 64]>>, // DEPRECATED
    pub marketplace_url: Option<Url>,
}
