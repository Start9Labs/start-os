use rpc_toolkit::command;
use serde::{Deserialize, Serialize};

use crate::util::Version;
use crate::Error;

#[command(subcommands(status, disk))]
pub fn setup() -> Result<(), Error> {
    Ok(())
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct StatusRes {
    is_recovering: bool,
    tor_address: Option<String>,
}

#[command(rpc_only, metadata(encrypted = true))]
pub fn status() -> Result<StatusRes, Error> {
    // TODO
    Ok(StatusRes {
        is_recovering: false,
        tor_address: None,
    })
}

#[command(subcommands(list))]
pub fn disk() -> Result<(), Error> {
    Ok(())
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct DiskInfo {
    logicalname: String,
    labels: Vec<String>,
    capacity: usize,
    used: Option<usize>,
    recovery: Option<RecoveryInfo>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct RecoveryInfo {
    version: Version,
    name: String,
}

#[command(rpc_only)]
pub fn list() -> Result<Vec<DiskInfo>, Error> {
    todo!()
}
