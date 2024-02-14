use std::collections::BTreeMap;
use std::net::Ipv4Addr;
use std::sync::{Arc, Weak};

use color_eyre::eyre::eyre;
use tokio::process::{Child, Command};
use tokio::sync::Mutex;
use tracing::instrument;

use crate::util::Invoke;
use crate::{Error, ResultExt};

pub async fn resolve_mdns(hostname: &str) -> Result<Ipv4Addr, Error> {
    Ok(String::from_utf8(
        Command::new("avahi-resolve-host-name")
            .kill_on_drop(true)
            .arg("-4")
            .arg(hostname)
            .invoke(crate::ErrorKind::Network)
            .await?,
    )?
    .split_once("\t")
    .ok_or_else(|| {
        Error::new(
            eyre!("Failed to resolve hostname: {}", hostname),
            crate::ErrorKind::Network,
        )
    })?
    .1
    .trim()
    .parse()?)
}
