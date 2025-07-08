use std::net::Ipv4Addr;

use color_eyre::eyre::eyre;
use tokio::process::Command;

use crate::prelude::*;
use crate::util::Invoke;

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
