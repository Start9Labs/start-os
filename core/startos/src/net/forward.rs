use std::collections::BTreeMap;
use std::net::SocketAddr;
use std::sync::{Arc, Weak};

use id_pool::IdPool;
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use tokio::sync::Mutex;

use crate::prelude::*;
use crate::util::Invoke;

pub const START9_BRIDGE_IFACE: &str = "lxcbr0";
pub const FIRST_DYNAMIC_PRIVATE_PORT: u16 = 49152;

#[derive(Debug, Deserialize, Serialize)]
pub struct AvailablePorts(IdPool);
impl AvailablePorts {
    pub fn new() -> Self {
        Self(IdPool::new_ranged(FIRST_DYNAMIC_PRIVATE_PORT..u16::MAX))
    }
    pub fn alloc(&mut self) -> Result<u16, Error> {
        self.0.request_id().ok_or_else(|| {
            Error::new(
                eyre!("No more dynamic ports available!"),
                ErrorKind::Network,
            )
        })
    }
    pub fn free(&mut self, ports: impl IntoIterator<Item = u16>) {
        for port in ports {
            self.0.return_id(port).unwrap_or_default();
        }
    }
}

pub struct LanPortForwardController {
    forwards: Mutex<BTreeMap<u16, BTreeMap<SocketAddr, Weak<()>>>>,
}
impl LanPortForwardController {
    pub fn new() -> Self {
        Self {
            forwards: Mutex::new(BTreeMap::new()),
        }
    }
    pub async fn add(&self, port: u16, addr: SocketAddr) -> Result<Arc<()>, Error> {
        let mut writable = self.forwards.lock().await;
        let (prev, mut forward) = if let Some(forward) = writable.remove(&port) {
            (
                forward.keys().next().cloned(),
                forward
                    .into_iter()
                    .filter(|(_, rc)| rc.strong_count() > 0)
                    .collect(),
            )
        } else {
            (None, BTreeMap::new())
        };
        let rc = Arc::new(());
        forward.insert(addr, Arc::downgrade(&rc));
        let next = forward.keys().next().cloned();
        if !forward.is_empty() {
            writable.insert(port, forward);
        }

        update_forward(port, prev, next).await?;
        Ok(rc)
    }
    pub async fn gc(&self, external: u16) -> Result<(), Error> {
        let mut writable = self.forwards.lock().await;
        let (prev, forward) = if let Some(forward) = writable.remove(&external) {
            (
                forward.keys().next().cloned(),
                forward
                    .into_iter()
                    .filter(|(_, rc)| rc.strong_count() > 0)
                    .collect(),
            )
        } else {
            (None, BTreeMap::new())
        };
        let next = forward.keys().next().cloned();
        if !forward.is_empty() {
            writable.insert(external, forward);
        }

        update_forward(external, prev, next).await
    }
    pub fn get_forwards(&self) -> Mutex<BTreeMap<u16, BTreeMap<SocketAddr, Weak<()>>>> {
        self.forwards
    }
}

async fn update_forward(
    external: u16,
    prev: Option<SocketAddr>,
    next: Option<SocketAddr>,
) -> Result<(), Error> {
    if prev != next {
        if let Some(prev) = prev {
            unforward(START9_BRIDGE_IFACE, external, prev).await?;
        }
        if let Some(next) = next {
            forward(START9_BRIDGE_IFACE, external, next).await?;
        }
    }
    Ok(())
}

// iptables -I FORWARD -o br-start9 -p tcp -d 172.18.0.2 --dport 8333 -j ACCEPT
// iptables -t nat -I PREROUTING -p tcp --dport 32768 -j DNAT --to 172.18.0.2:8333
async fn forward(iface: &str, external: u16, addr: SocketAddr) -> Result<(), Error> {
    Command::new("iptables")
        .arg("-I")
        .arg("FORWARD")
        .arg("-o")
        .arg(iface)
        .arg("-p")
        .arg("tcp")
        .arg("-d")
        .arg(addr.ip().to_string())
        .arg("--dport")
        .arg(addr.port().to_string())
        .arg("-j")
        .arg("ACCEPT")
        .invoke(crate::ErrorKind::Network)
        .await?;
    Command::new("iptables")
        .arg("-t")
        .arg("nat")
        .arg("-I")
        .arg("PREROUTING")
        .arg("-p")
        .arg("tcp")
        .arg("--dport")
        .arg(external.to_string())
        .arg("-j")
        .arg("DNAT")
        .arg("--to")
        .arg(addr.to_string())
        .invoke(crate::ErrorKind::Network)
        .await?;
    Ok(())
}

// iptables -D FORWARD -o br-start9 -p tcp -d 172.18.0.2 --dport 8333 -j ACCEPT
// iptables -t nat -D PREROUTING -p tcp --dport 32768 -j DNAT --to 172.18.0.2:8333
async fn unforward(iface: &str, external: u16, addr: SocketAddr) -> Result<(), Error> {
    Command::new("iptables")
        .arg("-D")
        .arg("FORWARD")
        .arg("-o")
        .arg(iface)
        .arg("-p")
        .arg("tcp")
        .arg("-d")
        .arg(addr.ip().to_string())
        .arg("--dport")
        .arg(addr.port().to_string())
        .arg("-j")
        .arg("ACCEPT")
        .invoke(crate::ErrorKind::Network)
        .await?;
    Command::new("iptables")
        .arg("-t")
        .arg("nat")
        .arg("-D")
        .arg("PREROUTING")
        .arg("-p")
        .arg("tcp")
        .arg("--dport")
        .arg(external.to_string())
        .arg("-j")
        .arg("DNAT")
        .arg("--to")
        .arg(addr.to_string())
        .invoke(crate::ErrorKind::Network)
        .await?;
    Ok(())
}
