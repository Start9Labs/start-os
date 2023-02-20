use std::collections::BTreeMap;
use std::net::SocketAddr;
use std::sync::{Arc, Weak};

use id_pool::IdPool;
use models::PackageId;
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use tokio::sync::Mutex;

use crate::util::Invoke;
use crate::Error;

pub const START9_BRIDGE_IFACE: &str = "br-start9";

#[derive(Debug, Deserialize, Serialize)]
pub struct LanPortForwards {
    pool: IdPool,
    allocated: BTreeMap<PackageId, BTreeMap<u16, u16>>,
}
impl LanPortForwards {
    pub fn new() -> Self {
        Self {
            pool: IdPool::new_ranged(32768..u16::MAX),
            allocated: BTreeMap::new(),
        }
    }
    pub fn alloc(&mut self, package: PackageId, port: u16) -> Option<u16> {
        if let Some(res) = self.allocated.get(&package).and_then(|a| a.get(&port)) {
            Some(*res)
        } else if let Some(res) = self.pool.request_id() {
            let mut ports = self.allocated.remove(&package).unwrap_or_default();
            ports.insert(port, res);
            self.allocated.insert(package, ports);
            Some(res)
        } else {
            None
        }
    }
    pub fn dealloc(&mut self, package: &PackageId) {
        for port in self
            .allocated
            .remove(package)
            .into_iter()
            .flat_map(|p| p.into_values())
        {
            self.pool.return_id(port).unwrap_or_default();
        }
    }
}

pub struct LpfController {
    forwards: Mutex<BTreeMap<u16, BTreeMap<SocketAddr, Weak<()>>>>,
}
impl LpfController {
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
    pub async fn gc(&self, port: u16) -> Result<(), Error> {
        let mut writable = self.forwards.lock().await;
        let (prev, forward) = if let Some(forward) = writable.remove(&port) {
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
            writable.insert(port, forward);
        }

        update_forward(port, prev, next).await
    }
}

async fn update_forward(
    port: u16,
    prev: Option<SocketAddr>,
    next: Option<SocketAddr>,
) -> Result<(), Error> {
    if prev != next {
        if let Some(prev) = prev {
            unforward(START9_BRIDGE_IFACE, port, prev).await?;
        }
        if let Some(next) = next {
            forward(START9_BRIDGE_IFACE, port, next).await?;
        }
    }
    Ok(())
}

// iptables -I FORWARD -o br-start9 -p tcp -d 172.18.0.2 --dport 8333 -j ACCEPT
// iptables -t nat -I PREROUTING -p tcp --dport 32768 -j DNAT --to 172.18.0.2:8333
async fn forward(iface: &str, port: u16, addr: SocketAddr) -> Result<(), Error> {
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
        .arg(port.to_string())
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
async fn unforward(iface: &str, port: u16, addr: SocketAddr) -> Result<(), Error> {
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
        .arg(port.to_string())
        .arg("-j")
        .arg("DNAT")
        .arg("--to")
        .arg(addr.to_string())
        .invoke(crate::ErrorKind::Network)
        .await?;
    Ok(())
}
