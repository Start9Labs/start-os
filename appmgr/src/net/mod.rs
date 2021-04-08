use std::net::Ipv4Addr;

use anyhow::anyhow;
use id_pool::IdPool;
use patch_db::HasModel;
use serde::{Deserialize, Serialize};

use self::host::Hosts;
use crate::s9pk::manifest::PackageId;
use crate::{Error, ResultExt};

pub mod host;
#[cfg(feature = "avahi")]
pub mod mdns;
pub mod tor;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct IpPool(IdPool);
impl IpPool {
    pub fn new() -> Self {
        let pool = IdPool::new();
        IpPool(pool)
    }

    pub fn get(&mut self) -> Option<Ipv4Addr> {
        let id = self.0.request_id()?;
        let ip = u32::from_be_bytes(crate::HOST_IP) + id as u32;
        Some(ip.into())
    }

    pub fn put(&mut self, ip: Ipv4Addr) {
        let ip = u32::from_be_bytes(ip.octets());
        let id = ip - u32::from_be_bytes(crate::HOST_IP);
        let _ = self.0.return_id(id as u16);
    }
}
impl Default for IpPool {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, HasModel)]
pub struct Network {
    pub ip_pool: IpPool,
    pub hosts: Hosts,
}
impl Network {
    pub fn register_host(&mut self, id: &PackageId) -> Result<Ipv4Addr, Error> {
        if let Some(exists) = self.hosts.0.get(id) {
            Ok(*exists)
        } else {
            let ip = self
                .ip_pool
                .get()
                .ok_or_else(|| anyhow!("No available IP addresses"))
                .with_kind(crate::ErrorKind::Network)?;
            self.hosts.0.insert(id.clone(), ip);
            Ok(ip)
        }
    }
}
