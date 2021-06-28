use std::net::Ipv4Addr;
use std::path::Path;

use indexmap::IndexMap;
use serde::{Deserialize, Deserializer, Serialize};
use sqlx::{Executor, Sqlite};
use torut::onion::TorSecretKeyV3;

use crate::db::model::{InterfaceAddressMap, InterfaceAddresses, InterfaceInfo};
use crate::id::Id;
use crate::s9pk::manifest::PackageId;
use crate::Error;

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Interfaces(pub IndexMap<InterfaceId, Interface>); // TODO
impl Interfaces {
    pub async fn install<Ex>(
        &self,
        secrets: &mut Ex,
        package_id: &PackageId,
        ip: Ipv4Addr,
    ) -> Result<InterfaceInfo, Error>
    where
        for<'a> &'a mut Ex: Executor<'a, Database = Sqlite>,
    {
        let mut interface_info = InterfaceInfo {
            ip,
            addresses: InterfaceAddressMap(IndexMap::new()),
        };
        for (id, iface) in &self.0 {
            let mut addrs = InterfaceAddresses {
                tor_address: None,
                lan_address: None,
            };
            if iface.tor_config.is_some() || iface.lan_config.is_some() {
                let key = TorSecretKeyV3::generate();
                let key_vec = key.as_bytes().to_vec();
                sqlx::query!(
                    "INSERT INTO tor (package, interface, key) VALUES (?, ?, ?)",
                    **package_id,
                    **id,
                    key_vec,
                )
                .execute(&mut *secrets)
                .await?;
                let onion = key.public().get_onion_address();
                if iface.tor_config.is_some() {
                    addrs.tor_address = Some(onion.to_string());
                }
                if iface.lan_config.is_some() {
                    addrs.lan_address =
                        Some(format!("{}.local", onion.get_address_without_dot_onion()));
                }
            }
            interface_info.addresses.0.insert(id.clone(), addrs);
        }
        Ok(interface_info)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct InterfaceId<S: AsRef<str> = String>(Id<S>);
impl<S: AsRef<str>> std::fmt::Display for InterfaceId<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl<S: AsRef<str>> std::ops::Deref for InterfaceId<S> {
    type Target = S;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl<S: AsRef<str>> AsRef<str> for InterfaceId<S> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<'de, S> Deserialize<'de> for InterfaceId<S>
where
    S: AsRef<str>,
    Id<S>: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(InterfaceId(Deserialize::deserialize(deserializer)?))
    }
}
impl<S: AsRef<str>> AsRef<Path> for InterfaceId<S> {
    fn as_ref(&self) -> &Path {
        self.0.as_ref().as_ref()
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Interface {
    pub tor_config: Option<TorConfig>,
    pub lan_config: Option<IndexMap<u16, LanPortConfig>>,
    pub ui: bool,
    pub protocols: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct TorConfig {
    pub port_mapping: IndexMap<u16, u16>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct LanPortConfig {
    pub ssl: bool,
    pub mapping: u16,
}
