use std::collections::HashMap;
use std::path::Path;

use anyhow::anyhow;
use futures::TryStreamExt;
use indexmap::{IndexMap, IndexSet};
use itertools::Either;
use serde::{Deserialize, Deserializer, Serialize};
use sqlx::{Executor, Sqlite};
use torut::onion::TorSecretKeyV3;

use crate::db::model::{InterfaceAddressMap, InterfaceAddresses};
use crate::id::Id;
use crate::s9pk::manifest::PackageId;
use crate::util::Port;
use crate::Error;

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Interfaces(pub IndexMap<InterfaceId, Interface>); // TODO
impl Interfaces {
    pub async fn install<Ex>(
        &self,
        secrets: &mut Ex,
        package_id: &PackageId,
    ) -> Result<InterfaceAddressMap, Error>
    where
        for<'a> &'a mut Ex: Executor<'a, Database = Sqlite>,
    {
        let mut interface_addresses = InterfaceAddressMap(IndexMap::new());
        for (id, iface) in &self.0 {
            let mut addrs = InterfaceAddresses {
                tor_address: None,
                lan_address: None,
            };
            if iface.tor_config.is_some() || iface.lan_config.is_some() {
                let key = TorSecretKeyV3::generate();
                let key_vec = key.as_bytes().to_vec();
                sqlx::query!(
                    "INSERT OR IGNORE INTO tor (package, interface, key) VALUES (?, ?, ?)",
                    **package_id,
                    **id,
                    key_vec,
                )
                .execute(&mut *secrets)
                .await?;
                let key_row = sqlx::query!(
                    "SELECT key FROM tor WHERE package = ? AND interface = ?",
                    **package_id,
                    **id,
                )
                .fetch_one(&mut *secrets)
                .await?;
                let mut key = [0_u8; 64];
                key.clone_from_slice(&key_row.key);
                let key = TorSecretKeyV3::from(key);
                let onion = key.public().get_onion_address();
                if iface.tor_config.is_some() {
                    addrs.tor_address = Some(onion.to_string());
                }
                if iface.lan_config.is_some() {
                    addrs.lan_address =
                        Some(format!("{}.local", onion.get_address_without_dot_onion()));
                }
            }
            interface_addresses.0.insert(id.clone(), addrs);
        }
        Ok(interface_addresses)
    }

    pub async fn tor_keys<Ex>(
        &self,
        secrets: &mut Ex,
        package_id: &PackageId,
    ) -> Result<HashMap<InterfaceId, TorSecretKeyV3>, Error>
    where
        for<'a> &'a mut Ex: Executor<'a, Database = Sqlite>,
    {
        Ok(sqlx::query!(
            "SELECT interface, key FROM tor WHERE package = ?",
            **package_id
        )
        .fetch_many(secrets)
        .map_err(Error::from)
        .try_filter_map(|qr| async move {
            Ok(if let Either::Right(r) = qr {
                let mut buf = [0; 64];
                buf.clone_from_slice(r.key.get(0..64).ok_or_else(|| {
                    Error::new(
                        anyhow!("Invalid Tor Key Length"),
                        crate::ErrorKind::Database,
                    )
                })?);
                Some((InterfaceId::from(Id::try_from(r.interface)?), buf.into()))
            } else {
                None
            })
        })
        .try_collect()
        .await?)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct InterfaceId<S: AsRef<str> = String>(Id<S>);
impl<S: AsRef<str>> From<Id<S>> for InterfaceId<S> {
    fn from(id: Id<S>) -> Self {
        Self(id)
    }
}
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
    pub name: String,
    pub description: String,
    pub tor_config: Option<TorConfig>,
    pub lan_config: Option<IndexMap<Port, LanPortConfig>>,
    pub ui: bool,
    pub protocols: IndexSet<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct TorConfig {
    pub port_mapping: IndexMap<Port, Port>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct LanPortConfig {
    pub ssl: bool,
    pub mapping: u16,
}
