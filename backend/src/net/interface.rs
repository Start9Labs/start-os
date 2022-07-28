use std::collections::BTreeMap;

use color_eyre::eyre::eyre;
use futures::TryStreamExt;
use indexmap::IndexSet;
use itertools::Either;
pub use models::InterfaceId;
use serde::{Deserialize, Deserializer, Serialize};
use sqlx::{Executor, Sqlite};
use torut::onion::TorSecretKeyV3;
use tracing::instrument;

use crate::db::model::{InterfaceAddressMap, InterfaceAddresses};
use crate::id::Id;
use crate::s9pk::manifest::PackageId;
use crate::util::serde::Port;
use crate::{Error, ResultExt};

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Interfaces(pub BTreeMap<InterfaceId, Interface>); // TODO
impl Interfaces {
    #[instrument]
    pub fn validate(&self) -> Result<(), Error> {
        for (_, interface) in &self.0 {
            interface.validate().with_ctx(|_| {
                (
                    crate::ErrorKind::ValidateS9pk,
                    format!("Interface {}", interface.name),
                )
            })?;
        }
        Ok(())
    }
    #[instrument(skip(secrets))]
    pub async fn install<Ex>(
        &self,
        secrets: &mut Ex,
        package_id: &PackageId,
    ) -> Result<InterfaceAddressMap, Error>
    where
        for<'a> &'a mut Ex: Executor<'a, Database = Sqlite>,
    {
        let mut interface_addresses = InterfaceAddressMap(BTreeMap::new());
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

    #[instrument(skip(secrets))]
    pub async fn tor_keys<Ex>(
        &self,
        secrets: &mut Ex,
        package_id: &PackageId,
    ) -> Result<BTreeMap<InterfaceId, TorSecretKeyV3>, Error>
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
                    Error::new(eyre!("Invalid Tor Key Length"), crate::ErrorKind::Database)
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

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Interface {
    pub name: String,
    pub description: String,
    pub tor_config: Option<TorConfig>,
    pub lan_config: Option<BTreeMap<Port, LanPortConfig>>,
    pub ui: bool,
    pub protocols: IndexSet<String>,
}
impl Interface {
    #[instrument]
    pub fn validate(&self) -> Result<(), color_eyre::eyre::Report> {
        if self.tor_config.is_some() && !self.protocols.contains("tcp") {
            color_eyre::eyre::bail!("must support tcp to set up a tor hidden service");
        }
        if self.lan_config.is_some() && !self.protocols.contains("http") {
            color_eyre::eyre::bail!("must support http to set up a lan service");
        }
        if self.ui && !(self.protocols.contains("http") || self.protocols.contains("https")) {
            color_eyre::eyre::bail!("must support http or https to serve a ui");
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct TorConfig {
    pub port_mapping: BTreeMap<Port, Port>,
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct LanPortConfig {
    pub ssl: bool,
    pub internal: u16,
}
impl<'de> Deserialize<'de> for LanPortConfig {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(rename_all = "kebab-case")]
        struct PermissiveLanPortConfig {
            ssl: bool,
            internal: Option<u16>,
            mapping: Option<u16>,
        }

        let config = PermissiveLanPortConfig::deserialize(deserializer)?;
        Ok(LanPortConfig {
            ssl: config.ssl,
            internal: config
                .internal
                .or(config.mapping)
                .ok_or_else(|| serde::de::Error::missing_field("internal"))?,
        })
    }
}
