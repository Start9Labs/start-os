use std::collections::BTreeMap;

use indexmap::IndexSet;
pub use models::InterfaceId;
use models::PackageId;
use serde::{Deserialize, Deserializer, Serialize};
use sqlx::{Executor, Postgres};
use tracing::instrument;

use crate::db::model::{InterfaceAddressMap, InterfaceAddresses};
use crate::net::keys::Key;
use crate::util::serde::Port;
use crate::{Error, ResultExt};

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Interfaces(pub BTreeMap<InterfaceId, Interface>); // TODO
impl Interfaces {
    #[instrument(skip_all)]
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
    #[instrument(skip_all)]
    pub async fn install<Ex>(
        &self,
        secrets: &mut Ex,
        package_id: &PackageId,
    ) -> Result<InterfaceAddressMap, Error>
    where
        for<'a> &'a mut Ex: Executor<'a, Database = Postgres>,
    {
        let mut interface_addresses = InterfaceAddressMap(BTreeMap::new());
        for (id, iface) in &self.0 {
            let mut addrs = InterfaceAddresses {
                tor_address: None,
                lan_address: None,
            };
            if iface.tor_config.is_some() || iface.lan_config.is_some() {
                let key =
                    Key::for_interface(secrets, Some((package_id.clone(), id.clone()))).await?;
                if iface.tor_config.is_some() {
                    addrs.tor_address = Some(key.tor_address().to_string());
                }
                if iface.lan_config.is_some() {
                    addrs.lan_address = Some(key.local_address());
                }
            }
            interface_addresses.0.insert(id.clone(), addrs);
        }
        Ok(interface_addresses)
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
    #[instrument(skip_all)]
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
