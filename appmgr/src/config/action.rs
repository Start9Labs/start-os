use anyhow::anyhow;
use indexmap::{IndexMap, IndexSet};
use nix::sys::signal::Signal;
use patch_db::HasModel;
use serde::{Deserialize, Serialize};

use super::{Config, ConfigSpec};
use crate::action::ActionImplementation;
use crate::dependencies::Dependencies;
use crate::net::host::Hosts;
use crate::s9pk::manifest::PackageId;
use crate::status::health_check::HealthCheckId;
use crate::util::Version;
use crate::volume::Volumes;
use crate::Error;

#[derive(Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct ConfigRes {
    pub config: Option<Config>,
    pub spec: ConfigSpec,
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
pub struct ConfigActions {
    pub get: ActionImplementation,
    pub set: ActionImplementation,
}
impl ConfigActions {
    pub async fn get(
        &self,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
        hosts: &Hosts,
    ) -> Result<ConfigRes, Error> {
        self.get
            .execute(pkg_id, pkg_version, volumes, hosts, None::<()>, false)
            .await
            .and_then(|res| {
                res.map_err(|e| Error::new(anyhow!("{}", e.1), crate::ErrorKind::ConfigGen))
            })
    }

    pub async fn set(
        &self,
        pkg_id: &PackageId,
        pkg_version: &Version,
        dependencies: &Dependencies,
        volumes: &Volumes,
        hosts: &Hosts,
        input: &Config,
    ) -> Result<SetResult, Error> {
        let res: SetResult = self
            .set
            .execute(pkg_id, pkg_version, volumes, hosts, Some(input), false)
            .await
            .and_then(|res| {
                res.map_err(|e| {
                    Error::new(anyhow!("{}", e.1), crate::ErrorKind::ConfigRulesViolation)
                })
            })?;
        Ok(SetResult {
            signal: res.signal,
            depends_on: res
                .depends_on
                .into_iter()
                .filter(|(pkg, _)| dependencies.0.contains_key(pkg))
                .collect(),
        })
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct SetResult {
    #[serde(deserialize_with = "crate::util::deserialize_from_str_opt")]
    #[serde(serialize_with = "crate::util::serialize_display_opt")]
    pub signal: Option<Signal>,
    pub depends_on: IndexMap<PackageId, IndexSet<HealthCheckId>>,
}
