use std::collections::{BTreeMap, BTreeSet};

use models::PackageId;
use serde::{Deserialize, Serialize};

use super::{Config, ConfigSpec};
#[allow(unused_imports)]
use crate::prelude::*;
use crate::status::health_check::HealthCheckId;

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct ConfigRes {
    pub config: Option<Config>,
    pub spec: ConfigSpec,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct SetResult {
    pub depends_on: BTreeMap<PackageId, BTreeSet<HealthCheckId>>,
}
