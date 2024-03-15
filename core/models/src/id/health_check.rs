use std::path::Path;
use std::str::FromStr;

use serde::{Deserialize, Deserializer, Serialize};

use crate::{Id, InvalidId};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, ts_rs::TS)]
pub struct HealthCheckId(Id);
impl std::fmt::Display for HealthCheckId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl FromStr for HealthCheckId {
    type Err = InvalidId;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Id::from_str(s).map(HealthCheckId)
    }
}
impl AsRef<str> for HealthCheckId {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<'de> Deserialize<'de> for HealthCheckId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(HealthCheckId(Deserialize::deserialize(deserializer)?))
    }
}
impl AsRef<Path> for HealthCheckId {
    fn as_ref(&self) -> &Path {
        self.0.as_ref().as_ref()
    }
}
