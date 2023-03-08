use std::path::Path;

use serde::{Deserialize, Deserializer, Serialize};

use crate::Id;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct HealthCheckId(Id);
impl std::fmt::Display for HealthCheckId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
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
