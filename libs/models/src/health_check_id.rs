use std::path::Path;

use serde::{Deserialize, Deserializer, Serialize};

use crate::Id;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct HealthCheckId<S: AsRef<str> = String>(Id<S>);
impl<S: AsRef<str>> std::fmt::Display for HealthCheckId<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl<S: AsRef<str>> AsRef<str> for HealthCheckId<S> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<'de, S> Deserialize<'de> for HealthCheckId<S>
where
    S: AsRef<str>,
    Id<S>: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(HealthCheckId(Deserialize::deserialize(deserializer)?))
    }
}
impl<S: AsRef<str>> AsRef<Path> for HealthCheckId<S> {
    fn as_ref(&self) -> &Path {
        self.0.as_ref().as_ref()
    }
}
