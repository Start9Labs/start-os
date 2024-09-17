use std::convert::Infallible;
use std::path::Path;
use std::str::FromStr;

use serde::{Deserialize, Serialize};
use ts_rs::TS;
use yasi::InternedString;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, TS)]
#[ts(type = "string")]
pub struct ReplayId(InternedString);
impl FromStr for ReplayId {
    type Err = Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(ReplayId(InternedString::intern(s)))
    }
}
impl AsRef<ReplayId> for ReplayId {
    fn as_ref(&self) -> &ReplayId {
        self
    }
}
impl std::fmt::Display for ReplayId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl AsRef<str> for ReplayId {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl AsRef<Path> for ReplayId {
    fn as_ref(&self) -> &Path {
        self.0.as_ref()
    }
}
impl<'de> Deserialize<'de> for ReplayId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        Ok(ReplayId(serde::Deserialize::deserialize(deserializer)?))
    }
}
