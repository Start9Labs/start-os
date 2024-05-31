use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::str::FromStr;

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use ts_rs::TS;

#[derive(Debug, Clone, TS)]
#[ts(type = "string", rename = "Version")]
pub struct VersionString {
    version: emver::Version,
    string: String,
}
impl VersionString {
    pub fn as_str(&self) -> &str {
        self.string.as_str()
    }
    pub fn into_version(self) -> emver::Version {
        self.version
    }
}
impl std::fmt::Display for VersionString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}
impl std::str::FromStr for VersionString {
    type Err = <emver::Version as FromStr>::Err;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(VersionString {
            string: s.to_owned(),
            version: s.parse()?,
        })
    }
}
impl From<emver::Version> for VersionString {
    fn from(v: emver::Version) -> Self {
        VersionString {
            string: v.to_string(),
            version: v,
        }
    }
}
impl From<VersionString> for emver::Version {
    fn from(v: VersionString) -> Self {
        v.version
    }
}
impl Default for VersionString {
    fn default() -> Self {
        Self::from(emver::Version::default())
    }
}
impl Deref for VersionString {
    type Target = emver::Version;
    fn deref(&self) -> &Self::Target {
        &self.version
    }
}
impl AsRef<emver::Version> for VersionString {
    fn as_ref(&self) -> &emver::Version {
        &self.version
    }
}
impl AsRef<str> for VersionString {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}
impl PartialEq for VersionString {
    fn eq(&self, other: &VersionString) -> bool {
        self.version.eq(&other.version)
    }
}
impl Eq for VersionString {}
impl PartialOrd for VersionString {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.version.partial_cmp(&other.version)
    }
}
impl Ord for VersionString {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.version.cmp(&other.version)
    }
}
impl Hash for VersionString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.version.hash(state)
    }
}
impl<'de> Deserialize<'de> for VersionString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let string = String::deserialize(deserializer)?;
        let version = emver::Version::from_str(&string).map_err(::serde::de::Error::custom)?;
        Ok(Self { string, version })
    }
}
impl Serialize for VersionString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.string.serialize(serializer)
    }
}
