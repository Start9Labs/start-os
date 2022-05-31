use std::{str::FromStr, ops::Deref};
use std::hash::{Hash, Hasher};

use patch_db::{HasModel, Model};
use serde::{Deserialize, Deserializer, Serialize, Serializer};


#[derive(Debug, Clone)]
pub struct Version {
    version: emver::Version,
    string: String,
}
impl Version {
    pub fn as_str(&self) -> &str {
        self.string.as_str()
    }
    pub fn into_version(self) -> emver::Version {
        self.version
    }
}
impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}
impl std::str::FromStr for Version {
    type Err = <emver::Version as FromStr>::Err;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Version {
            string: s.to_owned(),
            version: s.parse()?,
        })
    }
}
impl From<emver::Version> for Version {
    fn from(v: emver::Version) -> Self {
        Version {
            string: v.to_string(),
            version: v,
        }
    }
}
impl From<Version> for emver::Version {
    fn from(v: Version) -> Self {
        v.version
    }
}
impl Default for Version {
    fn default() -> Self {
        Self::from(emver::Version::default())
    }
}
impl Deref for Version {
    type Target = emver::Version;
    fn deref(&self) -> &Self::Target {
        &self.version
    }
}
impl AsRef<emver::Version> for Version {
    fn as_ref(&self) -> &emver::Version {
        &self.version
    }
}
impl AsRef<str> for Version {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}
impl PartialEq for Version {
    fn eq(&self, other: &Version) -> bool {
        self.version.eq(&other.version)
    }
}
impl Eq for Version {}
impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.version.partial_cmp(&other.version)
    }
}
impl Ord for Version {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.version.cmp(&other.version)
    }
}
impl Hash for Version {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.version.hash(state)
    }
}
impl<'de> Deserialize<'de> for Version {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let string = String::deserialize(deserializer)?;
        let version = emver::Version::from_str(&string).map_err(::serde::de::Error::custom)?;
        Ok(Self { string, version })
    }
}
impl Serialize for Version {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.string.serialize(serializer)
    }
}
impl HasModel for Version {
    type Model = Model<Version>;
}