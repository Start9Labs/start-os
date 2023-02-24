use std::borrow::Borrow;
use std::path::Path;
use std::str::FromStr;

use serde::{Deserialize, Serialize, Serializer};

use crate::{Id, InvalidId, SYSTEM_ID};

lazy_static::lazy_static! {
    pub static ref SYSTEM_PACKAGE_ID: PackageId = PackageId(SYSTEM_ID.clone());
}
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PackageId(Id);
impl FromStr for PackageId {
    type Err = InvalidId;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(PackageId(Id::try_from(s.to_owned())?))
    }
}
impl From<Id> for PackageId {
    fn from(id: Id) -> Self {
        PackageId(id)
    }
}
impl std::ops::Deref for PackageId {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl AsRef<PackageId> for PackageId {
    fn as_ref(&self) -> &PackageId {
        self
    }
}
impl std::fmt::Display for PackageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl AsRef<str> for PackageId {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl Borrow<str> for PackageId {
    fn borrow(&self) -> &str {
        self.0.as_ref()
    }
}
impl AsRef<Path> for PackageId {
    fn as_ref(&self) -> &Path {
        self.0.as_ref().as_ref()
    }
}
impl<'de> Deserialize<'de> for PackageId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        Ok(PackageId(Deserialize::deserialize(deserializer)?))
    }
}
impl Serialize for PackageId {
    fn serialize<Ser>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error>
    where
        Ser: Serializer,
    {
        Serialize::serialize(&self.0, serializer)
    }
}
