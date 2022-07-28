use std::borrow::Borrow;
use std::path::Path;
use std::str::FromStr;

use serde::{Deserialize, Serialize, Serializer};

use crate::{Id, InvalidId, SYSTEM_ID};

pub const SYSTEM_PACKAGE_ID: PackageId<&'static str> = PackageId(SYSTEM_ID);
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PackageId<S: AsRef<str> = String>(Id<S>);
impl<'a> PackageId<&'a str> {
    pub fn owned(&self) -> PackageId {
        PackageId(self.0.owned())
    }
}
impl FromStr for PackageId {
    type Err = InvalidId;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(PackageId(Id::try_from(s.to_owned())?))
    }
}
impl From<PackageId> for String {
    fn from(value: PackageId) -> Self {
        value.0.into()
    }
}
impl<S: AsRef<str>> From<Id<S>> for PackageId<S> {
    fn from(id: Id<S>) -> Self {
        PackageId(id)
    }
}
impl<S: AsRef<str>> std::ops::Deref for PackageId<S> {
    type Target = S;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl<S: AsRef<str>> AsRef<PackageId<S>> for PackageId<S> {
    fn as_ref(&self) -> &PackageId<S> {
        self
    }
}
impl<S: AsRef<str>> std::fmt::Display for PackageId<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl<S: AsRef<str>> AsRef<str> for PackageId<S> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<S: AsRef<str>> Borrow<str> for PackageId<S> {
    fn borrow(&self) -> &str {
        self.0.as_ref()
    }
}
impl<S: AsRef<str>> AsRef<Path> for PackageId<S> {
    fn as_ref(&self) -> &Path {
        self.0.as_ref().as_ref()
    }
}
impl<'de, S> Deserialize<'de> for PackageId<S>
where
    S: AsRef<str>,
    Id<S>: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        Ok(PackageId(Deserialize::deserialize(deserializer)?))
    }
}
impl<S> Serialize for PackageId<S>
where
    S: AsRef<str>,
{
    fn serialize<Ser>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error>
    where
        Ser: Serializer,
    {
        Serialize::serialize(&self.0, serializer)
    }
}
