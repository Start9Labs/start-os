use std::path::Path;

use serde::{Deserialize, Deserializer, Serialize};

use crate::Id;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Default)]
pub struct InterfaceId<S: AsRef<str> = String>(Id<S>);
impl<S: AsRef<str>> From<Id<S>> for InterfaceId<S> {
    fn from(id: Id<S>) -> Self {
        Self(id)
    }
}
impl<S: AsRef<str>> std::fmt::Display for InterfaceId<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl<S: AsRef<str>> std::ops::Deref for InterfaceId<S> {
    type Target = S;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl<S: AsRef<str>> AsRef<str> for InterfaceId<S> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<'de, S> Deserialize<'de> for InterfaceId<S>
where
    S: AsRef<str>,
    Id<S>: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(InterfaceId(Deserialize::deserialize(deserializer)?))
    }
}
impl<S: AsRef<str>> AsRef<Path> for InterfaceId<S> {
    fn as_ref(&self) -> &Path {
        self.0.as_ref().as_ref()
    }
}
