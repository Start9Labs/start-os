use std::borrow::Borrow;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::id_unchecked::IdUnchecked;
use crate::invalid_id::InvalidId;

pub const SYSTEM_ID: Id<&'static str> = Id("x_system");

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Id<S: AsRef<str> = String>(S);
impl<S: AsRef<str>> Id<S> {
    pub fn try_from(value: S) -> Result<Self, InvalidId> {
        if value
            .as_ref()
            .chars()
            .all(|c| c.is_ascii_lowercase() || c == '-')
        {
            Ok(Id(value))
        } else {
            Err(InvalidId)
        }
    }
}
impl<'a> Id<&'a str> {
    pub fn owned(&self) -> Id {
        Id(self.0.to_owned())
    }
}
impl From<Id> for String {
    fn from(value: Id) -> Self {
        value.0
    }
}
impl<S: AsRef<str>> std::ops::Deref for Id<S> {
    type Target = S;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<S: AsRef<str>> std::fmt::Display for Id<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.as_ref())
    }
}
impl<S: AsRef<str>> AsRef<str> for Id<S> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<S: AsRef<str>> Borrow<str> for Id<S> {
    fn borrow(&self) -> &str {
        self.0.as_ref()
    }
}
impl<'de, S> Deserialize<'de> for Id<S>
where
    S: AsRef<str>,
    IdUnchecked<S>: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let unchecked: IdUnchecked<S> = Deserialize::deserialize(deserializer)?;
        Id::try_from(unchecked.0).map_err(serde::de::Error::custom)
    }
}
impl<S: AsRef<str>> Serialize for Id<S> {
    fn serialize<Ser>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error>
    where
        Ser: Serializer,
    {
        serializer.serialize_str(self.as_ref())
    }
}
