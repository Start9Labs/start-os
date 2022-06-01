use std::{str::FromStr, path::Path};

use serde::{Serialize, Deserialize};

use crate::{Id, InvalidId};


#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct ActionId<S: AsRef<str> = String>(Id<S>);
impl FromStr for ActionId {
    type Err = InvalidId;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(ActionId(Id::try_from(s.to_owned())?))
    }
}
impl From<ActionId> for String {
    fn from(value: ActionId) -> Self {
        value.0.into()
    }
}
impl<S: AsRef<str>> AsRef<ActionId<S>> for ActionId<S> {
    fn as_ref(&self) -> &ActionId<S> {
        self
    }
}
impl<S: AsRef<str>> std::fmt::Display for ActionId<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl<S: AsRef<str>> AsRef<str> for ActionId<S> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<S: AsRef<str>> AsRef<Path> for ActionId<S> {
    fn as_ref(&self) -> &Path {
        self.0.as_ref().as_ref()
    }
}
impl<'de, S> Deserialize<'de> for ActionId<S>
where
    S: AsRef<str>,
    Id<S>: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        Ok(ActionId(serde::Deserialize::deserialize(deserializer)?))
    }
}
