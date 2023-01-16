use std::borrow::Borrow;

use internment::ArcIntern;
use regex::Regex;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::invalid_id::InvalidId;

lazy_static::lazy_static! {
    static ref ID_REGEX: Regex = Regex::new("^[a-z]+(-[a-z]+)*$").unwrap();
    pub static ref SYSTEM_ID: Id = Id(ArcIntern::from_ref("x_system"));
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Id(ArcIntern<String>);
impl TryFrom<ArcIntern<String>> for Id {
    type Error = InvalidId;
    fn try_from(value: ArcIntern<String>) -> Result<Self, Self::Error> {
        if ID_REGEX.is_match(&*value) {
            Ok(Id(value))
        } else {
            Err(InvalidId)
        }
    }
}
impl TryFrom<String> for Id {
    type Error = InvalidId;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        if ID_REGEX.is_match(&value) {
            Ok(Id(ArcIntern::new(value)))
        } else {
            Err(InvalidId)
        }
    }
}
impl TryFrom<&str> for Id {
    type Error = InvalidId;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if ID_REGEX.is_match(&value) {
            Ok(Id(ArcIntern::from_ref(value)))
        } else {
            Err(InvalidId)
        }
    }
}
impl std::ops::Deref for Id {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &*self.0)
    }
}
impl AsRef<str> for Id {
    fn as_ref(&self) -> &str {
        &*self.0
    }
}
impl Borrow<str> for Id {
    fn borrow(&self) -> &str {
        self.0.as_ref()
    }
}
impl<'de> Deserialize<'de> for Id {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let unchecked: String = Deserialize::deserialize(deserializer)?;
        Id::try_from(unchecked).map_err(serde::de::Error::custom)
    }
}
impl Serialize for Id {
    fn serialize<Ser>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error>
    where
        Ser: Serializer,
    {
        serializer.serialize_str(self.as_ref())
    }
}
