use std::borrow::Cow;

use serde::{Deserialize, Deserializer};


#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IdUnchecked<S: AsRef<str>>(pub S);
impl<'de> Deserialize<'de> for IdUnchecked<Cow<'de, str>> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = IdUnchecked<Cow<'de, str>>;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a valid ID")
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(IdUnchecked(Cow::Owned(v.to_owned())))
            }
            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(IdUnchecked(Cow::Owned(v)))
            }
            fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(IdUnchecked(Cow::Borrowed(v)))
            }
        }
        deserializer.deserialize_any(Visitor)
    }
}
impl<'de> Deserialize<'de> for IdUnchecked<String> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(IdUnchecked(String::deserialize(deserializer)?))
    }
}
impl<'de> Deserialize<'de> for IdUnchecked<&'de str> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(IdUnchecked(<&'de str>::deserialize(deserializer)?))
    }
}