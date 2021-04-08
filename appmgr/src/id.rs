use std::borrow::{Borrow, Cow};
use std::fmt::Debug;
use std::path::Path;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::util::Version;
use crate::Error;

pub const SYSTEM_ID: Id<&'static str> = Id("SYSTEM");

#[derive(Debug, thiserror::Error)]
#[error("Invalid ID")]
pub struct InvalidId;
impl From<InvalidId> for Error {
    fn from(err: InvalidId) -> Self {
        Error::new(err, crate::error::ErrorKind::InvalidPackageId)
    }
}

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct ImageId<S: AsRef<str> = String>(Id<S>);
impl<S: AsRef<str>> std::fmt::Display for ImageId<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl<S: AsRef<str>> ImageId<S> {
    pub fn for_package<PkgId: AsRef<crate::s9pk::manifest::PackageId<S0>>, S0: AsRef<str>>(
        &self,
        pkg_id: PkgId,
        pkg_version: Option<&Version>,
    ) -> String {
        format!(
            "start9/{}/{}:{}",
            pkg_id.as_ref(),
            self.0,
            pkg_version.map(|v| { v.as_str() }).unwrap_or("latest")
        )
    }
}
impl<'de, S> Deserialize<'de> for ImageId<S>
where
    S: AsRef<str>,
    Id<S>: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(ImageId(Deserialize::deserialize(deserializer)?))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct InterfaceId<S: AsRef<str> = String>(Id<S>);
impl<S: AsRef<str>> std::fmt::Display for InterfaceId<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
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
