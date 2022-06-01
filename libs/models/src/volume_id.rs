use std::{borrow::Borrow, path::Path};

use serde::{Deserialize, Serialize, Deserializer};

use crate::{Id, IdUnchecked};


#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VolumeId<S: AsRef<str> = String> {
    Backup,
    Custom(Id<S>),
}
impl<S: AsRef<str>> std::fmt::Display for VolumeId<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VolumeId::Backup => write!(f, "BACKUP"),
            VolumeId::Custom(id) => write!(f, "{}", id),
        }
    }
}
impl<S: AsRef<str>> AsRef<str> for VolumeId<S> {
    fn as_ref(&self) -> &str {
        match self {
            VolumeId::Backup => "BACKUP",
            VolumeId::Custom(id) => id.as_ref(),
        }
    }
}
impl<S: AsRef<str>> Borrow<str> for VolumeId<S> {
    fn borrow(&self) -> &str {
        self.as_ref()
    }
}
impl<S: AsRef<str>> AsRef<Path> for VolumeId<S> {
    fn as_ref(&self) -> &Path {
        AsRef::<str>::as_ref(self).as_ref()
    }
}
impl<'de, S> Deserialize<'de> for VolumeId<S>
where
    S: AsRef<str>,
    IdUnchecked<S>: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let unchecked: IdUnchecked<S> = Deserialize::deserialize(deserializer)?;
        Ok(match unchecked.0.as_ref() {
            "BACKUP" => VolumeId::Backup,
            _ => VolumeId::Custom(Id::try_from(unchecked.0).map_err(serde::de::Error::custom)?),
        })
    }
}
impl<S: AsRef<str>> Serialize for VolumeId<S> {
    fn serialize<Ser>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error>
    where
        Ser: serde::Serializer,
    {
        serializer.serialize_str(self.as_ref())
    }
}
