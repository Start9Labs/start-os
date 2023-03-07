use std::borrow::Borrow;
use std::path::Path;

use serde::{Deserialize, Deserializer, Serialize};

use crate::Id;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VolumeId {
    Backup,
    Custom(Id),
}
impl std::fmt::Display for VolumeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VolumeId::Backup => write!(f, "BACKUP"),
            VolumeId::Custom(id) => write!(f, "{}", id),
        }
    }
}
impl AsRef<str> for VolumeId {
    fn as_ref(&self) -> &str {
        match self {
            VolumeId::Backup => "BACKUP",
            VolumeId::Custom(id) => id.as_ref(),
        }
    }
}
impl Borrow<str> for VolumeId {
    fn borrow(&self) -> &str {
        self.as_ref()
    }
}
impl AsRef<Path> for VolumeId {
    fn as_ref(&self) -> &Path {
        AsRef::<str>::as_ref(self).as_ref()
    }
}
impl<'de> Deserialize<'de> for VolumeId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let unchecked: String = Deserialize::deserialize(deserializer)?;
        Ok(match unchecked.as_ref() {
            "BACKUP" => VolumeId::Backup,
            _ => VolumeId::Custom(Id::try_from(unchecked).map_err(serde::de::Error::custom)?),
        })
    }
}
impl Serialize for VolumeId {
    fn serialize<Ser>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error>
    where
        Ser: serde::Serializer,
    {
        serializer.serialize_str(self.as_ref())
    }
}
