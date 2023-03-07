use std::fmt::Debug;
use std::str::FromStr;

use serde::{Deserialize, Deserializer, Serialize};

use crate::{Id, InvalidId, PackageId, Version};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct ImageId(Id);
impl std::fmt::Display for ImageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl ImageId {
    pub fn for_package(&self, pkg_id: &PackageId, pkg_version: Option<&Version>) -> String {
        format!(
            "start9/{}/{}:{}",
            pkg_id,
            self.0,
            pkg_version.map(|v| { v.as_str() }).unwrap_or("latest")
        )
    }
}
impl FromStr for ImageId {
    type Err = InvalidId;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(ImageId(Id::try_from(s.to_owned())?))
    }
}
impl<'de> Deserialize<'de> for ImageId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(ImageId(Deserialize::deserialize(deserializer)?))
    }
}
