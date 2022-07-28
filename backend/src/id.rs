use std::fmt::Debug;
use std::str::FromStr;

pub use models::{Id, IdUnchecked, InvalidId, SYSTEM_ID};
use serde::{Deserialize, Deserializer, Serialize};

use crate::util::Version;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
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
impl FromStr for ImageId {
    type Err = InvalidId;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(ImageId(Id::try_from(s.to_owned())?))
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
