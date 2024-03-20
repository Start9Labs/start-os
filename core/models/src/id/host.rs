use std::path::Path;
use std::str::FromStr;

use serde::{Deserialize, Deserializer, Serialize};
use yasi::InternedString;

use crate::{Id, InvalidId};

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct HostId(Id);
impl FromStr for HostId {
    type Err = InvalidId;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(Id::try_from(s.to_owned())?))
    }
}
impl From<Id> for HostId {
    fn from(id: Id) -> Self {
        Self(id)
    }
}
impl From<HostId> for Id {
    fn from(value: HostId) -> Self {
        value.0
    }
}
impl From<HostId> for InternedString {
    fn from(value: HostId) -> Self {
        value.0.into()
    }
}
impl std::fmt::Display for HostId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl std::ops::Deref for HostId {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl AsRef<str> for HostId {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<'de> Deserialize<'de> for HostId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(HostId(Deserialize::deserialize(deserializer)?))
    }
}
impl AsRef<Path> for HostId {
    fn as_ref(&self) -> &Path {
        self.0.as_ref().as_ref()
    }
}
impl<'q> sqlx::Encode<'q, sqlx::Postgres> for HostId {
    fn encode_by_ref(
        &self,
        buf: &mut <sqlx::Postgres as sqlx::database::HasArguments<'q>>::ArgumentBuffer,
    ) -> sqlx::encode::IsNull {
        <&str as sqlx::Encode<'q, sqlx::Postgres>>::encode_by_ref(&&**self, buf)
    }
}
impl sqlx::Type<sqlx::Postgres> for HostId {
    fn type_info() -> sqlx::postgres::PgTypeInfo {
        <&str as sqlx::Type<sqlx::Postgres>>::type_info()
    }

    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> bool {
        <&str as sqlx::Type<sqlx::Postgres>>::compatible(ty)
    }
}
