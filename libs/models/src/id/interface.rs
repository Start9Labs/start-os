use std::path::Path;

use serde::{Deserialize, Deserializer, Serialize};

use crate::Id;

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct InterfaceId(Id);
impl From<Id> for InterfaceId {
    fn from(id: Id) -> Self {
        Self(id)
    }
}
impl std::fmt::Display for InterfaceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl std::ops::Deref for InterfaceId {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl AsRef<str> for InterfaceId {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<'de> Deserialize<'de> for InterfaceId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(InterfaceId(Deserialize::deserialize(deserializer)?))
    }
}
impl AsRef<Path> for InterfaceId {
    fn as_ref(&self) -> &Path {
        self.0.as_ref().as_ref()
    }
}
impl<'q> sqlx::Encode<'q, sqlx::Postgres> for InterfaceId {
    fn encode_by_ref(
        &self,
        buf: &mut <sqlx::Postgres as sqlx::database::HasArguments<'q>>::ArgumentBuffer,
    ) -> sqlx::encode::IsNull {
        <&str as sqlx::Encode<'q, sqlx::Postgres>>::encode_by_ref(&&**self, buf)
    }
}
impl sqlx::Type<sqlx::Postgres> for InterfaceId {
    fn type_info() -> sqlx::postgres::PgTypeInfo {
        <&str as sqlx::Type<sqlx::Postgres>>::type_info()
    }

    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> bool {
        <&str as sqlx::Type<sqlx::Postgres>>::compatible(ty)
    }
}
