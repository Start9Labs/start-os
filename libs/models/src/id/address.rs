use std::path::Path;

use serde::{Deserialize, Deserializer, Serialize};

use crate::Id;

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct AddressId(Id);
impl From<Id> for AddressId {
    fn from(id: Id) -> Self {
        Self(id)
    }
}
impl std::fmt::Display for AddressId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl std::ops::Deref for AddressId {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl AsRef<str> for AddressId {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<'de> Deserialize<'de> for AddressId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(AddressId(Deserialize::deserialize(deserializer)?))
    }
}
impl AsRef<Path> for AddressId {
    fn as_ref(&self) -> &Path {
        self.0.as_ref().as_ref()
    }
}
impl<'q> sqlx::Encode<'q, sqlx::Postgres> for AddressId {
    fn encode_by_ref(
        &self,
        buf: &mut <sqlx::Postgres as sqlx::database::HasArguments<'q>>::ArgumentBuffer,
    ) -> sqlx::encode::IsNull {
        <&str as sqlx::Encode<'q, sqlx::Postgres>>::encode_by_ref(&&**self, buf)
    }
}
impl sqlx::Type<sqlx::Postgres> for AddressId {
    fn type_info() -> sqlx::postgres::PgTypeInfo {
        <&str as sqlx::Type<sqlx::Postgres>>::type_info()
    }

    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> bool {
        <&str as sqlx::Type<sqlx::Postgres>>::compatible(ty)
    }
}
