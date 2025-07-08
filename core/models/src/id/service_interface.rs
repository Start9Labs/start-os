use std::path::Path;
use std::str::FromStr;

use rpc_toolkit::clap::builder::ValueParserFactory;
use serde::{Deserialize, Deserializer, Serialize};
use ts_rs::TS;

use crate::{FromStrParser, Id};

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, TS)]
#[ts(export, type = "string")]
pub struct ServiceInterfaceId(Id);
impl From<Id> for ServiceInterfaceId {
    fn from(id: Id) -> Self {
        Self(id)
    }
}
impl std::fmt::Display for ServiceInterfaceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}
impl std::ops::Deref for ServiceInterfaceId {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl AsRef<str> for ServiceInterfaceId {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<'de> Deserialize<'de> for ServiceInterfaceId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(ServiceInterfaceId(Deserialize::deserialize(deserializer)?))
    }
}
impl AsRef<Path> for ServiceInterfaceId {
    fn as_ref(&self) -> &Path {
        self.0.as_ref().as_ref()
    }
}
impl<'q> sqlx::Encode<'q, sqlx::Postgres> for ServiceInterfaceId {
    fn encode_by_ref(
        &self,
        buf: &mut <sqlx::Postgres as sqlx::Database>::ArgumentBuffer<'q>,
    ) -> Result<sqlx::encode::IsNull, sqlx::error::BoxDynError> {
        <&str as sqlx::Encode<'q, sqlx::Postgres>>::encode_by_ref(&&**self, buf)
    }
}
impl sqlx::Type<sqlx::Postgres> for ServiceInterfaceId {
    fn type_info() -> sqlx::postgres::PgTypeInfo {
        <&str as sqlx::Type<sqlx::Postgres>>::type_info()
    }

    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> bool {
        <&str as sqlx::Type<sqlx::Postgres>>::compatible(ty)
    }
}
impl FromStr for ServiceInterfaceId {
    type Err = <Id as FromStr>::Err;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Id::from_str(s).map(Self)
    }
}
impl ValueParserFactory for ServiceInterfaceId {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        FromStrParser::new()
    }
}
