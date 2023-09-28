use std::borrow::Borrow;

use regex::Regex;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use yasi::InternedString;

mod action;
mod address;
mod health_check;
mod image;
mod interface;
mod invalid_id;
mod package;
mod volume;

pub use action::ActionId;
pub use address::AddressId;
pub use health_check::HealthCheckId;
pub use image::ImageId;
pub use interface::InterfaceId;
pub use invalid_id::InvalidId;
pub use package::{PackageId, SYSTEM_PACKAGE_ID};
pub use volume::VolumeId;

lazy_static::lazy_static! {
    static ref ID_REGEX: Regex = Regex::new("^[a-z]+(-[a-z]+)*$").unwrap();
    pub static ref SYSTEM_ID: Id = Id(InternedString::intern("x_system"));
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Id(InternedString);
impl TryFrom<InternedString> for Id {
    type Error = InvalidId;
    fn try_from(value: InternedString) -> Result<Self, Self::Error> {
        if ID_REGEX.is_match(&*value) {
            Ok(Id(value))
        } else {
            Err(InvalidId)
        }
    }
}
impl TryFrom<String> for Id {
    type Error = InvalidId;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        if ID_REGEX.is_match(&value) {
            Ok(Id(InternedString::intern(value)))
        } else {
            Err(InvalidId)
        }
    }
}
impl TryFrom<&str> for Id {
    type Error = InvalidId;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if ID_REGEX.is_match(&value) {
            Ok(Id(InternedString::intern(value)))
        } else {
            Err(InvalidId)
        }
    }
}
impl std::ops::Deref for Id {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &*self.0)
    }
}
impl AsRef<str> for Id {
    fn as_ref(&self) -> &str {
        &*self.0
    }
}
impl Borrow<str> for Id {
    fn borrow(&self) -> &str {
        self.0.as_ref()
    }
}
impl<'de> Deserialize<'de> for Id {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let unchecked: InternedString = Deserialize::deserialize(deserializer)?;
        Id::try_from(unchecked).map_err(serde::de::Error::custom)
    }
}
impl Serialize for Id {
    fn serialize<Ser>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error>
    where
        Ser: Serializer,
    {
        serializer.serialize_str(&*self)
    }
}
impl<'q> sqlx::Encode<'q, sqlx::Postgres> for Id {
    fn encode_by_ref(
        &self,
        buf: &mut <sqlx::Postgres as sqlx::database::HasArguments<'q>>::ArgumentBuffer,
    ) -> sqlx::encode::IsNull {
        <&str as sqlx::Encode<'q, sqlx::Postgres>>::encode_by_ref(&&**self, buf)
    }
}
impl sqlx::Type<sqlx::Postgres> for Id {
    fn type_info() -> sqlx::postgres::PgTypeInfo {
        <&str as sqlx::Type<sqlx::Postgres>>::type_info()
    }

    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> bool {
        <&str as sqlx::Type<sqlx::Postgres>>::compatible(ty)
    }
}
