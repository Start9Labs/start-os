use std::marker::PhantomData;
use std::ops::Deref;
use std::process::exit;
use std::str::FromStr;

use clap::ArgMatches;
use color_eyre::eyre::eyre;
use serde::ser::{SerializeMap, SerializeSeq};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value;

use super::IntoDoubleEndedIterator;
use crate::{Error, ResultExt};

pub fn deserialize_from_str<
    'de,
    D: serde::de::Deserializer<'de>,
    T: FromStr<Err = E>,
    E: std::fmt::Display,
>(
    deserializer: D,
) -> std::result::Result<T, D::Error> {
    struct Visitor<T: FromStr<Err = E>, E>(std::marker::PhantomData<T>);
    impl<'de, T: FromStr<Err = Err>, Err: std::fmt::Display> serde::de::Visitor<'de>
        for Visitor<T, Err>
    {
        type Value = T;
        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(formatter, "a parsable string")
        }
        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            v.parse().map_err(|e| serde::de::Error::custom(e))
        }
    }
    deserializer.deserialize_str(Visitor(std::marker::PhantomData))
}

pub fn deserialize_from_str_opt<
    'de,
    D: serde::de::Deserializer<'de>,
    T: FromStr<Err = E>,
    E: std::fmt::Display,
>(
    deserializer: D,
) -> std::result::Result<Option<T>, D::Error> {
    struct Visitor<T: FromStr<Err = E>, E>(std::marker::PhantomData<T>);
    impl<'de, T: FromStr<Err = Err>, Err: std::fmt::Display> serde::de::Visitor<'de>
        for Visitor<T, Err>
    {
        type Value = Option<T>;
        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(formatter, "a parsable string")
        }
        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            v.parse().map(Some).map_err(|e| serde::de::Error::custom(e))
        }
        fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
        where
            D: serde::de::Deserializer<'de>,
        {
            deserializer.deserialize_str(Visitor(std::marker::PhantomData))
        }
        fn visit_none<E>(self) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(None)
        }
        fn visit_unit<E>(self) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(None)
        }
    }
    deserializer.deserialize_any(Visitor(std::marker::PhantomData))
}

pub fn serialize_display<T: std::fmt::Display, S: Serializer>(
    t: &T,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    String::serialize(&t.to_string(), serializer)
}

pub fn serialize_display_opt<T: std::fmt::Display, S: Serializer>(
    t: &Option<T>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    Option::<String>::serialize(&t.as_ref().map(|t| t.to_string()), serializer)
}

pub mod ed25519_pubkey {
    use ed25519_dalek::PublicKey;
    use serde::de::{Error, Unexpected, Visitor};
    use serde::{Deserializer, Serializer};

    pub fn serialize<S: Serializer>(pubkey: &PublicKey, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&base32::encode(
            base32::Alphabet::RFC4648 { padding: true },
            pubkey.as_bytes(),
        ))
    }
    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<PublicKey, D::Error> {
        struct PubkeyVisitor;
        impl<'de> Visitor<'de> for PubkeyVisitor {
            type Value = ed25519_dalek::PublicKey;
            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(formatter, "an RFC4648 encoded string")
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                PublicKey::from_bytes(
                    &base32::decode(base32::Alphabet::RFC4648 { padding: true }, v).ok_or(
                        Error::invalid_value(Unexpected::Str(v), &"an RFC4648 encoded string"),
                    )?,
                )
                .map_err(Error::custom)
            }
        }
        deserializer.deserialize_str(PubkeyVisitor)
    }
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum ValuePrimative {
    Null,
    Boolean(bool),
    String(String),
    Number(serde_json::Number),
}
impl<'de> serde::de::Deserialize<'de> for ValuePrimative {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = ValuePrimative;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a JSON primative value")
            }
            fn visit_unit<E>(self) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Null)
            }
            fn visit_none<E>(self) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Null)
            }
            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Boolean(v))
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::String(v.to_owned()))
            }
            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::String(v))
            }
            fn visit_f32<E>(self, v: f32) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(
                    serde_json::Number::from_f64(v as f64).ok_or_else(|| {
                        serde::de::Error::invalid_value(
                            serde::de::Unexpected::Float(v as f64),
                            &"a finite number",
                        )
                    })?,
                ))
            }
            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(
                    serde_json::Number::from_f64(v).ok_or_else(|| {
                        serde::de::Error::invalid_value(
                            serde::de::Unexpected::Float(v),
                            &"a finite number",
                        )
                    })?,
                ))
            }
            fn visit_u8<E>(self, v: u8) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
            fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
            fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
            fn visit_i8<E>(self, v: i8) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
            fn visit_i16<E>(self, v: i16) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
            fn visit_i32<E>(self, v: i32) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
        }
        deserializer.deserialize_any(Visitor)
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum IoFormat {
    Json,
    JsonPretty,
    Yaml,
    Cbor,
    Toml,
    TomlPretty,
}
impl Default for IoFormat {
    fn default() -> Self {
        IoFormat::JsonPretty
    }
}
impl std::fmt::Display for IoFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IoFormat::*;
        match self {
            Json => write!(f, "JSON"),
            JsonPretty => write!(f, "JSON (pretty)"),
            Yaml => write!(f, "YAML"),
            Cbor => write!(f, "CBOR"),
            Toml => write!(f, "TOML"),
            TomlPretty => write!(f, "TOML (pretty)"),
        }
    }
}
impl std::str::FromStr for IoFormat {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        serde_json::from_value(Value::String(s.to_owned()))
            .with_kind(crate::ErrorKind::Deserialization)
    }
}
impl IoFormat {
    pub fn to_writer<W: std::io::Write, T: Serialize>(
        &self,
        mut writer: W,
        value: &T,
    ) -> Result<(), Error> {
        match self {
            IoFormat::Json => {
                serde_json::to_writer(writer, value).with_kind(crate::ErrorKind::Serialization)
            }
            IoFormat::JsonPretty => serde_json::to_writer_pretty(writer, value)
                .with_kind(crate::ErrorKind::Serialization),
            IoFormat::Yaml => {
                serde_yaml::to_writer(writer, value).with_kind(crate::ErrorKind::Serialization)
            }
            IoFormat::Cbor => serde_cbor::ser::into_writer(value, writer)
                .with_kind(crate::ErrorKind::Serialization),
            IoFormat::Toml => writer
                .write_all(
                    &serde_toml::to_vec(
                        &serde_toml::Value::try_from(value)
                            .with_kind(crate::ErrorKind::Serialization)?,
                    )
                    .with_kind(crate::ErrorKind::Serialization)?,
                )
                .with_kind(crate::ErrorKind::Serialization),
            IoFormat::TomlPretty => writer
                .write_all(
                    serde_toml::to_string_pretty(
                        &serde_toml::Value::try_from(value)
                            .with_kind(crate::ErrorKind::Serialization)?,
                    )
                    .with_kind(crate::ErrorKind::Serialization)?
                    .as_bytes(),
                )
                .with_kind(crate::ErrorKind::Serialization),
        }
    }
    pub fn to_vec<T: Serialize>(&self, value: &T) -> Result<Vec<u8>, Error> {
        match self {
            IoFormat::Json => serde_json::to_vec(value).with_kind(crate::ErrorKind::Serialization),
            IoFormat::JsonPretty => {
                serde_json::to_vec_pretty(value).with_kind(crate::ErrorKind::Serialization)
            }
            IoFormat::Yaml => serde_yaml::to_string(value)
                .with_kind(crate::ErrorKind::Serialization)
                .map(|s| s.into_bytes()),
            IoFormat::Cbor => {
                let mut res = Vec::new();
                serde_cbor::ser::into_writer(value, &mut res)
                    .with_kind(crate::ErrorKind::Serialization)?;
                Ok(res)
            }
            IoFormat::Toml => serde_toml::to_vec(
                &serde_toml::Value::try_from(value).with_kind(crate::ErrorKind::Serialization)?,
            )
            .with_kind(crate::ErrorKind::Serialization),
            IoFormat::TomlPretty => serde_toml::to_string_pretty(
                &serde_toml::Value::try_from(value).with_kind(crate::ErrorKind::Serialization)?,
            )
            .map(|s| s.into_bytes())
            .with_kind(crate::ErrorKind::Serialization),
        }
    }
    /// BLOCKING
    pub fn from_reader<R: std::io::Read, T: for<'de> Deserialize<'de>>(
        &self,
        mut reader: R,
    ) -> Result<T, Error> {
        match self {
            IoFormat::Json | IoFormat::JsonPretty => {
                serde_json::from_reader(reader).with_kind(crate::ErrorKind::Deserialization)
            }
            IoFormat::Yaml => {
                serde_yaml::from_reader(reader).with_kind(crate::ErrorKind::Deserialization)
            }
            IoFormat::Cbor => {
                serde_cbor::de::from_reader(reader).with_kind(crate::ErrorKind::Deserialization)
            }
            IoFormat::Toml | IoFormat::TomlPretty => {
                let mut s = String::new();
                reader
                    .read_to_string(&mut s)
                    .with_kind(crate::ErrorKind::Deserialization)?;
                serde_toml::from_str(&s).with_kind(crate::ErrorKind::Deserialization)
            }
        }
    }
    pub async fn from_async_reader<
        R: tokio::io::AsyncRead + Unpin,
        T: for<'de> Deserialize<'de>,
    >(
        &self,
        reader: R,
    ) -> Result<T, Error> {
        use crate::util::io::*;
        match self {
            IoFormat::Json | IoFormat::JsonPretty => from_json_async_reader(reader).await,
            IoFormat::Yaml => from_yaml_async_reader(reader).await,
            IoFormat::Cbor => from_cbor_async_reader(reader).await,
            IoFormat::Toml | IoFormat::TomlPretty => from_toml_async_reader(reader).await,
        }
    }
    pub fn from_slice<T: for<'de> Deserialize<'de>>(&self, slice: &[u8]) -> Result<T, Error> {
        match self {
            IoFormat::Json | IoFormat::JsonPretty => {
                serde_json::from_slice(slice).with_kind(crate::ErrorKind::Deserialization)
            }
            IoFormat::Yaml => {
                serde_yaml::from_slice(slice).with_kind(crate::ErrorKind::Deserialization)
            }
            IoFormat::Cbor => {
                serde_cbor::de::from_reader(slice).with_kind(crate::ErrorKind::Deserialization)
            }
            IoFormat::Toml | IoFormat::TomlPretty => {
                serde_toml::from_slice(slice).with_kind(crate::ErrorKind::Deserialization)
            }
        }
    }
}

pub fn display_serializable<T: Serialize>(t: T, matches: &ArgMatches) {
    let format = match matches.value_of("format").map(|f| f.parse()) {
        Some(Ok(f)) => f,
        Some(Err(_)) => {
            eprintln!("unrecognized formatter");
            exit(1)
        }
        None => IoFormat::default(),
    };
    format
        .to_writer(std::io::stdout(), &t)
        .expect("Error serializing result to stdout")
}

pub fn parse_stdin_deserializable<T: for<'de> Deserialize<'de>>(
    stdin: &mut std::io::Stdin,
    matches: &ArgMatches,
) -> Result<T, Error> {
    let format = match matches.value_of("format").map(|f| f.parse()) {
        Some(Ok(f)) => f,
        Some(Err(_)) => {
            eprintln!("unrecognized formatter");
            exit(1)
        }
        None => IoFormat::default(),
    };
    format.from_reader(stdin)
}

#[derive(Debug, Clone, Copy)]
pub struct Duration(std::time::Duration);
impl Deref for Duration {
    type Target = std::time::Duration;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl From<std::time::Duration> for Duration {
    fn from(t: std::time::Duration) -> Self {
        Duration(t)
    }
}
impl std::str::FromStr for Duration {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let units_idx = s.find(|c: char| c.is_alphabetic()).ok_or_else(|| {
            Error::new(
                eyre!("Must specify units for duration"),
                crate::ErrorKind::Deserialization,
            )
        })?;
        let (num, units) = s.split_at(units_idx);
        use std::time::Duration;
        Ok(Duration(match units {
            "d" if num.contains(".") => Duration::from_secs_f64(num.parse::<f64>()? * 86_400_f64),
            "d" => Duration::from_secs(num.parse::<u64>()? * 86_400),
            "h" if num.contains(".") => Duration::from_secs_f64(num.parse::<f64>()? * 3_600_f64),
            "h" => Duration::from_secs(num.parse::<u64>()? * 3_600),
            "m" if num.contains(".") => Duration::from_secs_f64(num.parse::<f64>()? * 60_f64),
            "m" => Duration::from_secs(num.parse::<u64>()? * 60),
            "s" if num.contains(".") => Duration::from_secs_f64(num.parse()?),
            "s" => Duration::from_secs(num.parse()?),
            "ms" if num.contains(".") => Duration::from_secs_f64(num.parse::<f64>()? / 1_000_f64),
            "ms" => {
                let millis: u128 = num.parse()?;
                Duration::new((millis / 1_000) as u64, (millis % 1_000) as u32)
            }
            "us" | "µs" if num.contains(".") => {
                Duration::from_secs_f64(num.parse::<f64>()? / 1_000_000_f64)
            }
            "us" | "µs" => {
                let micros: u128 = num.parse()?;
                Duration::new((micros / 1_000_000) as u64, (micros % 1_000_000) as u32)
            }
            "ns" if num.contains(".") => {
                Duration::from_secs_f64(num.parse::<f64>()? / 1_000_000_000_f64)
            }
            "ns" => {
                let nanos: u128 = num.parse()?;
                Duration::new(
                    (nanos / 1_000_000_000) as u64,
                    (nanos % 1_000_000_000) as u32,
                )
            }
            _ => {
                return Err(Error::new(
                    eyre!("Invalid units for duration"),
                    crate::ErrorKind::Deserialization,
                ))
            }
        }))
    }
}
impl std::fmt::Display for Duration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nanos = self.as_nanos();
        match () {
            _ if nanos % 86_400_000_000_000 == 0 => write!(f, "{}d", nanos / 86_400_000_000_000),
            _ if nanos % 3_600_000_000_000 == 0 => write!(f, "{}h", nanos / 3_600_000_000_000),
            _ if nanos % 60_000_000_000 == 0 => write!(f, "{}m", nanos / 60_000_000_000),
            _ if nanos % 1_000_000_000 == 0 => write!(f, "{}s", nanos / 1_000_000_000),
            _ if nanos % 1_000_000 == 0 => write!(f, "{}ms", nanos / 1_000_000),
            _ if nanos % 1_000 == 0 => write!(f, "{}µs", nanos / 1_000),
            _ => write!(f, "{}ns", nanos),
        }
    }
}
impl<'de> Deserialize<'de> for Duration {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserialize_from_str(deserializer)
    }
}
impl Serialize for Duration {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serialize_display(self, serializer)
    }
}

pub fn deserialize_number_permissive<
    'de,
    D: serde::de::Deserializer<'de>,
    T: FromStr<Err = E> + num::cast::FromPrimitive,
    E: std::fmt::Display,
>(
    deserializer: D,
) -> std::result::Result<T, D::Error> {
    struct Visitor<T: FromStr<Err = E> + num::cast::FromPrimitive, E>(std::marker::PhantomData<T>);
    impl<'de, T: FromStr<Err = Err> + num::cast::FromPrimitive, Err: std::fmt::Display>
        serde::de::Visitor<'de> for Visitor<T, Err>
    {
        type Value = T;
        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(formatter, "a parsable string")
        }
        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            v.parse().map_err(|e| serde::de::Error::custom(e))
        }
        fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            T::from_f64(v).ok_or_else(|| {
                serde::de::Error::custom(format!(
                    "{} cannot be represented by the requested type",
                    v
                ))
            })
        }
        fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            T::from_u64(v).ok_or_else(|| {
                serde::de::Error::custom(format!(
                    "{} cannot be represented by the requested type",
                    v
                ))
            })
        }
        fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            T::from_i64(v).ok_or_else(|| {
                serde::de::Error::custom(format!(
                    "{} cannot be represented by the requested type",
                    v
                ))
            })
        }
    }
    deserializer.deserialize_str(Visitor(std::marker::PhantomData))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Port(pub u16);
impl<'de> Deserialize<'de> for Port {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        //TODO: if number, be permissive
        deserialize_number_permissive(deserializer).map(Port)
    }
}
impl Serialize for Port {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serialize_display(&self.0, serializer)
    }
}

#[derive(Debug, Clone)]
pub struct Reversible<T, Container = Vec<T>>
where
    for<'a> &'a Container: IntoDoubleEndedIterator<&'a T>,
{
    reversed: bool,
    data: Container,
    phantom: PhantomData<T>,
}
impl<T, Container> Reversible<T, Container>
where
    for<'a> &'a Container: IntoDoubleEndedIterator<&'a T>,
{
    pub fn new(data: Container) -> Self {
        Reversible {
            reversed: false,
            data,
            phantom: PhantomData,
        }
    }

    pub fn reverse(&mut self) {
        self.reversed = !self.reversed
    }

    pub fn iter(
        &self,
    ) -> itertools::Either<
        <&Container as IntoDoubleEndedIterator<&T>>::IntoIter,
        std::iter::Rev<<&Container as IntoDoubleEndedIterator<&T>>::IntoIter>,
    > {
        let iter = IntoDoubleEndedIterator::into_iter(&self.data);
        if self.reversed {
            itertools::Either::Right(iter.rev())
        } else {
            itertools::Either::Left(iter)
        }
    }
}
impl<T, Container> Serialize for Reversible<T, Container>
where
    for<'a> &'a Container: IntoDoubleEndedIterator<&'a T>,
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let iter = IntoDoubleEndedIterator::into_iter(&self.data);
        let mut seq_ser = serializer.serialize_seq(match iter.size_hint() {
            (lower, Some(upper)) if lower == upper => Some(upper),
            _ => None,
        })?;
        if self.reversed {
            for elem in iter.rev() {
                seq_ser.serialize_element(elem)?;
            }
        } else {
            for elem in iter {
                seq_ser.serialize_element(elem)?;
            }
        }
        seq_ser.end()
    }
}
impl<'de, T, Container> Deserialize<'de> for Reversible<T, Container>
where
    for<'a> &'a Container: IntoDoubleEndedIterator<&'a T>,
    Container: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(Reversible::new(Deserialize::deserialize(deserializer)?))
    }
    fn deserialize_in_place<D>(deserializer: D, place: &mut Self) -> Result<(), D::Error>
    where
        D: Deserializer<'de>,
    {
        Deserialize::deserialize_in_place(deserializer, &mut place.data)
    }
}

pub struct KeyVal<K, V> {
    pub key: K,
    pub value: V,
}
impl<K: Serialize, V: Serialize> Serialize for KeyVal<K, V> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry(&self.key, &self.value)?;
        map.end()
    }
}
impl<'de, K: Deserialize<'de>, V: Deserialize<'de>> Deserialize<'de> for KeyVal<K, V> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor<K, V>(PhantomData<(K, V)>);
        impl<'de, K: Deserialize<'de>, V: Deserialize<'de>> serde::de::Visitor<'de> for Visitor<K, V> {
            type Value = KeyVal<K, V>;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "A map with a single element")
            }
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let (key, value) = map
                    .next_entry()?
                    .ok_or_else(|| serde::de::Error::invalid_length(0, &"1"))?;
                Ok(KeyVal { key, value })
            }
        }
        deserializer.deserialize_map(Visitor(PhantomData))
    }
}
